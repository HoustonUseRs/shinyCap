


#' shinyCap
#' @import shiny dplyr RPostgreSQL sqldf
#' @export
shinyCap <- function() {

  #require(shiny)
  #require(dplyr)
  #require(RPostgreSQL)
  #require(sqldf)
  #require(shinyjs)
  #require(DT)
  shinyApp(
    ui =
      navbarPage("shinyCap",
                 shinyjs::useShinyjs(),
                 tabPanel("Database Level",
                          navlistPanel("Table CRUD Operations",
                                       tabPanel("Define Database Connection",
                                                textInput("user_id",
                                                          "Enter a Valid Username",
                                                          value = "postgres"),
                                                passwordInput("password_id",
                                                              "Enter posgres user password"),
                                                p("Password will display for testing"),
                                                textInput("dbname_id",
                                                          "Enter/Update Database Name",
                                                          value = "postgres"),
                                                textInput("ip_id",
                                                          "Enter Server IP address",
                                                          value = "localhost"),
                                                numericInput("port_id",
                                                             "Enter Port #",
                                                             value = 5432),
                                                actionButton("conn_button", "Connect"),
                                                textOutput("passwd_print")
                                       ),
                                       tabPanel("Create New Database",
                                                textInput("create_db",
                                                          "Define database Name"
                                                ),
                                                numericInput("conn_limit",
                                                             "Max Connection Limit",
                                                             value = 10),
                                                actionButton("db_submit",
                                                             "Create Database")
                                       )
                          )),
                 tabPanel("Table Level",
                          navlistPanel("Table CRUD Operations",
                                       tabPanel("Create New Table",
                                                textInput("create_tb",
                                                          "Create Table"),
                                                textInput("pk_id",
                                                          "Name of Primary Key Column"),
                                                p("Primary Key defaults to auto-increment serial datatype, uses user input as table owner"),
                                                actionButton("create_tb_button", "Create Table")
                                       ),
                                       tabPanel("Create Table from File",
                                                textInput("create_tb2",
                                                          "Specify New Table Name"),
                                                fileInput('upload_tbl', 'Choose CSV File',
                                                          accept=c('text/csv',
                                                                   'text/comma-separated-values,text/plain',
                                                                   '.csv')),
                                                dataTableOutput("read_file_view")
                                       ),
                                       tabPanel("Select Table",
                                                p("Requires Valid Database Connection to be submitted"),
                                                uiOutput("tbl_select")
                                       ),
                                       tabPanel("Table Fields",
                                                p("Requires Valid Table to be Selected"),
                                                textInput("new_col_id", "New Column Name"),
                                                selectizeInput("col_type", "Column Data Type",
                                                               choices  = c("text", "double precision", "boolean", "date")),
                                                actionButton("new_col_button", "New Column/Field")
                                       )
                          )
                 ),
                 tabPanel("Row Level",
                          fluidPage(
                            # exampleModUI("my_mod"),
                            # verbatimTextOutput("my_mean")
                            uiOutput("group_inputs"),
                            actionButton("submit", "Submit"),
                            actionButton("new", "New"),
                            actionButton("delete", "Delete"),
                            DT::dataTableOutput("responses"),
                            DT::dataTableOutput("my_responses"),
                            verbatimTextOutput("row_select")
                          )
                 )
      ),
    server = function(input, output, session) {
      
      tblSchema <- function(tbl_name, conn) {
        dbSendQuery(conn,
                    sprintf("SELECT column_name, data_type
                            FROM   information_schema.columns
                            WHERE  table_name = '%s'
                            ORDER  BY ordinal_position;", tbl_name)
                    ) %>% fetch()
      }
      
      rendershinyCapUI <- 
        function(x, y) {
          if(y == "integer") {
            shiny_input <-  numericInput(x, x, value = 0)
          } else if(y == "text") {
            shiny_input <-  textInput(x, x)
          } else if(y == "date") {
            shiny_input <-  dateInput(x, x, value = #Sys.time()
                                        "")
          }
          return(shiny_input)
        }
      
      defaultValue <- function(x, y) {
        if(y == "integer") {
          value <-  0
        } else if(y == "text") {
          value <-  ""
        } else if(y == "date") {
          value <- Sys.time()
        }
        return(value)
      }
      
      CreateDefaultRecord <- function(tbl_name, conn) {
        tbl_col_types <- tblSchema(tbl_name, conn)
        mydefault <- 
          # CastData(list(id = 0#GetNextId()
          #        , employee_name = "",
          #        date_created = ""))
          Map(tbl_col_types$column_name, tbl_col_types$data_type, f = defaultValue) %>%
          CastData()
        return (mydefault)
      }
      
      
      
      GetNextId <- function( tbl_name, conn) {
        conn %>% 
          tbl(
            sql(paste("SELECT max(id) FROM", tbl_name,sep = " "))
          ) %>% collect() %>% as.numeric() + 1
      }
      
      GetTableMetadata <- function(tbl_name, conn) {
        
        fields <-
          Map(x =  dbListFields(conn, tbl_name),
              y = dbListFields(conn, tbl_name),
              function(x, y) {
                assign(x, y)
              }) %>% unlist()
        
        result <- list(fields = fields)
        return (result)
      }
      
      CreateData <- function(data, tbl_name, conn, conn2) {
        df <- CastData(data)
        df$id <- GetNextId(tbl_name, conn)
        tbl_col_types <- tblSchema(tbl_name, conn2)
        
        date_cols <-
          filter(tbl_col_types, 
                 data_type == "date")
        
        df[names(df) %in% date_cols$column_name] <- 
          apply(df[names(df) %in% date_cols$column_name], 
                MARGIN =2,
                FUN = as.character)
        
        # df$date_created <- as.character(
        #   Sys.time()
        # )
        
        dbSendQuery(conn2, 
                    sprintf("INSERT INTO %s (%s) VALUES ('%s')", tbl_name,
                            paste(df %>% names(), collapse = ", "),
                            paste(unname(df), collapse = "', '")
                    ))
      }
      
      UpdateDataQuery <- function(data, tbl_name, conn) {
        data <- CastData(data)
        df <- select(data, -id)
        df_pk <- data[, "id"]
        tbl_col_types <- tblSchema(tbl_name, conn)
        
        date_cols <-
          filter(tbl_col_types, 
                 data_type == "date")
        
        df[names(df) %in% date_cols$column_name] <- 
          apply(df[names(df) %in% date_cols$column_name], 
                MARGIN =2,
                FUN = as.character)
        
        dbSendQuery(conn, 
                    sprintf("UPDATE %s SET (%s) = ('%s') WHERE %s;", tbl_name,
                            paste(df %>% names(), collapse = ", "),
                            paste(unname(df), collapse = "', '"),
                            paste("id", df_pk, sep = " = ")
                    )
        )
      }
      

      CastData <- function(data) {
        data.frame(data, stringsAsFactors = FALSE)
      }
      

      UpdateInputs <- function(data, session) {
        Map(x = data, y = names(data), function(x, y) {
          updateTextInput(session, y, value = as.character(unname(x)))
        })
      }

      ### Example insert statement with proper syntax
      # query_tmp <-
      #"INSERT INTO data_cap (mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb) VALUES (21, 22, 3.9, 2.62, 16.46, 0, 1, 4, 4, 3, 3), (21, 22, 3.9, 2.62, 16.46, 0, 1, 4, 4, 3, 3)"

      drv <- PostgreSQL()
      # update_db_conn
      observeEvent(input$conn_button, {
        options(sqldf.RPostgreSQL.user = input$user_id,
                sqldf.RPostgreSQL.password = input$password_id,
                sqldf.RPostgreSQL.dbname =input$dbname_id,
                sqldf.RPostgreSQL.host =input$ip_id,
                sqldf.RPostgreSQL.port =input$port_id)
      })

      output$passwd_print <-
        renderText(input$password)

      observeEvent(input$db_submit, {
        sqldf(sprintf("CREATE DATABASE %s
        WITH OWNER = postgres
        ENCODING = 'UTF8'
        TABLESPACE = pg_default
        CONNECTION LIMIT = %s;", input$create_db, input$conn_limit))
      })

      observeEvent(input$create_tb_button, {
        sqldf(
          sprintf(
            "CREATE TABLE %s
      (
      %s serial NOT NULL,
      CONSTRAINT tbl_pk PRIMARY KEY (%s)
      )
      WITH (
      OIDS=FALSE
      );
      ALTER TABLE %s
      OWNER TO %s;", input$create_tb,
            input$pk_id, input$pk_id,
            input$create_tb, input$user_id)
        )
      })

      conn_fun <- reactive({
        drv <- PostgreSQL()
        conn <- dbConnect(drv,
                          dbname  = input$dbname_id,
                          host = input$ip_id,
                          port = input$port_id,
                          user = input$user_id,
                          password = input$password_id)
        return(conn)
      })

      dplyr_conn <- reactive({
        src_postgres(dbname = input$dbname_id,
                     host = input$ip_id,
                     port = input$port_id,
                     user = input$user_id,
                     password = input$password_id)
      })

      observeEvent(input$new_col_button, {
        dbSendQuery(conn_fun(),sprintf(
          "ALTER TABLE %s
          ADD COLUMN %s %s ;",
          input$tbl_select,
          input$new_col_id,
          input$col_type)
        )
      })

      # ALTER TABLE table1 ADD COLUMN test_cols1 varchar(10) NOT NULL DEFAULT 'foo';
      read_file <-
        reactive({
          inFile <- input$upload_tbl

          if (is.null(inFile))
            return(NULL)

          read.csv(inFile$datapath, stringsAsFactors = FALSE)
        })

      output$read_file_view <-
        renderDataTable(
          read_file()
        )
      
      observeEvent(input$upload_tbl, {
        inFile <- input$upload_tbl
        if (is.null(inFile))
          return(NULL)
        dbWriteTable(conn_fun(),
                     input$create_tb2, read_file(), row.names = FALSE,
                     append = TRUE)
      })
      # output$ls_elements <-
      #   renderUI({
      #   switch(type, "numeric/double" = "numericInput()",
      #          varchar = "textInput()") #%>% parse(text = .)
      #      #   %>% eval()
      #   })


      output$tbl_select <-
        renderUI({
          selectizeInput("tbl_select_inp",
                         "Select a table from Defined DB Conn",
                         choices = src_tbls(dplyr_conn()))
        })
      # mod_test <- callModule(exampleMod, "my_mod",conn_fun)

      # observeEvent()

      # output$my_mean <-
      #   renderPrint(mod_test())
      
      # tbl_name <- reactive({
      #   input$tbl_select_inp
      # })
      ###
      
      #### Row Level
      tbl_name <- "it_tickets"
      
      
      ui_ls <- reactive({
        tbl_col_types <- tblSchema(tbl_name,conn = conn_fun())
        Map(tbl_col_types$column_name, tbl_col_types$data_type,f = rendershinyCapUI)
      })
      # 
      output$group_inputs <-
        renderUI({
          return(
            tagList(
              disabled(ui_ls()$id),
              ui_ls()[names(ui_ls()) != "id"]
            )
          )
        })
      # 
      observeEvent(input$submit, {
        if (input$id != 0) {
          UpdateDataQuery(formData(),tbl_name, conn = conn_fun())
        } else {
          CreateData(formData(), tbl_name, conn = dplyr_conn(), conn2 = conn_fun())
          UpdateInputs(CreateDefaultRecord(tbl_name, conn = conn_fun()), session)
        }
      }, priority = 1)
      # 
      # 
      responses <- reactive({
        input$submit
        tbl(dplyr_conn(), input$tbl_select_inp) %>%
          collect()
      })
      # 
      formData <- reactive({
        values <- lapply(names(GetTableMetadata(tbl_name, conn_fun())$fields), function(x) input[[x]])
        names(values) <- names(GetTableMetadata(tbl_name, conn_fun())$fields)
        return(values)
      })

      output$responses<- DT::renderDataTable({
        responses()
      }, server = FALSE, selection = "single"
      )
      # 
      # 
      selected_responses <-
        eventReactive(input$responses_rows_selected, {

          my_row <- input$responses_rows_selected
          data <- responses()[input$responses_rows_selected, ]
          return(data)

        })
      # 
      output$my_responses <-
        DT::renderDataTable({
          selected_responses()
        })
      # 
      observeEvent(input$new,{
        data <-
          # isolate(
          CreateDefaultRecord(tbl_name, conn = conn_fun())
          # )
        UpdateInputs(data, session)
      })
      # 
      observeEvent(input$responses_rows_selected, {
        if (length(input$responses_rows_selected) > 0) {
          data <- responses()[input$responses_rows_selected, ]
          UpdateInputs(data, session)
        }

      })

    }
    


  )
}


