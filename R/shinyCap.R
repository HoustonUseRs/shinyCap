#' exampleMod
#'
#' @param input
#' @param output
#' @param session
#' @param conn_fun
#' @export
exampleMod <- function(input, output, session,conn_fun) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs), col = 'darkgray', border = 'white')
  })

  react_return_fun <- eventReactive(input$submit, {
    df <- data.frame(rnorm = input$obs,
                     text_col = input$text_inp,
                     date_col = input$date_inp,
                     time = Sys.time())
    return(df)
  })

  observeEvent(input$submit, {
    dbWriteTable(conn_fun(), "TABLE_NAME",
                 react_return_fun(), append = TRUE,
                 row.names = FALSE)
  })
  return(
    react_return_fun
  )
}

#' exampleModUI
#'
#' @param id
#' @export
exampleModUI <- function(id) {
  ns <- NS(id)
  tagList(
    sliderInput(ns("obs"), "Number of observations:",
                min = 10, max = 500, value = 100),
    textInput(ns("text_inp"), "My Varchar"),
    dateInput(ns("date_inp"), "Select Date"),
    actionButton(ns("submit"), "Send!"),
    plotOutput(ns("distPlot"))
  )
}


#' shinyCap
#' @import shiny dplyr RPostgreSQL sqldf
#' @export
shinyCap <- function() {

  #require(shiny)
  #require(dplyr)
  #require(RPostgreSQL)
  #require(sqldf)
  shinyApp(
    ui =
      navbarPage("shinyCap",
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
                            exampleModUI("my_mod"),
                            verbatimTextOutput("my_mean")
                          )
                 )
      ),
    server = function(input, output) {

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

      dplyr_conn <- eventReactive(input$conn_button, {
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
          selectizeInput("tbl_select",
                         "Select a table from Defined DB Conn",
                         choices = src_tbls(dplyr_conn()))
        })
      mod_test <- callModule(exampleMod, "my_mod",conn_fun)

      # observeEvent()

      output$my_mean <-
        renderPrint(mod_test())
    }


  )
}
