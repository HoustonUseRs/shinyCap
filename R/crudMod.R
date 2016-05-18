library(shiny)
library(shinyjs)
library(RPostgreSQL)
library(dplyr)

dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}

GetNextId <- function() {
  if (exists("responses") && nrow(responses) > 0) {
    # max(as.integer(rownames(responses))) + 1
    dplyr_conn %>% 
      tbl(
        sql("SELECT max(id) FROM default_tbl")
      ) %>% collect() %>% as.numeric() + 1
  } else {
    return (1)
  }
}

GetTableMetadata <- function() {
  
  fields <-
    Map(x =  dbListFields(conn, "default_tbl"),
        y = dbListFields(conn, "default_tbl"),
        function(x, y) {
          assign(x, y)
        }) %>% unlist()
  
  result <- list(fields = fields)
  return (result)
}

ReadData <- function() {
  if (exists("responses")) {
    responses
  }
}

UpdateDataQuery <- function(data) {
  data <- CastData(data)
  df <- select(data, -id)
  df_pk <- data[, "id"]
  dbSendQuery(conn, 
              sprintf("UPDATE %s SET (%s) = ('%s') WHERE %s;", "default_tbl",
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

shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    uiOutput("group_inputs"),
    actionButton("submit", "Submit"),
    actionButton("new", "New"),
    actionButton("delete", "Delete"),
    DT::dataTableOutput("responses"),
    DT::dataTableOutput("my_responses"),
    verbatimTextOutput("row_select")
    
  ),
  server = function(input, output, session) {
    
    observeEvent(input$submit, {
      if (input$id != "0") {
        UpdateDataQuery(formData())
      } else {
        # CreateData(formData())
        # UpdateInputs(CreateDefaultRecord(), session)
      }
    }, priority = 1)
    
    
    ui_ls <-
      Map(x = GetTableMetadata()$fields, 
          y = names(GetTableMetadata()$fields),
          function(x, y) {
            textInput(x, y)
          })
    # 
    # output$id_input <-
    #   renderUI({
    #     shinyjs::disabled(
    #     ui_ls$id
    #     )
    #   })
    # 
    output$group_inputs <-
      renderUI({
        return(
          tagList(
            disabled(ui_ls$id),
            ui_ls[names(ui_ls) != "id"]
          )
        )
      })
    
    responses <- 
      tbl(dplyr_conn, "default_tbl") %>%
      collect()
    
    formData <- reactive({
      values <- lapply(names(GetTableMetadata()$fields), function(x) input[[x]])
      names(values) <- names(GetTableMetadata()$fields)
      return(values)
    })
    # output$table_view <- 
    #   renderDataTable({
    #     tbl(dplyr_conn, "default_tbl") %>%
    #       collect()
    #   })
    # 
    # display table
    output$responses <- DT::renderDataTable({
      #update after submit is clicked
      # input$submit
      #update after delete is clicked
      # input$delete
      # ReadData()
      # df <- tbl(dplyr_conn, "default_tbl") %>%
      #   collect()
      responses
      # row.names(df) <- df$id
      # return(df)
    }, server = FALSE, selection = "single"#,
    #colnames = unname(GetTableMetadata()$fields)[-1]
    )
    
    
    selected_responses <-
      eventReactive(input$responses_rows_selected, {
        
        my_row <- input$responses_rows_selected
        # df <- tbl(dplyr_conn, "default_tbl") %>%
        #   filter(id == my_row) %>%
        #   collect()
        #   return(df)
        data <- responses[input$responses_rows_selected, ]
        # ReadData()
        # output$responses
        return(data)
        
      })
    
    output$my_responses <-
      DT::renderDataTable({
        selected_responses()
        # responses
      })
    
    # reactive_data <-
    #   reactive({
    #     responses <<- CastData()
    #   })
    # 
    
    observeEvent(input$responses_rows_selected, {
      if (length(input$responses_rows_selected) > 0) {
        data <- responses[input$responses_rows_selected, ]
        UpdateInputs(data, session)
      }
      
    })
    
    
    
    output$row_select <-
      # renderPrint(input$responses_rows_selected)
      # renderPrint(formData())
      renderPrint({
        UpdateDataQuery(
          # data.frame(
          formData()
          # , stringsAsFactors = FALSE)
        )
      })
    
    
  }
  
)