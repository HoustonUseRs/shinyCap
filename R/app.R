library(dplyr)
library(RPostgreSQL)
library(sqldf)
library(shiny)
drv <- PostgreSQL()


mod1 <- function(input, output, session,conn_fun) {
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

mod1UI <- function(id) {
  ns <- NS(id)
  tagList(
    sliderInput(ns("obs"), "Number of observations:", min = 10, max = 500, value = 100),
    textInput(ns("text_inp"), "My Varchar"),
    dateInput(ns("date_inp"), "Select Date"),
    actionButton(ns("submit"), "Send!"),
    plotOutput(ns("distPlot"))
  )
}

server <- function(input, output) {
  
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
  
output$tbl_select <-
  renderUI({
    selectizeInput("tbl_select", 
                   "Select a table from Defined DB Conn",
                   choices = src_tbls(dplyr_conn()))
  })
 mod_test <- callModule(mod1, "my_mod",conn_fun)
 
 # observeEvent()
 
  output$my_mean <- 
    renderPrint(mod_test())
}

ui <- 
  navbarPage("shinyCap",
    tabPanel("Default Conn",
       sidebarLayout(
         sidebarPanel(
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
         actionButton("conn_button", "Connect")
         ),
         mainPanel(
         textOutput("passwd_print")
         )
       )),
    tabPanel("Create DB",
      fluidPage(
            textInput("create_db",
                      "Define database Name"
            ),
            numericInput("conn_limit",
                         "Max Connection Limit",
                         value = 10),
            actionButton("db_submit",
                         "Create Database")
      )
    ),
    tabPanel("Create Table",
      fluidRow(
        textInput("create_tb",
                  "Create Table"),
        textInput("pk_id",
                  "Name of Primary Key Column"),
        p("Primary Key defaults to auto-increment serial datatype, uses user input as table owner"),
        actionButton("create_tb_button", "Create Table"),
        uiOutput("tbl_select")
      )
    ),
    tabPanel("Sample Insert",
     fluidPage(
       mod1UI("my_mod"),
       verbatimTextOutput("my_mean")
     )
    )
)

shinyApp(ui = ui, server = server)



# 
# TABLE_NAME <- 
#   "data_cap"

# query <-
#  sprintf("INSERT INTO %s (%s) VALUES %s", 
#          TABLE_NAME, paste(names(mtcars), 
#                            collapse = ", "),
#          gsub(paste(mtcars, collapse = ", "),
#               pattern = "c\\(", replacement = "("
#          )
#          )
# dbGetQuery(conn, query)
# 
# dbWriteTable(conn, TABLE_NAME,
#              mtcars, append = TRUE, row.names = FALSE)
# 
# query_tmp <- "INSERT INTO data_cap (mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb) VALUES (21, 22, 3.9, 2.62, 16.46, 0, 1, 4, 4, 3, 3), (21, 22, 3.9, 2.62, 16.46, 0, 1, 4, 4, 3, 3)"
