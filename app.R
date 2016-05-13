mod1 <- function(input, output, session) {
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
  library(RPostgreSQL)
  drv <- PostgreSQL()
  conn <- dbConnect(drv, 
                    dbname  = "shiny_db",
                    host = "localhost",
                    port = 5432,
                    user = "postgres",
                    password = password)
  
 mod_test <- callModule(mod1, "my_mod")
 
 # observeEvent()
 
  output$my_mean <- 
    renderPrint(mod_test())
}

ui <- fluidPage(
  mod1UI("my_mod"),
  verbatimTextOutput("my_mean")
)

shinyApp(ui = ui, server = server)

TABLE_NAME <- 
  "data_cap"

query <-
 sprintf("INSERT INTO %s (%s) VALUES %s", 
         TABLE_NAME, paste(names(mtcars), 
                           collapse = ", "),
         gsub(paste(mtcars, collapse = ", "),
              pattern = "c\\(", replacement = "("
         )
         )
dbGetQuery(conn, query)

dbWriteTable(conn, TABLE_NAME,
             mtcars, append = TRUE, row.names = FALSE)

query_tmp <- "INSERT INTO data_cap (mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb) VALUES (21, 22, 3.9, 2.62, 16.46, 0, 1, 4, 4, 3, 3), (21, 22, 3.9, 2.62, 16.46, 0, 1, 4, 4, 3, 3)"
