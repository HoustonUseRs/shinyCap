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