exampleModUI <- function(id) {
  ns <- NS(id)
  tagList(
    sliderInput(ns("obs"), "Number of observations:", min = 10, max = 500, value = 100),
    textInput(ns("text_inp"), "My Varchar"),
    dateInput(ns("date_inp"), "Select Date"),
    actionButton(ns("submit"), "Send!"),
    plotOutput(ns("distPlot"))
  )
}
