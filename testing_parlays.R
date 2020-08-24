library(shiny)

# Going to test a stripped down version of the app (only parlays! ) here to make the variable number of lines work. 



# defining formulas

impliedodds <- function(line) {
  if (line < 0) { odds <- abs(line) / (100 + abs(line))
  }
  else { odds <- 100 / (100 + line)
  }
  return(odds)
}

impliedprob <- function(prob){
  if(prob >= 0.5){line <- (100 * prob)/(prob-1)}
  else{line <- (100*(1-prob)/prob)}
  return(line)
}

winning <- function(wager, line) {
  if(line >= 0){payout <- wager * line / 100}
  else{payout <- wager * 100 / abs(line)}
  return(payout)
}



#Now the actual Shiny App

ui <- fluidPage(

    tabPanel(tags$strong("parlays"),
             tags$h3("parlay odds calculator"),
             
             fluidRow(
               column (
                 numericInput("parlaybet",tags$h3("Bet Amount"), 100),
                 tags$br(),
                 numericInput("parlay1", "Line #1", 0),
                 numericInput("parlay2", "Line #2", 0),
                 numericInput("parlay3", "Line #3", 0),

                 actionLink("parlaycalc", "Calculate my payout"),
                 width = 6),
               column(
                 tags$h3("Parlay Payout"),
                 tags$br(),
                 tags$h5("This parlay should pay out at:"),
                 tags$h3(textOutput("text4")),
                 tags$br(),
                 tags$h5("Given your bet amount, this equates to:"),
                 tags$h3(textOutput("text5")),
                 width = 6)
             )))


server <- function(input, output) {

  
  ### PARLAY FUNCTION HERE
  
  parlay_1 <- eventReactive(
    eventExpr = input$parlaycalc,
    valueExpr = {input$parlay1})
  
  parlay_2 <- eventReactive(
    eventExpr = input$parlaycalc,
    valueExpr = {input$parlay2})
  
  parlay_3 <- eventReactive(
    eventExpr = input$parlaycalc,
    valueExpr = {input$parlay3})

  
  parlay_bet <- eventReactive(
    eventExpr = input$parlaycalc,
    valueExpr = {input$parlaybet})
  
  output$text4 <- renderText({
    (round(impliedprob(prod( impliedodds(parlay_1()), 
                             impliedodds(parlay_2()), 
                             impliedodds(parlay_3()), 

                             na.rm = TRUE)),2))})
  
  
  output$text5 <- renderText({
    (round(winning(parlay_bet(), impliedprob(prod(impliedodds(parlay_1()), 
                                                  impliedodds(parlay_2()), 
                                                  impliedodds(parlay_3()), 

                                                  na.rm = TRUE))),2))})
  

  
}





# Run the application
shinyApp(ui = ui, server = server)

