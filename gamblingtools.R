library(shiny)
library(shinythemes)

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

kelly <- function(bet_prob, real_prob) {
  fraction <- (((1 / bet_prob - 1) * real_prob - (1 - real_prob)) / (1 / bet_prob - 1))
  return(fraction)
}

winning <- function(wager, line) {
  if(line >= 0){payout <- wager * line / 100}
  else{payout <- wager * 100 / abs(line)}
  return(payout)
}


### Hedge function
hedge1 <- function(wager, line_init, line_new){
  init_win <- winning(wager, line_init)
  new_win <- winning(1, line_new)
  hedge_amount  <- (wager + init_win) / (1 + new_win)
  return(hedge_amount)
}

hedge2 <- function(wager, line_init, line_new){
  init_win <- winning(wager, line_init)
  new_win <- winning(1, line_new)
  hedge_amount <- wager / new_win
  return(hedge_amount)
}

#Now the actual Shiny App
  
ui <- fluidPage(
  theme = shinytheme("yeti"),

  tags$h1("tools for sports betting"),
  
  
### PAGE 1 - BET SIZING 

  tabsetPanel(
    tabPanel(tags$strong("size"),
       tags$h3("optimal bet sizing"),
        tags$p("The Kelly criterion is a formula used in bet sizing intended to maximize long-term expected returns, given your expectations on 
               probability of winning a given wager. Given the inputs below, this calculator will estimate your ideal wager size using the Kelly formula."
        ),
       tags$br(),
  
        fluidRow(
          column(
            numericInput("bankroll","What is your total bankroll for betting?", 1000, 0, 1000000
           ),
           width = 4
           
          ),
          column(
           numericInput("kellyline", "What is the line you are betting?",-110),
           width = 4
         ),
         column(
           numericInput("kellyprob", "What chance to win do you expect (0-100)?", 50, 0, 100),
           width = 4
         )),
       fluidRow(
         column(
           actionLink(inputId = "kellycalc", label = "Calculate my bet"),
                width = 12)),
       tags$br(),
  
        fluidRow(
          column (
            textOutput("text1"),
           tags$br(),
           textOutput("text2"),
            textOutput("text3"),
            tags$br(),
            tags$em(tags$a(href = "https://en.wikipedia.org/wiki/Kelly_criterion", "More information on the Kelly criterion here.")),
            width = 12
          ))),
  
### PAGE 2 - PARLAY CALCULATOR

  tabPanel(tags$strong("parlays"),
          tags$h3("parlay odds calculator"),
          
          fluidRow(
            column (
            numericInput("parlaybet",tags$h3("Bet Amount"), 100),
            tags$br(),
            numericInput("parlay1", "Line #1", 0),
            numericInput("parlay2", "Line #2", 0),
            numericInput("parlay3", "Line #3", 0),
            numericInput("parlay4", "Line #4", 0),
            numericInput("parlay5", "Line #5", 0),
            numericInput("parlay6", "Line #6", 0),
            numericInput("parlay7", "Line #7", 0),
            numericInput("parlay8", "Line #8", 0),
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
  )),

### PAGE 3 - HEDGE CALCULATOR
tabPanel(tags$strong("hedging"),
         tags$h3("hedge calculator"),
         fluidRow(
           column(
             selectInput("hedgetype", "type of hedge",c("complete hedge", "zero downside hedge")),
             numericInput("hedgewager", "initial bet", 0),
             numericInput("hedgeline", "initial line", 0),
             numericInput("hedgeline2", "line to hedge with", 0),
             actionLink("hedgecalc", "Calculate my hedge"),
             width = 6),
           column(
             tags$h3("How to hedge"),
             tags$br(),
             tags$h5(textOutput("text6")),
             tags$h5(textOutput("text7")),
             width = 6)        
           ))))

server <- function(input, output) {
  
### KELLY CRITERION STUFF HERE
  
  kelly_prob <- eventReactive(
                    eventExpr = input$kellycalc,
                    valueExpr = {input$kellyprob})
                      
  kelly_line <- eventReactive(
                    eventExpr = input$kellycalc,
                    valueExpr = {input$kellyline})                    
                      
  kelly_bankroll <- eventReactive(
                    eventExpr = input$kellycalc,
                    valueExpr = {input$bankroll})
  
  output$text1 <- renderText({
                      paste(
                        "Given that, you're expecting to win",
                        100 * round(kelly_prob() / 100, 4),
                        "percent of the time, but are getting paid as if you will win ",
                        100 * round(impliedodds(kelly_line()), 4),
                        "percent of the time."
                      )})
  
  output$text2 <- renderText({
                      if (kelly_prob() / 100 > impliedodds(kelly_line())) {
                      paste(
                        "The Kelly criterion would recommend a max bet of",
                        round((kelly_bankroll() * kelly(impliedodds(kelly_line()), kelly_prob() / 100)), 2),
                        " (or",
                        100 * round(kelly(impliedodds(kelly_line()), kelly_prob() / 100), 4),
                        "percent of your bankroll)."
                      )}})
  
  output$text3 <- renderText({
                      if (kelly_prob() / 100 <= impliedodds(kelly_line())) {
                        paste("The Kelly criterion would not recommend a bet."
                      )}})
  
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
  
  parlay_4 <- eventReactive(
               eventExpr = input$parlaycalc,
               valueExpr = {input$parlay4})
  
  parlay_5 <- eventReactive(
                eventExpr = input$parlaycalc,
                valueExpr = {input$parlay5})
  
  parlay_6 <- eventReactive(
                eventExpr = input$parlaycalc,
                valueExpr = {input$parlay6})
  
  parlay_7 <- eventReactive(
                eventExpr = input$parlaycalc,
                valueExpr = {input$parlay7})
  
  parlay_8 <- eventReactive(
               eventExpr = input$parlaycalc,
               valueExpr = {input$parlay8})
  
  parlay_bet <- eventReactive(
                  eventExpr = input$parlaycalc,
                  valueExpr = {input$parlaybet})
 
output$text4 <- renderText({
           (round(impliedprob(prod( impliedodds(parlay_1()), 
                                    impliedodds(parlay_2()), 
                                    impliedodds(parlay_3()), 
                                    impliedodds(parlay_4()),
                                    impliedodds(parlay_5()),
                                    impliedodds(parlay_6()),
                                    impliedodds(parlay_7()),
                                    impliedodds(parlay_8()),
                                    na.rm = TRUE)),2))})


output$text5 <- renderText({
          (round(winning(parlay_bet(), impliedprob(prod(impliedodds(parlay_1()), 
                                                          impliedodds(parlay_2()), 
                                                          impliedodds(parlay_3()), 
                                                          impliedodds(parlay_4()),
                                                          impliedodds(parlay_5()),
                                                          impliedodds(parlay_6()),
                                                          impliedodds(parlay_7()),
                                                          impliedodds(parlay_8()),
                                                          na.rm = TRUE))),2))})

### HEDGE FUNCTION HERE

hedge_type <- eventReactive(
                eventExpr = input$hedgecalc,
                valueExpr = {input$hedgetype})

hedge_wager <- eventReactive(
               eventExpr = input$hedgecalc,
               valueExpr = {input$hedgewager})

hedge_line <- eventReactive(
               eventExpr = input$hedgecalc,
               valueExpr = {input$hedgeline})

hedge_line2 <- eventReactive(
                eventExpr = input$hedgecalc,
                valueExpr = {input$hedgeline2})

output$text6 <- renderText({
                    paste("Your initial wager was",
                          round(hedge_wager(),2),
                          "to win",
                          round(winning(hedge_wager(), hedge_line()),2),
                          "and you're able to hedge at a line of",
                          hedge_line2()
                          )})

output$text7 <- renderText({
                  if(hedge_type() == "complete hedge"){
                    if(winning(hedge_wager(),hedge_line()) - hedge1(hedge_wager(),hedge_line(),hedge_line2()) <= 0){
                      paste("Unfortunately you are unable to lock in a profit, given those lines.")                  
                      }else {paste("To completely hedge, you should wager",
                            round(hedge1(hedge_wager(),hedge_line(),hedge_line2()),2),
                            "which will lock in a profit of",
                            round(winning(hedge_wager(),hedge_line()) - hedge1(hedge_wager(),hedge_line(),hedge_line2()),2))}}
  
                  else{ if(winning(hedge_wager(),hedge_line()) - hedge2(hedge_wager(),hedge_line(),hedge_line2()) <= 0){
                          paste("Unfortunately you are unable to eliminate your downside, given those lines.")
                  }else{ paste("To eliminate your downside, you should wager",
                          round(hedge2(hedge_wager(), hedge_line(), hedge_line2()),2),
                          "leaving you with 0 if you lose or",
                          round(winning(hedge_wager(),hedge_line()) - hedge2(hedge_wager(),hedge_line(),hedge_line2()),2),
                          "if you win.")
      }}})

}

# Run the application
shinyApp(ui = ui, server = server)

