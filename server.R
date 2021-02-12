source("scripts/fxs.R")

library(shinydashboard)

shinyServer(function(input, output) {
  
  
  eptsx<-reactive({
    epts(age = input$age,
         diabetes = input$diabetes,
         priortx = input$priortx,
         tdialysis = input$tdialysis)$score_EPTS
  })
  

  output$eptsBox <- renderValueBox({
    valueBox(
      eptsx(), "EPTS score", icon = icon("percent"),
      color = ifelse(eptsx() > 20, "orange", "green")
    )
  })
  
  kdpix<-reactive({
    kdpi(age = input$ageD
         , raceAA = input$raceAA
         , hypertension = input$hipertension
         , diabetes = input$diabetesD
         , creatinine = input$creatinine
         , stroke = input$stroke
         , height = input$height
         , weight = input$weight
         , dcd = input$dcd
         , hcv = input$hcv
         , mmB = input$mmB
         , mmDR = input$mmDR
         , cold = input$cold
         , enbloc = input$enbloc
         , double = input$double
    )$score_KDPI
  })
  
  output$kdpiBox <- renderValueBox({
    valueBox(
      kdpix(), 
      "KDPI percentage", icon = icon("percent"),
      color = ifelse(kdpix() > 20, "orange", "green")
    )
  })
  
  txscorex<-reactive({
    txscore(ageR = input$ageR
            , race = input$race
            #, insurance = 0
            , causeESRD = input$ESRD
            , timeD = input$timeD
            , diabetesR = input$diabetesR
            , coronary = input$coronary
            , albumin = input$albumin
            , hemoglobin = input$hemoglobin
            , ageD = input$ageDD
            , diabetesD= input$diabetesDD
            , ECD = input$ECD
            , mmHLA = input$mmHLA
            )$prob5y
  })
  
  output$txscoreBox <- renderValueBox({
    valueBox(
      txscorex()
      , "5-year event probability", icon = icon("percent")
      , color = ifelse(txscorex() > 50, "orange", "green")
    )
  })
  
  
  # teste <- reactive(input$ECD)
  # output$testex <- renderText(teste())
  
})


#kdpi
