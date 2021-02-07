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
})


#kdpi
