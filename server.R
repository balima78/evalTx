source("scripts/fxs.R")
source("scripts/data4correlations.R")

library(shinydashboard)

library(plotly)

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
      color = ifelse(eptsx() < 20, "green", 
                     ifelse(eptsx() < 40,"orange", "red"))
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
      color = ifelse(kdpix() < 20, "green", 
                     ifelse(kdpix() < 40,"orange", "red"))
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
      , color = ifelse(txscorex() > 60, "orange", "green")
    )
  })
  
  
  output$plot.epts <- renderPlot({
    cor.epts <- round(
      with(txs.uk,
           cor.test(txscore, epts, data=txs.score)
      )[[4]],2) 
    
    p1 <- ggplot(txs.uk, aes(txscore, epts)) + 
      geom_point() + 
      geom_smooth() + 
      geom_text(aes(x = 55, y = 110), 
                label = paste("rho =", cor.epts,"(p.value < 0.001)"), 
                parse = F) +
      ylab("EPTS") + 
      xlab("TRANSPLANTSCORE") +
      theme_bw()
    
    ggExtra::ggMarginal(p1, type = "histogram", binwidth = 5)

  })
  
  output$plot.kdpi <- renderPlot({
    cor.kdpi <- round(
      with(txs.uk,
           cor.test(txscore, kdpi, data=txs.score)
      )[[4]],2) 
    
    p2 <- ggplot(txs.uk, aes(txscore, kdpi)) + 
      geom_point() + 
      geom_smooth() + 
      geom_text(aes(x = 55, y = 90), 
                label = paste("rho =", cor.kdpi,"(p.value < 0.001)"), 
                parse = F) +
      ylab("KDPI") + 
      xlab("TRANSPLANTSCORE") +
      theme_bw()
    
    ggExtra::ggMarginal(p2, type = "histogram", binwidth = 5) 
    
  })
  
  output$plot.roc <- renderPlot({
    
    plot.roc(txs.uk$USgood, txs.uk$txscore, print.auc = T, ci = T)
    
  })
  
  output$plot.sens.spec <- renderPlotly({
    spg<-ggplot(dataroc) + 
      geom_line(aes(cutoff, sensitivity, color = "sens")) +
      geom_line(aes(cutoff, specificity, color = "spec")) + 
      scale_color_manual(values = c("red","blue")) + 
      labs(color = "", x = "TRANSPLANTSCORE", y="Sens. & Spec. values"
           ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme_bw()  
    ggplotly(spg)
    
  })
  
  output$plot.density <- renderPlot({
    
    ggplot(data.frame(rr$predictor,rr$response),
           aes(rr.predictor, fill = factor(rr.response))) + geom_density(alpha = 0.2) + 
      labs(fill = "prognostic", x = "TRANSPLANTSCORE", 
           caption = "EPTS / KDPI prognostic: 1 - good outcome; 2 - bad outcome") + 
      theme_bw()
  })
  
  output$door <- renderImage({
    return(list(src = "www/openDoor1.jpg"
                ,contentType = "image/jpg"
                ,alt = "door"))
    }, deleteFile = FALSE)

  
})

