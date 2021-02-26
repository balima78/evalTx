source("scripts/fxs.R")
source("scripts/data4correlations.R")

library(shinydashboard)

library(plotly)


shinyServer(function(input, output, session) {
  
  
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
            #, mmHLA = input$mmHLA
            , mmHLA_A = input$mmHLA_A
            , mmHLA_B = input$mmHLA_B
            , mmHLA_DR = input$mmHLA_DR
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
           cor.test(txscore, epts
                    #, data=txs.score
                    )
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
           cor.test(txscore, kdpi
                    #, data=txs.score
                    )
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
      geom_vline(xintercept = youden$threshold, 
                 linetype="dotted", 
                 color = "gray" ) +
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
      geom_vline(xintercept = youden$threshold, 
                 linetype="dotted", 
                 color = "gray" ) +
      labs(fill = "prognostic", x = "TRANSPLANTSCORE", 
           caption = "EPTS / KDPI prognostic: 1 - good outcome; 2 - bad outcome") + 
      theme_bw()
  })
  
  output$door <- renderImage({
    return(list(src = "www/openDoor1.jpg"
                ,contentType = "image/jpg"
                ,alt = "door"))
    }, deleteFile = FALSE)
  
  output$table_Nyberg <- renderImage({
    return(list(src = "www/table_Nyberg75.jpg",
                contentType = "image/jpg",
                alt = "Alignment"))
  }, deleteFile = FALSE) 

  
  nybergscorex<-reactive({
    nybergscore(age = input$age_N
                , hyper = input$hyper_N
                , cretin = input$cretin_N
                , HLAmm_A = input$HLAmm_A
                , HLAmm_B = input$HLAmm_B
                , HLAmm_DR = input$HLAmm_DR
                #, HLAmm = input$HLAmm_N
                , CVA = input$CVA_N)
  })
  
  output$nybergscoreBox <- renderValueBox({
    valueBox(
      nybergscorex()
      , "Donor's score", icon = icon("calculator")
      , color = ifelse(nybergscorex() > 20, "orange", "green")
    )
  })
  
  output$plot.nyberg <- renderPlot({
    cor.nyberg <- round(
      with(txs.uk,
           cor.test(txscore, Nyberg_score)
      )[[4]],2) 
    
    p2 <- ggplot(txs.uk, aes(txscore, Nyberg_score)) + 
      geom_point() + 
      geom_smooth() + 
      geom_text(aes(x = 55, y = 30), 
                label = paste("rho =", cor.nyberg,"(p.value < 0.001)"), 
                parse = F) +
      ylab("Donors' score (Nyberg, et al) ") + 
      xlab("TRANSPLANTSCORE") +
      theme_bw()
    
    ggExtra::ggMarginal(p2, type = "histogram", binwidth = 5) 
    
  })

  
  ############
  ############ ligação entre inputs
  ############   
  observeEvent(input$age,{
    newage <- input$age
    updateSliderInput(session, "ageR", value = newage) 
  })
  
  observeEvent(input$ageR,{ 
    newage <- input$ageR
    updateSliderInput(session, "age", value = newage) 
  })
  
  observeEvent(input$tdialysis,{
    newdial <- input$tdialysis
    updateSliderInput(session, "timeD", value = newdial) 
  })
  
  observeEvent(input$timeD,{ 
    newdial <- input$timeD
    updateSliderInput(session, "tdialysis", value = newdial) 
  })
  
  observeEvent(input$diabetes,{
    newdiabetes <- input$diabetes
    updateCheckboxInput(session, "diabetesR", value = newdiabetes) 
  })
  
  observeEvent(input$diabetesR,{ 
    newdiabetes <- input$diabetesR
    updateCheckboxInput(session, "diabetes", value = newdiabetes) 
  })
  
  observeEvent(input$ageD,{
    newaged <- input$ageD
    updateSliderInput(session, "ageDD", value = newaged) 
  })
  
  observeEvent(input$ageDD,{ 
    newaged <- input$ageDD
    updateSliderInput(session, "ageD", value = newaged) 
  })
  
  observeEvent(input$diabetesD,{
    newdiad <- input$diabetesD
    updateRadioButtons(session, "diabetesDD", selected = newdiad)
  })

  observeEvent(input$diabetesDD,{
    newdiad <- input$diabetesDD
    updateRadioButtons(session, "diabetesD", selected = newdiad)
  })

  observeEvent(input$mmA,{
    newA <- input$mmA
    updateRadioButtons(session, "mmHLA_A", selected = newA)
  })
  
  observeEvent(input$mmHLA_A,{
    newA <- input$mmHLA_A
    updateRadioButtons(session, "mmA", selected = newA)
  })
  
  observeEvent(input$mmB,{
    newB <- input$mmB
    updateRadioButtons(session, "mmHLA_B", selected = newB)
  })
  
  observeEvent(input$mmHLA_B,{
    newB <- input$mmHLA_B
    updateRadioButtons(session, "mmB", selected = newB)
  })
  
  observeEvent(input$mmDR,{
    newDR <- input$mmDR
    updateRadioButtons(session, "mmHLA_DR", selected = newDR)
  })
  
  observeEvent(input$mmHLA_DR,{
    newDR <- input$mmHLA_DR
    updateRadioButtons(session, "mmDR", selected = newDR)
  })
  
  observeEvent(input$mmA,{
    newA <- input$mmA
    updateRadioButtons(session, "HLAmm_A", selected = newA)
  })
  
  observeEvent(input$HLAmm_A,{
    newA <- input$HLAmm_A
    updateRadioButtons(session, "mmA", selected = newA)
  })
  
  observeEvent(input$mmB,{
    newB <- input$mmB
    updateRadioButtons(session, "HLAmm_B", selected = newB)
  })
  
  observeEvent(input$HLAmm_B,{
    newB <- input$HLAmm_B
    updateRadioButtons(session, "mmB", selected = newB)
  })
  
  observeEvent(input$mmDR,{
    newDR <- input$mmDR
    updateRadioButtons(session, "HLAmm_DR", selected = newDR)
  })
  
  observeEvent(input$HLAmm_DR,{
    newDR <- input$HLAmm_DR
    updateRadioButtons(session, "mmDR", selected = newDR)
  })
  
  observeEvent(input$stroke,{
    newavc <- input$stroke
    updateCheckboxInput(session, "CVA_N", value = newavc) 
  })
  
  observeEvent(input$CVA_N,{ 
    newavc <- input$CVA_N
    updateCheckboxInput(session, "stroke", value = newavc) 
  })
  
  observeEvent(input$ageD,{
    newaged <- input$ageD
    updateSliderInput(session, "age_N", value = newaged) 
  })
  
  observeEvent(input$age_N,{ 
    newaged <- input$age_N
    updateSliderInput(session, "ageD", value = newaged) 
  })
  
})

