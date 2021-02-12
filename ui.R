# https://rstudio.github.io/shinydashboard/structure.html
# https://rpubs.com/yzhao9/anly512_final


# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)
library(plotly)
library(shinydashboard)

header <- dashboardHeader(title="Transplant Open Registry - score Tx"
                          , titleWidth = 370
                          )

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("OPTN scores", tabName = "optn", icon = icon("columns")
             ),
    menuItem("Transplant score", icon = icon("columns"), tabName = "Txscore"
             #, badgeLabel = "in progress", badgeColor = "yellow"
             ),
    menuItem("Correlation", icon = icon("chart-line"), tabName = "corr",
             badgeLabel = "in progress", badgeColor = "yellow"
             ),
    menuItem("Disclaimer", icon = icon("exclamation-triangle"), tabName = "discl",
             badgeLabel = "Attention", badgeColor = "red"
    )
    
    )
  )

frow1 <- fluidRow(
  box(
    sliderInput("age", "select patient's age (years):",
                min = 18, max = 99,
                value = 40, step = 1, sep = ""),
    checkboxInput("diabetes", "Diabetic", value = FALSE)
    ),
  box(
    sliderInput("tdialysis", "Time on dialysis (months):",
              min = 0, max = 400,
              value = 200, step = 1, sep = ""),
    checkboxInput("priortx", "Prior transplants", value = FALSE)
    )
)

frow2 <- fluidRow(
  valueBoxOutput("eptsBox"),
  valueBoxOutput("kdpiBox")

  )

frow3 <- fluidRow(
  box(
  sliderInput("ageD", "select donor's age (years):",
              min = 18, max = 99,
              value = 40, step = 1, sep = ""),
  sliderInput("creatinine", "Serum creatinine (mg/dL):",
              min = 0.1, max = 10,
              value = 0.9, step = 0.1, sep = ""),
  sliderInput("height", "select donor's height (cm):",
              min = 140, max = 220,
              value = 180, step = 1, sep = ""),
  sliderInput("weight", "select donor's weight (Kg):",
              min = 40, max = 140,
              value = 80, step = 1, sep = ""),
  sliderInput("cold", "select cold ischemia time (hours):",
              min = 0, max = 30,
              value = 15, step = 1, sep = "")
  ),
  box(h6("Donor's characteritics:"),
    checkboxInput("raceAA", "Black race", value = FALSE),
    checkboxInput("hipertension", "Hipertensive", value = FALSE),
    checkboxInput("diabetesD", "Diabetic", value = FALSE),
    checkboxInput("stroke", "Stroke as cause of death", value = FALSE),
    checkboxInput("dcd", "Donation after cardiac death", value = FALSE),
    checkboxInput("hcv", "Hepatitis C", value = FALSE),
    h6("Transplant variables:"),
    checkboxInput("enbloc", "Enbloc kidney transplant", value = FALSE),
    checkboxInput("double", "Double kidney transplant", value = FALSE),
    radioButtons("mmB", "number of HLA-B mismatches:", choices = 0:2, selected = 0, inline = T),
    radioButtons("mmDR", "number of HLA-DR mismatches:", choices = 0:2, selected = 0, inline = T) 
    
  )
)
frow4 <- fluidRow(
  valueBoxOutput("txscoreBox")
)
frow5 <- fluidRow(
  box(selectInput("ageR", "Recipient's age", 
                  choices = c("18-34","35-49","50-64", "65+"),
                  selected = "18-34", multiple =F
                  #, selectize
                  , width = '50%'
                  # , size
                  )
      , selectInput("ESRD", "Cause of ESRD", 
                    choices = c("Diabetes","Hypertension","Glomerulonephritis", "Cystic Disease", "Other"),
                    selected = "Other", multiple =F
                    #, selectize
                    , width = '75%'
                    # , size
                    )
      , radioButtons("race", "Recipient Race", 
                     #choicesNames = c("White","Black","Hispanic", "Other"), 
                     choices = c("White","Black","Hispanic", "Other"), selected = "White", inline = T)
      , selectInput("timeD", "Time on dialysis (yrs)", 
                    choices = c("<1 yr",">=1yr, <3 yr",">=3yr, <=5yr", ">5yr"),
                    selected = "<1 yr", multiple =F
                    #, selectize
                    , width = '50%'
                    # , size
      )
      , checkboxInput("coronary", "Recipient Coronary Artery Disease", value = FALSE)
      , checkboxInput("diabetesR", "Recipient Diabetes", value = FALSE)
      ),
  box(sliderInput("albumin", "Recipient Albumin (g/dL):",
                  min = 1, max = 5,
                  value = 1.5, step = 0.1, sep = "")
      , sliderInput("hemoglobin", "Recipient Hemoglobin (g/dL):",
                    min = 3, max = 20,
                    value = 10, step = 0.1, sep = "")
      ,sliderInput("ageDD", "Donor's age:",
                   min = 18, max = 80,
                   value = 50, step = 1, sep = "")
      , radioButtons("diabetesDD", "Donor Diabetes",
                     choices = c("Absence", "Presence", "Unknown"), selected = "Absence", inline = T)
      , checkboxInput("ECD", "Extended Criteria Donor", value = FALSE)
      , radioButtons("mmHLA", "HLA mismatches", 
                     #label=c("0","1-3","4-6"), 
                     choices = c("0","1-3","4-6"), selected = "0", inline = T)
      )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "optn"
            , h2("OPTN scores")
            , h5("The Organ Procurement and Transplantation Network (OPTN) from the U.S. Department of Health & Human Services made available scores to classify candidates and donors for kideny transplantation.") 
            , h5("One of them, the Estimated Post Transplant Survival (EPTS) score [1] can be applied to kidney transplant candidates when a donor is available in order to proritize those candidates.") 
            , h5("EPTS score can range from 0% to 100% where lower values correspond to healthier patients and consequently more fit to be transplanted but not necessarely those more in need for a transplant.")
            , h5("On the other and, the Kidney Donor Risk Index (KDRI) [2] combines a variety of donor factors to summarize the risk of graft failure after kidney transplant into a single number. 
                 The Kidney Donor Percentual Index (KDPI) [3] is a remapping of the KDRI onto a cumulative percentage scale, such that a donor with a KDPI of 80% has higher expected risk of graft failure than 80% of all kidney donors.")
            , h3("EPTS options:") 
            , frow1 
            , frow2 
            , h3("KDPI options:")
            , frow3
            , a(href ="https://optn.transplant.hrsa.gov/resources/allocation-calculators/epts-calculator/"
                , "[1] - Organ Procurement and Transplantation Network. EPTS Calculator. accessed on February, 2021")
            , br()
            , a(href ="https://pubmed.ncbi.nlm.nih.gov/19623019/"
                , "[2] - A Comprehensive Risk Quantification Score for Deceased Donor Kidneys: The Kidney Donor Risk Index. 
                Rao P, Scaibel D, Guidinger M, et al. 
                Transplantation. 2009 Jul 27;88(2):231-6")
            , br()
            , a(href ="https://optn.transplant.hrsa.gov/resources/allocation-calculators/kdpi-calculator/"
                , "[3] - Organ Procurement and Transplantation Network. KDPI Calculator. accessed on February, 2021")
            ),
    
    tabItem(tabName = "Txscore",
            h2("Transplant score")
            , h5("In 2017, Molnar,  et al [4], proposed a tool [5] to predict allograft and patient survival upon kidney transplant. This tool uses data exclusively available at transplantation to predict event probabilities.") 
            , h5("Here we replicate TRANSPLANTESCORE.COM for mortality or graft failure as a combined outcome, computing the estimated 5-year event probability.")
            , h4("Estimated 5-year event (mortality or graft failure) probabilities:")
            , frow4 
            , frow5
            , a(href ="https://pubmed.ncbi.nlm.nih.gov/27391198/"
                , "[4] - Predictive Score for Posttransplantation Outcomes. 
                Molnar M, Nguyen D, Chen Y, et al. 
                Transplantation. 2017 Jun;101(6):1353-1364")
            , br()
            , a(href ="http://www.transplantscore.com/"
                , "[5] - Predictive Score for Post-Transplantation Outcomes. TRANSPLANTESCORE.COM. accessed on February, 2021")
            
            ),
    
    tabItem(tabName = "corr",
            h2("Correlation between scores")
            ),
    
    tabItem(tabName = "discl",
            h2("Disclaimer")
            , h4("Disclaimer: This application is intended for research purposes only, not for clinical or commercial use. It is a non-profit service to the scientific community, provided on an ‘AS-IS’ basis without any warranty, expressed or implied. The authors can not be held liable in any way for the service provided here.")
            , br()
            , strong("'score Tx' is part of Transplant Open Registry (TOR) initiative intended to provide high-quality information about kidney transplantation and based solely on open source resources.")
            , br()
            , HTML('<a href="https://bioestatisticas.wixsite.com/bioestatisticas/tor" style="text-align:right">Bruno A Lima, Oficina de Bioestatística, 2021 <i class="fa fa-creative-commons"></i></a>')
            )
    )
)

shinyUI(
  dashboardPage(skin = "blue"
                ,header
                , sidebar
                , body
                )
  
  
)
