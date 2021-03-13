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
library(gt)

header <- dashboardHeader(title="Transplants Open Registry - score Tx"
                          , titleWidth = 370
                          )

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("OPTN scores", tabName = "optn", icon = icon("columns")
             ),
    menuItem("TRANSPLANTSCORE", icon = icon("columns"), tabName = "Txscore"
             #, badgeLabel = "in progress", badgeColor = "yellow"
             ),
    menuItem("Correlations", icon = icon("chart-line"), tabName = "corr"
             #, badgeLabel = "in progress", badgeColor = "yellow"
             ),
    menuItem("Nyberg score", icon = icon("columns"), tabName = "nyberg"
             #, badgeLabel = "in progress", badgeColor = "yellow"
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
              min = 0, max = 200,
              value = 60, step = 1, sep = ""),
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
              value = 9, step = 1, sep = "")
  ),
  box(h6("Donor's characteritics:"),
    checkboxInput("raceAA", "Black race", value = FALSE),
    checkboxInput("hipertension", "Hypertensive", value = FALSE),
    radioButtons("diabetesD", "Diabetes",
                 choices = c("Absence", "Presence", "Unknown"), selected = "Absence"
                 , inline = T),
    #checkboxInput("diabetesD", "Diabetic", value = FALSE),
    checkboxInput("stroke", "Stroke as cause of death", value = FALSE),
    checkboxInput("dcd", "Donation after cardiac death", value = FALSE),
    checkboxInput("hcv", "Hepatitis C", value = FALSE),
    h6("Transplant variables:"),
    checkboxInput("enbloc", "Enbloc kidney transplant", value = FALSE),
    checkboxInput("double", "Double kidney transplant", value = FALSE),
    radioButtons("mmA", "number of HLA-A mismatches:", choices = 0:2, selected = 0, inline = T),
    radioButtons("mmB", "number of HLA-B mismatches:", choices = 0:2, selected = 0, inline = T),
    radioButtons("mmDR", "number of HLA-DR mismatches:", choices = 0:2, selected = 0, inline = T) 
    
  )
)
frow4 <- fluidRow(
  valueBoxOutput("txscoreBox")
)
frow5 <- fluidRow(
  box(sliderInput("ageR", "Recipient's age",
                  min = 18, max = 99,
                  value = 40, step = 1, sep = "")
    # , selectInput("ageR", "Recipient's age", 
    #               choices = c("18-34","35-49","50-64", "65+"),
    #               selected = "18-34", multiple =F
    #               #, selectize
    #               , width = '50%'
    #               # , size
    #               )
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
      , sliderInput("timeD", "Time on dialysis (months):",
                    min = 0, max = 200,
                    value = 60, step = 1, sep = "")
      # , selectInput("timeD", "Time on dialysis (yrs)", 
      #               choices = c("<1yr",">=1yr, <3yr",">=3yr, <=5yr", ">5yr"),
      #               selected = "<1 yr", multiple =F
      #               #, selectize
      #               , width = '50%'
      #               # , size
      # )
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
                   min = 18, max = 99,
                   value = 50, step = 1, sep = "")
      , radioButtons("diabetesDD", "Donor Diabetes",
                     choices = c("Absence", "Presence", "Unknown"), selected = "Absence"
                     , inline = T)
      , checkboxInput("ECD", "Extended Criteria Donor", value = FALSE)
      , radioButtons("mmHLA_A", "number of HLA-A mismatches:", choices = 0:2, selected = 0, inline = T)
      , radioButtons("mmHLA_B", "number of HLA-B mismatches:", choices = 0:2, selected = 0, inline = T)
      , radioButtons("mmHLA_DR", "number of HLA-DR mismatches:", choices = 0:2, selected = 0, inline = T)
      # , radioButtons("mmHLA", "HLA mismatches", 
      #                #label=c("0","1-3","4-6"), 
      #                choices = c("0","1-3","4-6"), selected = "0", inline = T)
      )
)
frow6 <- fluidRow(
  valueBoxOutput("nybergscoreBox")
)
frow7 <- fluidRow(
 box(title = "Donor's variables"
      , solidHeader = T
      , status="info"
      , sliderInput("age_N", "Donor's age (years)",
                       min = 18, max = 99,
                       value = 40, step = 1, sep = "")
      # , selectInput("age_N", "Donor's age (years)", 
      #               choices = c("<30","30-39","40-49", "50-59", "60-69", "70+"),
      #               selected = "<30", multiple =F
      #               #, selectize
      #               , width = '50%'
      #               # , size
      #               )
      , selectInput("hyper_N", "History of hypertension", 
                    choices = c("None","Yes; duration unknown","<=5y", "6-10y", ">10y"),
                    selected = "None", multiple =F
                    #, selectize
                    , width = '50%'
                    # , size
                    )
      , selectInput("cretin_N", "Creatinine clearance, mL/min", 
                    choices = c("100+","75-99","50-74", "<50"),
                    selected = "100+", multiple =F
                    #, selectize
                    , width = '50%'
                    # , size
                    )
     , radioButtons("HLAmm_A", "number of HLA-A mismatches:", choices = 0:2, selected = 0, inline = T)
     , radioButtons("HLAmm_B", "number of HLA-B mismatches:", choices = 0:2, selected = 0, inline = T)
     , radioButtons("HLAmm_DR", "number of HLA-DR mismatches:", choices = 0:2, selected = 0, inline = T)
      # , radioButtons("HLAmm_N", "HLA mismatch, no. of antigens",
      #                choices = c("0", "1-2", "3-4", "5-6"), 
      #                selected = "0", 
      #                inline = T
      #                )
      , checkboxInput("CVA_N", "Cause of death: CVA", value = FALSE)
      , footer = "CVA = cerebrovascular accident, 
      including ischemic and hemorrhagic types."
      )
 , box(title = "Table from Nyberg, et al [7]", 
       status = "primary", solidHeader = TRUE
       #, width = 12
       , imageOutput("table_Nyberg"
                   , height = "auto"
                   )
       )
  
)


body <- dashboardBody(
  tags$head(tags$link(rel = "shortcut icon", href = "openDoor1.jpg")),
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
            h2("TRANSPLANTSCORE")
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
            h2("Correlation between scores"),
            fluidRow(
              box(title = "Introduction"
                  , h5("Deceased donors’ kidneys for transplantation are a scarce good, 
                       so their use should be made in order to guarantee a good outcome as far as possible. 
                       The development of risk scores that can predict the outcome inherent to the donor-recipient pair 
                       in transplantation has allowed for better decision making 
                       in the allocation of organs and in patients’ clinical management. 
                       Organ Procurement and Transplantation Network (OPTN) uses two different scores: 
                       an Estimated Post Transplant Survival (EPTS) score (based on patients characteristics) 
                       to summarize the fitness of the patient; and a Kidney Donor Percentual Index (KDPI), 
                       that combines factors from donors and from the transplant, to summarize the risk of graft failure. 
                       Also, Molnar, et al proposed a TRANSPLANTSCORE to predict posttransplant outcomes using pretransplant information.")
                  , h5("With this analysis, we aim to understand how 
                  TRANSPLANTSCORE correlates with the scores from OPTN, 
                  i.e, EPTS and KDPI.")
                  , solidHeader = T
                  , status="danger"),
              box(title = "Methods"
                  , solidHeader = T
                  , h5("Data from 140 simulated kidney transplants was obtained from 
                  Kidney Allocation Rules Simulator (KARS) [6] application. 
                       We used KARS' example data and applied UK Transplant algorithm for the selection of those donor-recipient pairs.")
                  , h5("To each one of the selected pairs we computed the scores EPTS, KDPI and TRANSPLANTSCORE. 
                  For each recipient we have data on age, time on dialysis and diabetic status; 
                  for each donor, we have data on age, height and hypertension history; 
                  and we also have data on the number of HLA mismatches. 
                  For all other variables needed to compute the three scores, 
                  we used the values presented as default in this application. 
                       We also defined a variable fo good transplant prognostic to those pairs with both EPTS and KDPI values lower than 40%.")
                  , h5("Correlations between TRANSPLANTSCORE and OPTN scores were analyzed with Pearson's correlation coefficients and tests. 
                       The performance of TRANSPLANTSCORE to predict a good transplant prognostic as defined with EPTS < 40% and KDPI < 40% 
                       was analysed with a receiver operating characteristic (ROC) curve (and the area under the curve) 
                       and with the calculation of sensitivity and specificity values for different cutoffs of TRANSPLANTSCORE.")
                  , status="warning")
              ),
            fluidRow(
              box(title = "Correlation between TRANSPLANTSCORE and EPTS", 
                  solidHeader = T,
                  footer = "We can observe a moderate positive correlation betweem TRANSPLANTSCORE from Molnar, et al and EPTS values from OPTN.",
                  plotOutput("plot.epts"),
                  status = "info"),
              box(title = "Correlation between TRANSPLANTSCORE and KDPI", 
                  solidHeader = T,
                  #background = "red",
                  footer = "Here we observe a week positive correlation betweem TRANSPLANTSCORE from Molnar, et al and KDPI values from OPTN.",
                  plotOutput("plot.kdpi"),
                  status = "info")
            ),
            fluidRow(
              box(title = "Sensitivity & Specificity curves"
                  , status = "primary"
                  , solidHeader = T
                  , plotlyOutput("plot.sens.spec")),
              box(title = "ROC curve"
                  , plotOutput("plot.roc")
                  , solidHeader = T
                  , status = "primary"
                  , footer = "With an Area Under the Curve (AUC) of 0.73, we can conclude that TRANSPLANTSCORE is a good descriminator for a good prognostic, as defined by a kidney transplant with a donor-recipient pair with KDPI < 40% and EPTS < 40%, respectively."
                  )
                ),
            fluidRow(
              box(title = "Density functions by prognostic"
                  , plotOutput("plot.density")
                  , solidHeader = T
                  , status = "primary"
                  , footer = "Good outcome was defined as donor-recipient pairs with both EPTS and KDPI values lower than 40%.
                  As expected, to higher values of the TRANSPLANTSCORE will correspond a prognostic of a bad outcome for kidney transplantation."
                  ),
              box(title = "Conclusions"
                  , h5("With a rho = 0.55 (p value < 0.001), we can conclude that TRANSPLANTSCORE is more correlated with EPTS than with KDPI (rho = 0.29, p value < 0.01).")
                  , h5("On the other hand, TRANSPLANTSCORE have a good ability to discriminate a good transplant outcome (defined as donor-recipient pair with KDPI < 40% and EPTS < 40%, respectively). 
                       According to Youden method, we obtain the best cutoff for a TRANSPLANTSCORE of 61.4 with a specificity of 0.71 and a sensitivity of 0.66.")
                  , h5("If we could have a crystal ball at transplantation, we would like to predict time-to-graft failure or patient survival. 
                       The scores presented here, are not a crystal ball but they can help us to compare potential donor-recipient pairs and make better informed decisions.")
                  , solidHeader = T
                  , status="success")
              )
            , a(href ="https://balima.shinyapps.io/kars/"
                , "[6] - Kidney Allocation Rules Simulator (KARS). Oficina de Bioestatistica. accessed on February, 2021")
            ),
    tabItem(tabName = "nyberg",
            h2("Nyberg score")
            , h4("In 2003, Nyberg et al [7], developed and presented a scoring system 
                 to grade decesead donors' kidneys based on the success of renal transplantation. 
                 In particular, this score is a quantitative approach to assessing kidneys from expanded criteria donors.")
            , frow6
            , frow7
            , fluidRow(box(title = "Correlation between TRANSPLANTSCORE and Nyberg's score", 
                           solidHeader = T,
                           #background = "red",
                           footer = "To analyse the correlation between TRANSPLANTSCORE and Nyberg's score we used the data from 140 simulated kidney transplants as previously described. 
                           To compute Nyberg's score we only used the variables donor's age, history of hipertension (None / Yes, duration unknown) and HLA mismatches. 
                           As donors' age have the higher punctuation, this score is highly influenced by that variable. 
                           In this analysis, we found a week positive correlation betweem TRANSPLANTSCORE from Molnar, et al 
                           and Donors' score from Nyberg, et al.",
                           plotOutput("plot.nyberg"),
                           status = "info"))
            , br()
            , a(href ="https://pubmed.ncbi.nlm.nih.gov/12780563/", 
            "[7] - Improved Scoring System to Assess Adult Donors For Cadaver Renal Transplantation. 
                Nyberg S, Matas A, Kremers W, et al. 
                American Journal of Transplantation 2003; 3: 715–721")
            ),
    tabItem(tabName = "discl",
            h2("Disclaimer")
            , h4("Disclaimer: This application is intended for research purposes only, not for clinical or commercial use. It is a non-profit service to the scientific community, provided on an ‘AS-IS’ basis without any warranty, expressed or implied. The authors can not be held liable in any way for the service provided here.")
            , br()
            , strong("'score Tx' is part of the")
            , a(href = "https://transplants-open.netlify.app/index.html"
                , "Transplants Open Registry (TOR)")
            , strong("initiative 
                     intended to provide high-quality information about kidney transplantation and based solely on open source resources.")
            , br()
            , imageOutput("door")
            , br()
            , HTML('<a href="https://bioestatisticas.wixsite.com/bioestatisticas/tor" style="text-align:right">Bruno A Lima, Oficina de Bioestatística, 2021 <i class="fa fa-creative-commons"></i></a>')
            )
    )
)

shinyUI(
  dashboardPage(skin = "blue"
                , header
                , sidebar
                , body
                )
  
  
)
