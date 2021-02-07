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
                          , titleWidth = 450
                          )

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("OPTN", tabName = "optn", icon = icon("columns")
             ),
    menuItem("Transplant score", icon = icon("th"), tabName = "Txscore",
             badgeLabel = "in progress", badgeColor = "yellow"
             ),
    menuItem("Correlation", icon = icon("th"), tabName = "corr",
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
  # , valueBox(value=''
  #          , subtitle ='test'
  #          , icon = NULL
  #          , color = "aqua"
  #          , width = 4,
  #          href = NULL)
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
  sliderInput("cold", "select cold esquemia time (hours):",
              min = 0, max = 30,
              value = 15, step = 1, sep = "")
  ),
  box(
    checkboxInput("raceAA", "Black race", value = FALSE),
    checkboxInput("hipertension", "Hipertensive", value = FALSE),
    checkboxInput("diabetesD", "Diabetic", value = FALSE),
    checkboxInput("stroke", "Stroke as cause of death", value = FALSE),
    checkboxInput("dcd", "Donation after cardiac death", value = FALSE),
    checkboxInput("hcv", "Hepatitis C", value = FALSE),
    checkboxInput("enbloc", "Enbloc kidney transplant", value = FALSE),
    checkboxInput("double", "Double kidney transplant", value = FALSE),
    radioButtons("mmB", "number of HLA-B mismatches:", choices = 0:2, selected = 0, inline = T),
    radioButtons("mmDR", "number of HLA-DR mismatches:", choices = 0:2, selected = 0, inline = T) 
    
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "optn"
            , h2("OPTN scores")
            , h5("Organ Procurement and Transplantation Network (OPTN) from the U.S. Department of Health & Human Services made available scores to classify candidates and donors for kideny transplantation.") 
            , h5("The Estimated Post Transplant Survival (EPTS) score [1] can be applied to kidney transplant candidates when a donor is available in order to proritize those candidates.") 
            , h5("EPTS score can range from 0% to 100% where lower values correspond to healthier patients and consequently more fit to be transplanted but not necessarely those more in need for a transplant.")
            , h5("The Kidney Donor Risk Index (KDRI) [2] combines a variety of donor factors to summarize the risk of graft failure after kidney transplant into a single number.")
            , h5("The Kidney Donor Percentual Index (KDPI) [3] is a remapping of the KDRI onto a cumulative percentage scale, such that a donor with a KDPI of 80% has higher expected risk of graft failure than 80% of all kidney donors.")
            , h4("EPTS options:") 
            , frow1 
            , frow2 
            , h4("KDPI options:")
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
            , HTML('<a href="https://bioestatisticas.wixsite.com/bioestatisticas/tor" style="text-align:right">Bruno A Lima, Oficina de Bioestatística, 2020 <i class="fa fa-creative-commons"></i></a>')
            )
    )
)

shinyUI(
  dashboardPage(header, sidebar, body)
  
  
#   fluidPage(
#   # nome no browser
#   headerPanel(title ="",
#               windowTitle = "TOR | Oficina de BioEstatistica"), 
#   
#   # Título da página com imagem OB
#   titlePanel(a(href="http://bioestatisticas.wixsite.com/bioestatisticas", target = "_blank",
#                img(src='ob.jpg', align = "right",height=60,width=150))),
#   a(href="http://bioestatisticas.wixsite.com/bioestatisticas/rat", target = "_blank",
#     h1("Transplant Open Registry (TOR)")),
# 
#   #tags$head(includeScript("gtagUE28.js")),
#   
#   hr(),
#   
#   tagList(
#     navbarPage(theme = shinytheme("cerulean"),
#                title=div(img(src="openDoor1.jpg"), "TOR"),
#                tabPanel("OPTN", icon = icon("heartbeat"),
#                         h3("Transplant Scores"),
#                         # Sidebar with a slider input for year
#                         sidebarLayout(
#                           sidebarPanel(
#                             wellPanel(
#                               h5("EPTS options:"),
#                               sliderInput("age", "select patient's age (years):",
#                                           min = 18, max = 99,
#                                           value = 40, step = 1, sep = ""),
#                               checkboxInput("diabetes", "Diabetes", value = FALSE),
#                               checkboxInput("priortx", "Prior transplants", value = FALSE),
#                               sliderInput("tdialysis", "Time on dialysis (months):",
#                                           min = 0, max = 400,
#                                           value = 200, step = 1, sep = "")
#                               ),
#                             wellPanel()
#                             ), 
#                           # Show a plot of the countries distribution
#                           mainPanel(
#                             valueBoxOutput("eptsBox")
#                             # br(),
#                             # br(),
#                             # br(),
#                             # h5("Divergencia em relação à média anual (valores normalizados)"),
#                             # #plotOutput("divPlot"),
#                             # br(),
#                             # h5("Correlação entre os transplantes com DV e com DC"),
#                             # #plotlyOutput("corrPlot"),
#                             # br(),
#                             # h5("Evolução da ordem de cada país"),
#                             # #plotOutput("tipoPlot")
#                             )),
#                         hr(),
#                         print("ALGUM TEXTO DESCRITIVO")
#                         ),
#                tabPanel("Outra TAB", icon = icon("database"),
#                         p("Dados – valores, registos, observações ou ocorrências em estado bruto. Quando apresentados numa folha de cálculo (com colunas e linhas identificadas) são designados de dados estruturados. Exemplos: a idade de um dador cadáver; o tempo de diálise de um candidato a transplante renal; o grupo sanguíneo de um doente transplantado."),
#                         p("Informação – mensagem gerada a partir de um conjunto de dados processados. Exemplos: média de idades dum grupo de dadores cadáver; mediana do tempo de diálise até ao transplante de um grupo de candidatos em lista de espera; gráfico de barras com as frequências relativas dos grupos sanguíneos dum conjunto de doentes transplantados."),
#                         p("Conhecimento – Disseminação da experiência obtida com a capacidade de processar um conjunto de informação. Exemplo: um artigo publicado numa revista científica."),
#                p("Dados abertos – dados que possam ser acedidos, processados, reutilizados, modificados e partilhados por qualquer pessoa e para qualquer propósito sem qualquer tipo de restrições."),
#                br(),
#                h6("Bruno A Lima. A call for open data of renal transplantation in Portugal. Port J Nephrol Hypert 31(3), 2017: 155-157")
#                )
#       
#       )
#     ),
#   
#   hr(),
#   fluidRow(
#     column(8, icon("copyright"), 
#            a("2018, 'Oficina de BioEstatística' - All Rights Reserved |"),
#            icon("envelope-o"), a(href="http://bioestatisticas.wixsite.com/bioestatisticas/contact","bioestatisticas@gmail.com")
#     )),
#   hr()  
#   
# )

)
