### Load the necessary libraries 
library(shiny)
library(readr)
library(DT)
library(shinythemes)
library(shinydashboard)
library(summarytools)

### Defining the user interface
ui <- navbarPage(
  theme = shinytheme("flatly"),
  title = "Naive Bayes Classifier App",
  
  tabPanel("Loading data", 
           sidebarLayout(
             sidebarPanel(
               fileInput("file", "Select a CSV or XLSX file."),
               uiOutput("chooseYColumn"),
               actionButton("validateButton", "Validate"),
               actionButton("viewDataButton", "View Data"),
               numericInput("numRows", "Number of rows to display", value = 5)
             ),
             mainPanel(
               verbatimTextOutput("X_output"),
               verbatimTextOutput("y_output")
             )
           )
  ),
  
  tabPanel("Data exploration",
           sidebarLayout(
             sidebarPanel(
               actionButton("generateStats", "Generate statistic")
             ),
             mainPanel(
               tabPanel("Résumé des statistiques", dataTableOutput("summaryTable"))
             )
           )
  ),
  
  tabPanel("Fit",
           tabsetPanel(
            tabPanel("Fit",
                     sidebarLayout(
                       sidebarPanel(
                         sliderInput("testSize", "Select the size of test dataset",
                                     min = 0.0, max = 1.0, value = 0.3, step = 0.01),
                         actionButton("splitDataButton", "Split Data"),
                         selectizeInput("selectedXVariables", "Select explanatory varables",
                                        choices = NULL, multiple = TRUE),
                         actionButton("trainButton", "Train model")
                         
                       ),
                       mainPanel(
                         verbatimTextOutput("message"),
                         verbatimTextOutput("message_split")
                         
                       )
                     )
                     ),
            tabPanel("Plot",
                     sidebarLayout(
                       sidebarPanel(
                         actionButton("plotButton", "Show plot")
                       ),
                       mainPanel(
                         plotlyOutput("plot_freq")
                       )
                     )
                     )
           )

  ),
  
  tabPanel("Predict",
           tabsetPanel(
             tabPanel("Prediction",
                      sidebarLayout(
                        sidebarPanel(
                          actionButton("predictButton", "Predict"),
                          downloadButton("exportButton", "Export results", class = "btn-success")
                        ),
                        mainPanel(
                          DTOutput("predictionTable")
                        )
                      )
             ),
             tabPanel("Measure",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("scoreType", "Select score type",
                                      choices = c("table", "accuracy"), selected = "accuracy"),
                          actionButton("scoreButton", "Score")
                        ),
                        mainPanel(
                          conditionalPanel(
                            condition = "input.scoreType == 'accuracy'",
                            helpText("Prediction Results Using Testing data"),
                            verbatimTextOutput("accuracyOutput")
                          ),
                          conditionalPanel(
                            condition = "input.scoreType == 'table'",
                            helpText("Prediction Results Using Testing data"),
                            verbatimTextOutput('scoreTable')
                          )
                        )
                      )
             ),
             tabPanel("Probability",
                      sidebarLayout(
                        sidebarPanel(
                          actionButton("generateProbabilitiesButton", "Generate Probabilities")
                        ),
                        mainPanel(
                          verbatimTextOutput("probabilityOutput")
                        )
                      )
             )
           )
  )
)
