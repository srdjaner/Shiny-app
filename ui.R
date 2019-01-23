library(shiny)
library(shinyAce)
library(tidyverse)
library(ggplot2)
library(DT)
library(dplyr)

shinyUI(fluidPage(
  #titlePanel("RStudio shiny app for communicating a data science process."),
  navbarPage("RStudio Shiny App",
             tabPanel("App",
                      navlistPanel(
                        "Six Steps",
                        widths = c(2, 8),
                        tabPanel("Import",
                                 sidebarLayout(
                                   sidebarPanel(
                                     conditionalPanel(
                                       condition = "input.own == false",
                                       selectInput("dataset", label = "Choose Dataset", choices = c("Iris", "Abalone", "Wine"))
                                     ),
                                     conditionalPanel(
                                       condition = "input.own == true",
                                       fileInput('file', 'Choose CSV File',
                                                 accept=c('text/csv', 
                                                          'text/comma-separated-values,text/plain', 
                                                          '.csv'))
                                     ),
                                     checkboxInput("own", "Upload Dataset"),
                                     actionButton("save_button", "Save")
                                   ),
                                   mainPanel(DT::dataTableOutput("mytable1")
                                   )
                                 )         
                        ),
                        tabPanel("Tidy",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("tidy_type", label = "Chose the operation",
                                                 choices = c("General Formatting","Separate", "Arrange", "Handle Missing Data", "Delete a Column", "Filter", "Standardize" )),
                                     conditionalPanel(
                                       condition = "input.tidy_type == 'Standardize'",
                                       uiOutput("select_name_to_normalize"),
                                       actionButton("normalize_button", "Apply")
                                     ),
                                     conditionalPanel(
                                       condition = "input.tidy_type == 'Separate'",
                                       uiOutput("select_name_to_separate"),
                                       textInput('name_separated1', 'Enter new name 1'),
                                       textInput('name_separated2', 'Enter new name 2'),
                                       actionButton("sep_button", "Apply")
                                     ),
                                     conditionalPanel(
                                       condition = "input.tidy_type == 'Filter'",
                                       uiOutput("select_name_to_filter"),
                                       radioButtons(inputId = "comparison", label = "Select the operation", choices = list("=", ">", "<")),
                                       textInput('value', 'Enter value to compare with'),
                                       actionButton("filter_button", "Apply")
                                     ),
                                     conditionalPanel(
                                       condition = "input.tidy_type == 'Delete a Column'",
                                       uiOutput("select_name_to_delete"),
                                       actionButton("del_button", "Apply")
                                     ),
                                     conditionalPanel(
                                       condition = "input.tidy_type == 'Handle Missing Data'",
                                       radioButtons("fill_delete", "Operation",
                                                    choices = c(Fill = "fill",
                                                                Delete = "delete"),
                                                    selected = "fill"),
                                       actionButton("NA_button", "Apply")
                                     ),
                                     conditionalPanel(
                                       condition = "input.tidy_type == 'General Formatting'&&input.own == true",
                                       checkboxInput("header", "Header", TRUE),
                                       radioButtons("sep", "Separator",
                                                    choices = c(Comma = ",",
                                                                Semicolon = ";",
                                                                Tab = "\t"),
                                                    selected = ","),
                                       radioButtons("quote", "Quote",
                                                    choices = c(None = "",
                                                                "Double Quote" = '"',
                                                                "Single Quote" = "'"),
                                                    selected = '"'),
                                       hr(),
                                       radioButtons("disp", "Display",
                                                    choices = c(Head = "head",
                                                                All = "all"),
                                                    selected = "head"),
                                       hr(),
                                       actionButton("tidy_button", "Tidy")
                                     ),
                                     conditionalPanel(
                                       condition = "input.tidy_type == 'Arrange'",
                                       uiOutput("select_name_to_arrange"),
                                       radioButtons("order", "Order",
                                                    choices = c(Descending = "descending",
                                                                Ascending = "ascending"),
                                                    selected = "ascending"),
                                       actionButton("arr_button", "Apply")
                                     )
                                   ),
                                   mainPanel(
                                     conditionalPanel(
                                       condition = "input.tidy_type == 'Separate'||input.tidy_type == 'Standardize'||input.tidy_type == 'Arrange'||input.tidy_type == 'Filter'||input.tidy_type == 'Handle Missing Data'||input.tidy_type == 'Delete a Column'",
                                       mainPanel(DT::dataTableOutput("table_tidy"))
                                     ),
                                     conditionalPanel(
                                       condition = "input.tidy_type == 'General Formatting' && input.own == true",
                                       tableOutput("contents")
                                     ),
                                     conditionalPanel(
                                       condition = "input.tidy_type == 'General Formatting' && input.own == false",
                                       h4("Example datasets are already formatted according to the app!")
                                     )
                                   )
                                 )
                        ),
                        tabPanel("Transform",
                                 sidebarLayout(
                                   sidebarPanel(
                                     radioButtons(inputId = "op", label = "Select the operation", choices = list("X+Y", "X-Y", "a*X", "X^a")),
                                     hr(),
                                     uiOutput("ColumnSelector_xx"),
                                     conditionalPanel(
                                       condition = "input.op == 'a*X' || input.op == 'X^a'",
                                       numericInput("const", "Choose constant a", value = 1, step = 0.1)
                                     ),
                                     conditionalPanel(
                                       condition = "input.op == 'X+Y' || input.op == 'X-Y'",
                                       uiOutput("ColumnSelector_yy")
                                     ),
                                     hr(),
                                     textInput('NewCol', 'Enter new column name'),
                                     actionButton("btn", "Add column")
                                   ),
                                   mainPanel(
                                     DT::dataTableOutput("mytable2_make")
                                   )
                                 )
                        ),
                        tabPanel("Visualization",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("plot_type", "Choose visualization:", choices = c("Histogram", "Numerical", "T-Test")),
                                     hr(),
                                     conditionalPanel(
                                       condition = "input.plot_type == 'T-Test'",
                                       radioButtons("sample",
                                                    "Please choose one sample t test or two sample t test:",
                                                    choices = c(Two  = "twoSamp", 
                                                                One  = "oneSamp"))
                                     ),
                                     uiOutput("ColumnSelector"),
                                     conditionalPanel(
                                       condition = "input.plot_type == 'Histogram'",
                                       sliderInput("bins","Number of bins:",min=1,max=50,value=30)
                                     ),
                                     conditionalPanel(
                                       condition = "input.plot_type == 'Numerical'"
                                     ),
                                     conditionalPanel(
                                       condition = "input.plot_type == 'T-Test' && input.sample == 'twoSamp'",
                                       uiOutput("sample_two")
                                     ),
                                     conditionalPanel(
                                       condition = "input.plot_type == 'T-Test' && input.sample == 'oneSamp'",
                                       numericInput("mu", label = "Input null hypotesis mean:", value = 1, step = 0.1)
                                     ),
                                     actionButton("visual_button", "Code")
                                   ),
                                   mainPanel(
                                     conditionalPanel(
                                       condition = "input.plot_type == 'Histogram'",
                                       plotOutput("distPlot")
                                     ),
                                     conditionalPanel(
                                       condition = "input.plot_type == 'Numerical'",
                                       verbatimTextOutput("summary")
                                     ),
                                     conditionalPanel(
                                       condition = "input.plot_type == 'T-Test'",
                                       h2("Key summary statistics"),
                                       p("The observed sample statistics were:"),
                                       tableOutput('parametric'),
                                       h2("Hypothesis of the t-test"),
                                       p("The observed t test statistic:"),
                                       textOutput('tvalue'),
                                       p("A low P value suggests that your sample provides enough evidence that you can reject the null hypothesis for the entire population."),
                                       textOutput('pvalue')
                                     )
                                   )
                                 )
                        ),
                        tabPanel("Model",
                                 sidebarLayout(
                                   sidebarPanel(
                                     uiOutput("ColumnSelector_x"),
                                     uiOutput("ColumnSelector_y"),
                                     hr(),
                                     numericInput('clusters', 'Cluster count', 3,
                                                  min = 1, max = 9),
                                     actionButton("model_button", "Code")
                                   ),
                                   mainPanel(
                                     plotOutput('plot_model')
                                   )
                                 )
                                 
                        ),
                        tabPanel("Communicate",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("action", "Choose action:", choices = c("Save plot", "Save dataset", "Save model")),
                                     hr(),
                                     conditionalPanel(
                                       condition = "input.action == 'Save plot'",
                                       radioButtons(inputId = "var3", label = "Select the file type", choices = list("png", "pdf")),
                                       downloadButton("down_plot", label = "Download plot")
                                     ),
                                     conditionalPanel(
                                       condition = "input.action == 'Save model'",
                                       radioButtons(inputId = "var5", label = "Select the file type", choices = list("png", "pdf")),
                                       downloadButton("down_model", label = "Download model")
                                     ),
                                     conditionalPanel(
                                       condition = "input.action == 'Save dataset'",
                                       radioButtons(inputId = "var4", label = "Select the file type", choices = list("Excel (CSV)", "Text (TSV)", "Text (space separated)", "Doc")),
                                       downloadButton("down_data", label = "Download data"),
                                       actionButton("save_button_code", "Code")
                                     )
                                   ),
                                   mainPanel(
                                     conditionalPanel(
                                       condition = "input.action == 'Save plot'",
                                       plotOutput("savePlot")
                                     ),
                                     conditionalPanel(
                                       condition = "input.action == 'Save model'",
                                       plotOutput("saveModel")
                                     ),
                                     conditionalPanel(
                                       condition = "input.action == 'Save dataset'",
                                       h4("Head of the new dataset"),
                                       tableOutput("contents2")
                                     )
                                   )
                                 )
                        )
                      )
             ),
             tabPanel("Underlying Code"
             ),
             navbarMenu("More",
                        tabPanel("About",
                                 h4("This is a project of Srdjan Milojevic as a part of Business Analysis Seminar.")
                        )
             )
             
  ),
  h4("Here is the code..."),
  aceEditor("myEditor", "", mode = "r", readOnly = TRUE, theme = "chrome")
)
)
