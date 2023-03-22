# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Introduction ----
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# raincloudplots: A shinyApp for easy & flexible data visualization
#
# Raincloud plots were created and developed by Micah Allen, Davide Poggiali, Kirstie Whitaker, Tom Rhys Marshall, Jordy van Langen and Rogier Kievit. 
# New to 'raincloudplots'? Please check-out our paper:
#
# - Allen, M., Poggiali, D., Whitaker, K., Marshall, T. R., van Langen, J., & Kievit, R.A.
#   Raincloud plots: a multi-platform tool for robust data visualization [version 2; peer review: 2 approved]
#   Wellcome Open Research 2021, 4:63. https://doi.org/10.12688/wellcomeopenres.15191.2
#
# The creation of this ShinyApp is part of our larger project called: "Raincloud plots 2.0", more info can be found here: https://nwo.nl/en/projects/203001011
# Team members Rogier Kievit and Jordy van Langen were awarded the inaugural "NWO Open Science Fund 2021", which allowed us to further develop our raincloudplots tools.
# One of the tools that we have created is this ShinyApp.
#
# More information about Raincloud plots in general is available on our dedicated GitHub page: https://github.com/RainCloudPlots/RainCloudPlots
# More information about this specific "raincloudplots" ShinyApp is available on our other dedicated GitHub page: https://github.com/jorvlan/raincloudplots-shiny
# More information about our new "Raincloud plots 2.0" project is available on the website of NWO (Dutch Research Counsil): [https://nwo.nl/en/projects/203001011]
# 
# If you have a technical question or a problem using our ShinyApp, please open an issue on the designated GitHub page: https://github.com/jorvlan/raincloudplots-shiny
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# |- Credits ----
# written by: Jordy van Langen (Personal GitHub: https://github.com/jorvlan)
# date: 13-05-2022
# contact:jordyvanlangen@gmail.com or DM on Twitter: https://twitter.com/jordyvanlangen
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load packages ----

## If you haven't installed these packages, please start do so with `install.packages()`
library(shiny)
library(shinydashboard)
library(datasets)
library(DT)

#setwd("/Users/jordyvanlangen/Desktop/raincloudplots_shiny/raincloudplots")
#setwd("/Users/jordyvanlangen/Desktop/raincloudplots_shiny/raincloudplots/www")

## functions
source("raincloud_1x1.R")
source("raincloud_1x1_repmes.R")
source("raincloud_2x2_repmes.R")
source("raincloud_2x3_repmes.R")
source("geom_flat_violin.R")

## Read in the example dataset 'iris' {datasets}
df_example <- datasets::iris

# Define UI functions ----
ui <- dashboardPage(skin = 'blue',
  
  # |- App title ----                                
  dashboardHeader(title = 'raincloudplots'),
  
  # |- Side bar boxes ----
  dashboardSidebar(
    sidebarMenu(

      menuItem("About", tabName = "aboutraincloudplots"),
      
      menuItem("Raincloudplot options", tabName = "raincloudplotexamples", icon = icon("stats", lib = "glyphicon"), startExpanded = FALSE,
        menuSubItem("raincloudplot 1 by 1", tabName = "subitem1"),
        menuSubItem("raincloudplot 1 by 1 RM", tabName = "subitem2"),
        menuSubItem("raincloudplot 2 by 2 RM", tabName = "subitem3")),
      
      menuItem("Example data", tabName = "exampledata", icon = icon("eye-open", lib = "glyphicon"), startExpanded = FALSE,
        menuSubItem("Example data 1x1", tabName = "subitem4"),
        menuSubItem("Example data 2x2", tabName = "subitem5")),
      
      menuItem("Upload data", tabName = "uploaddata", icon = icon("upload", lib = "glyphicon")),
      menuItem("Download plot", tabName = "downloadplot", icon = icon("download", lib = "glyphicon")),
      menuItem("Source code for app", icon = icon("console", lib = "glyphicon"), href = "https://github.com/jorvlan/raincloudplots-shiny/app.R"),
      
      sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                        label = "Search...")

    )
  ),
  
  # |- Body content ----
  dashboardBody(
    
    # |-- Custom Font for Title ----
    tags$head(tags$style(HTML('
    .main-header .logo {
    font-family: Calibri, Helvetica, sans-serif;
    font-weight: bold;
    font-size: 25px;
    }'))),
    
  
    tabItems(
      
      # |-- 1 tab content ----
      tabItem(tabName = "aboutraincloudplots",
              #h2("History"),
      fluidRow(
        tabBox(title = "",
               id = "tabset-about", width = "10",
               tabPanel("Background", includeHTML("about.html"))))),
      
      # |-- 2nd tab content ----
      tabItem(tabName = "raincloudplotexamples",
              h2("The types of raincloudplots you can create with this ShinyApp")),
      
      # -| 3rd tab content ----
      tabItem(tabName = "subitem1",
              h2("The 1 by 1 raincloudplot (horizontal)"),
      tags$img(src="1x1_h.png", height="70%", width="70%"),
              h2("The 1 by 1 raincloudplot (vertical)"),
      tags$img(src='1x1.png', height="70%", width="70%")),
      
      # -| 4th tab content ----
      tabItem(tabName = "subitem2",
              h2("The 1 by 1 repeated measures raincloudplot"),
      tags$img(src='1x1_rm.png', height="75%", width="75%")),
      
      # -| 5th tab content ----
      tabItem(tabName = "subitem3",
              h2("The 2 by 2 repeated measures raincloudplots"),
      tags$img(src='2x2_rm_2.png', height="70%", width="70%", align = 'center'),
      tags$img(src='2x2_rm.png', height="70%", width="70%", align = 'center')),
      
      # -| 6th tab content ----
      tabItem(tabName = "subitem4",
              h2("Pre-specified dataset for raincloudplot 1 by 1"),
              h5("This is how the dataset looks like when it will be used to make a 1 by 1 raincloudplot."),
              # View User Data Table
              fluidRow(
                DT::dataTableOutput("df_example_tidy_1x1")),
              downloadButton("csv", "Download csv-file")),
      
      # -| 7th tab Content ----
      tabItem(tabName = "subitem5",
              h2("Pre-specified dataset for raincloudplot 2 by 2"),
              h5("This is how the dataset looks like when it will be used to make a 2 by 2 raincloudplot."),
              # View User Data Table
              fluidRow(
                DT::dataTableOutput("df_example_tidy_2x2"))),
              #downloadButton("csv", "Download csv-file")),
  
      
      
      # -| 8th tab content ----
      tabItem(tabName = "uploaddata",
              h2("Upload your data"),
              # fileInput("upload", NULL, multiple = FALSE, accept = c(".xlsx", ".xls", ".txt", ".csv")),
              
              # selectInput("upload_delim", label = "Select Delimiter (for text file):", choices =list("Comma" = ",",
              #                                                                                        "Tab" = "\t",
              #                                                                                        "Semicolon" = ";",
              #                                                                                        "Space" = " ")),
              fluidRow(
                shiny::column(width = 3,
                              radioButtons(inputId = "inputType",
                                           label = "Select Data Source",
                                           choices = c("User Data", "Example Data"),
                                           selected = "User Data")),
                shiny::column(width = 6,
                              uiOutput("DataSource")),
                shiny::column(width = 3,
                              checkboxInput(inputId = "toggle_tidy", label = "Convert to tidy", value = FALSE)),
                ),
                
                # View User Data Table
                fluidRow(
                  uiOutput("UserData")
                ),
              
                
                # Variable Selection
                fluidRow(
                  shiny::column(width = 6,
                                uiOutput("IDVar"),
                                textOutput("Nsub")
                  )
                ),
                shiny::br(),
                fluidRow(
                  shiny::column(width = 6,
                                uiOutput("XVar"),
                                uiOutput("YVar")
                  ),
                  shiny::column(width = 6,
                                uiOutput("compVar"),
                                uiOutput("userCompName")
                  )
                ),
                fluidRow(
                  shiny::column(width = 6
                  ),
                  shiny::column(width = 6,
                                uiOutput("ItemVars"),
                                textOutput("Nitems")
                  )
                ),
                
                # Warnings for Improper Input
                fluidRow(
                  shiny::column(width = 6,
                                textOutput("xyWarning"),
                                tags$head(tags$style("#xyWarning{color: red;
                                 font-size: 15px;
                                 font-style: italic;
                                 }"))
                  ),
                  shiny::column(width = 6,
                                textOutput("itemWarning"),
                                tags$head(tags$style("#itemWarning{color: red;
                                 font-size: 15px;
                                 font-style: italic;
                                 }"))
                  )
                ),
                shiny::br(),
                
                # View Final Data Frame
                fluidRow(
                  uiOutput("FinalData")
                ),
                shiny::br(),
                
                # Press to Continue to Analysis Options
                fluidRow(
                  shiny::column(width = 6,
                                uiOutput("AnalysisOptionsButton")
                  )
                ),
                
                tabBox(
                  title = "Options",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", height = "400px", width = "12",
                  tabPanel(title = "Data upload", value = "Example dataset"),
                  tabPanel(title = "Plot", value =  "", 
                           downloadButton("downloadPlotPDF", "Download pdf-file", style = "padding: 5px 5px 5px 5px; margin: 300px 5px 5px 5px; "),
                           downloadButton("downloadPlotSVG", "Download svg-file", style = "padding: 5px 5px 5px 5px; margin: 300px 5px 5px 5px; "),
                           downloadButton("downloadPlotPNG", "Download png-file", style = "padding: 5px 5px 5px 5px; margin: 300px 5px 5px 5px; ")),
                  tabPanel(title = "Descriptives", value = "")
                ),
                fluidRow(
                  tabBox(
                    # Title can include an icon
                    title = tagList(icon("gear", lib = "glyphicon"), "tabBox status"),
                    tabPanel("Tab1",
                             "Currently selected tab from first box:",
                             verbatimTextOutput("tabset1Selected")
                    ),
                    tabPanel("Tab2", "Tab content 2")
                  ))))))
      
      # -| 7th tab content ----
      #tabItem(tabName = "downloadplot",
      #        h2("Download your plot"))))))


# Define server functions ----
server <- function(input, output) { 
  # The currently selected tab from the first box
  # output$tabset1Selected <- renderText({
  #   input$tabset1
  # })
  
#### DISPLAY example data ##################
  
    output$df_example_tidy_1x1 <- DT::renderDataTable({
        #DT::datatable(
          df_example_tidy_1x1})
    
    output$df_example_tidy_2x2 <- DT::renderDataTable({
#       DT::datatable(
       df_example_tidy_2x2})



### Here I am. 

## Select Data Source
output$DataSource <- renderUI({
  input$inputType == "User Data"
    fileInput(inputId = "userfile", 
              label = "Upload a file: (supports CSV, Excel, & SPSS files)",
              multiple = TRUE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv",
                         ".sav",
                         ".xls",
                         ".xlsx"))
  }
)

## Read In User-File (need to write function for other data types)
userdata <- reactive({
  if(input$inputType == "User Data"){
    if (is.null(input$userfile$datapath)) return(NULL)
    if (grepl(".sav", input$userfile$datapath)){
      df <- foreign::read.spss(input$userfile$datapath, to.data.frame = TRUE)
    } else if (grepl(".xls", input$userfile$datapath)){
      df <- read.table(input$userfile$datapath, sep="\t", header = TRUE)
    } else {
      df <- read.csv(input$userfile$datapath, header = TRUE)
    }
  } else if (input$inputType == "Example Data"){
      df <- read.csv("./data/iris_ct.csv")
    }
  if(input$toggle_tidy == TRUE){
    gather(df)} else{df}
  })

# you can nest reactives or just assign

# # for toggle_tidy
# mod_userdata <- reactive({
#   if(input$toggle_tidy == TRUE){
#     gather(userdata())
#   } else userdata()})


## Display User Data in Table
    output$UserData <- renderUI({
      if(is.null(userdata())) return(NULL)
      req(userdata())
      box(
        title = "Uploaded data", width = NULL, status = "primary",
        collapsible = TRUE, collapsed = FALSE,
        div(style = "overflow-x: scroll", tableOutput("dataTable"))
      )
    })
    output$dataTable <- renderTable(userdata()) #head(userdata(), row = 1000))
}


# Run the shiny application ----
shinyApp(ui = ui, server = server, options = list(launch.browser = T))




