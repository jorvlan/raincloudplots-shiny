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
library(shinyWidgets)
library(shinydashboard)
library(datasets)
library(DT)
library(ggrain)


#setwd("/Users/jordyvanlangen/Desktop/raincloudplots_shiny/raincloudplots")
#setwd("/Users/jordyvanlangen/Desktop/raincloudplots_shiny/raincloudplots/www")

## functions

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
                        
                        menuItem("Make Rainclouds", tabName = "uploaddata", icon = icon("upload", lib = "glyphicon"))
                        
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
                                fluidRow(
                                  tabBox(title = "",
                                         id = "tabset-about", width = "10",
                                         tabPanel("Background", includeHTML("about.html"))))),
                      
                        
                        
                  
                        # -| 2nd tab content ----
                        tabItem(tabName = "uploaddata",
                                h2("Upload your data"),
                                # fileInput("upload", NULL, multiple = FALSE, accept = c(".xlsx", ".xls", ".txt", ".csv")),
                                
                                # selectInput("upload_delim", label = "Select Delimiter (for text file):", choices =list("Comma" = ",",
                                #                                                                                        "Tab" = "\t",
                                #                                                                                        "Semicolon" = ";",
                                #                                                                                        "Space" = " ")),
                                fluidRow(                                  
                                  shiny::column(width = 2, uiOutput("DataSource")),
                                  shiny::column(width = 2,
                                                radioButtons(inputId = "inputType",
                                                             label = "",
                                                             choices = c("User Data", "Iris Data"), # "Example Data"),
                                                             selected = "Iris Data")),

                                  shiny::column(width = 2,
                                                checkboxInput(inputId = "toggle_tidy", label = "Convert to tidy", value = FALSE)),
                                  
                                ),
                                
                                # View User Data Table
                                # fluidRow(
                                #   uiOutput("UserData")
                                # ),
                                
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
                                
                                # # View Final Data Frame
                                # fluidRow(
                                #   uiOutput("FinalData")
                                # ),
                                shiny::br(),
                                sidebarPanel(
                                  uiOutput("picker_variable"),
                                  uiOutput("picker_group")
                                  #actionButton("variable", "Variable"),
                                ),
                                # sidebarPanel(
                                #   uiOutput("picker_group")#,
                                # ),
                                sidebarPanel(
                                sliderInput("height", "height", min = 100, max = 1000, value = 300, step = 100),
                                sliderInput("width", "width", min = 100, max = 1000, value = 500, step = 100)),
                                shiny::br(), shiny::br(),
                                tabBox(
                                  title = "",
                                  # The id lets us use input$tabset1 on the server to find the current tab
                                  id = "tabset1", height = "300px", width = "500px",
                                  tabPanel(title = "Data", value =  "",
                                             uiOutput("UserData")
                                           ),
                                  tabPanel(title = "Plot", value =  "", 
                                           plotOutput("rain", width = 500, height = 300),
                                           downloadButton("downloadPlotPDF", "Download pdf-file", style = "padding: 5px 5px 5px 5px; margin: 300px 5px 5px 5px; "),
                                           downloadButton("downloadPlotSVG", "Download svg-file", style = "padding: 5px 5px 5px 5px; margin: 300px 5px 5px 5px; "),
                                           downloadButton("downloadPlotPNG", "Download png-file", style = "padding: 5px 5px 5px 5px; margin: 300px 5px 5px 5px; "))
                                )
                        ))))


# Define server functions ----
server <- function(input, output) { 
  # The currently selected tab from the first box
  # output$tabset1Selected <- renderText({
  #   input$tabset1
  # })
  
  #### DISPLAY example data ##################
  
 # output$df_example_tidy_1x1 <- DT::renderDataTable({
    #DT::datatable(
#    df_example_tidy_1x1})
  
 # output$df_example_tidy_2x2 <- DT::renderDataTable({
    #       DT::datatable(
  #  df_example_tidy_2x2})
  
  

  
  ## Select Data Source
  output$DataSource <- renderUI({
    input$inputType == "User Data"
    fileInput(inputId = "userfile", 
              label = "CSV, Excel, & SPSS files",
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
    } else if (input$inputType == "Iris Data"){
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
      div(style = "overflow-x: scroll", tableOutput("dataTable")) # style = "overflow-x: scroll", 
    )
  })
  output$dataTable <- renderTable(userdata()) #head(userdata(), row = 1000))
  
  
  
  output$picker_variable <- renderUI({
    pickerInput(inputId = 'pick_var', 
                label = 'Choose Variable', 
                choices = colnames(userdata()),
                options = list(`actions-box` = TRUE),multiple = F)
  })
  
  output$picker_group <- renderUI({
    pickerInput(inputId = 'pick_grp', 
                label = 'Choose Group',
                choices = colnames(userdata())[as.logical(sapply(userdata(), is.character) + sapply(userdata(), is.factor) == 1)],
                options = list(`actions-box` = TRUE),multiple = F)
  })
  
  # ct <- reactive({input$pick_var})
  
  output$rain <- renderPlot(
    width = function() input$width,
    height = function() input$height,
    res = 96,
    {
      ggplot(userdata(), aes(y = .data[[input$pick_var]], 
                             x = .data[[input$pick_grp]],
                             fill = .data[[input$pick_grp]])) + 
        geom_rain()
    })
}

# https://stackoverflow.com/questions/65564029/r-shiny-reactive-x-axis-ggplot


# Run the shiny application ----
shinyApp(ui = ui, server = server, options = list(launch.browser = T))




# when the user uploads data, it should switch to user data?





