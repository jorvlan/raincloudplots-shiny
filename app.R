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
# written by: Nicholas Judd, Rogier Kievit & Jordy van Langen
# date: 28-05-2023
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
ui <- dashboardPage(skin = 'black',
                    
                    # |- App title ----                                
                    dashboardHeader(title = 'raincloudplots'),
                    
                    # |- Side bar boxes ----
                    dashboardSidebar(
                      sidebarMenu(
                        
                        menuItem("About", tabName = "aboutraincloudplots"),
                        menuItem("Make Rainclouds", tabName = "uploaddata", 
                                 selected = TRUE,
                                 icon = icon("upload", lib = "glyphicon"))
                        
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
                                  shiny::column(width = 3, uiOutput("DataSource")),
                                  # shiny::column(1, checkboxInput(inputId = "inputType", label = "Iris Data", value = TRUE))),
                                  # shiny::column(width = 2,
                                  #               radioButtons(inputId = "inputType",
                                  #                            label = "",
                                  #                            choices = c("User Data", "Iris Data"), # "Example Data"),
                                  #                            selected = "Iris Data"))),
                                  shiny::column(width = 2, uiOutput("picker_variable")),
                                  shiny::column(width = 2, uiOutput("picker_group"))
                                  # fluidRow(
                                  #   sidebarPanel(
                                  #     uiOutput("picker_variable"),
                                  #     uiOutput("picker_group")
                                  #     #actionButton("variable", "Variable"),
                                  #   ),
                                  
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
                                #shiny::br(),
                                
                                tabBox(
                                  title = "",
                                  # The id lets us use input$tabset1 on the server to find the current tab
                                  id = "tabset1", height = "700px", width = "500px",
                                  tabPanel(title = "Data", value =  "",
                                             uiOutput("UserData")
                                           # DT::dataTableOutput("UserData")
                                           ),
                                  tabPanel(title = "Plot Features", value = "",
                                  #          # sidebarPanel(
                                  #          # sliderInput("height", "height", min = 100, max = 1000, value = 500, step = 100),
                                  #          # sliderInput("width", "width", min = 100, max = 1000, value = 700, step = 100)),
                                  # )
                                           ### Here you add (flipping, colors & color pallets)
                                           ### size adjustments
                                           ### alpha
                                           ### overlapping option
                                           ### change labs on axis
                                           ### background themes (xkyd!)
                                           ### base size of theme
                                  fluidRow(shiny::column(1, selectInput("side", "Side", choices = c("r", "l"), selected = "r")),
                                           shiny::column(2, numericInput("alpha", "Alpha", value = 1, min = 0.1, max = 1, step = .1, width = "35%")),
                                           shiny::column(1, checkboxInput(inputId = "flip", label = "Flip plot", value = FALSE)),
                                           shiny::column(1, checkboxInput(inputId = "overlap", label = "Overlap groups", value = FALSE))),
                                  fluidRow(shiny::column(5, textInput("title", "Plot Title"))),
                                  fluidRow(shiny::column(2, textInput("xlab", "Label for x-axis")),
                                           shiny::column(2, checkboxInput(inputId = "xaxis", label = "Remove", value = FALSE))),
                                  fluidRow(shiny::column(2, textInput("ylab", "Label for y-axis")),
                                           shiny::column(2, checkboxInput(inputId = "yaxis", label = "Remove", value = FALSE))),
                                  fluidRow(shiny::column(2, textInput("llab", "Label for legend")),
                                           shiny::column(2, checkboxInput(inputId = "leg", label = "Remove", value = FALSE))),
                                  fluidRow(shiny::column(2, numericInput("basesize", "Base Size", value = 16, min = 2, max = 50, step = 2, width = '50%')),
                                           shiny::column(2, numericInput("height", "Height", value = 500, min = 100, max = 1000, step = 100, width = '50%')),
                                           shiny::column(2, numericInput("width", "Width", value = 700, min = 100, max = 1000, step = 100, width = '50%'))),
                                  fluidRow(shiny::column(3, selectInput("colorSingle", "Single rainplot color", 
                                                                        choices = c("white", "yellow", "red","coral", "pink", "lightgreen", "darkgreen", "lightblue", "darkblue", "purple","gray", "black"), 
                                                                        selected = "coral")),
                                           shiny::column(3, selectInput("colorPal", "Group rainplot Palettes", 
                                                                        choices = c("Blues", "Oranges","Reds", "Greens","Purples", "Dark2", "Pastel1", "BuGn", "YlOrRd", "RdPu", "OrRd", "Accent", "RdBu"), 
                                                                        selected = "Dark2"))),
                                  box(title = "Advanced features", width = NULL, status = "primary", collapsible = TRUE, collapsed = TRUE,
                                      fluidRow(shiny::column(2, numericInput("d_width", "Dot width", value = 0.05, min = 0, max = 1, step = .05, width = '50%')),
                                               shiny::column(2, numericInput("d_nudge", "Dot nudge", value = 0, min = 0, max = 1, step = .05, width = '50%'))),
                                      fluidRow(shiny::column(2, numericInput("b_width", "Box width", value = 0.05, min = 0, max = 1, step = .05, width = '50%')),
                                               shiny::column(2, numericInput("b_width", "Box nudge", value = .1, min = 0, max = 1, step = .05, width = '50%'))),
                                      fluidRow(shiny::column(2, numericInput("v_width", "Violin width", value = 0.1, min = 0, max = 1, step = .05, width = '50%')),
                                               shiny::column(2, numericInput("v_width", "Violin nudge", value = 0.15, min = 0, max = 1, step = .05, width = '50%'))))
                                           ),

                                  tabPanel(title = "Raincloud Plot", value =  "", 
                                           fluidRow(plotOutput("rain", width = 500, height = 300)),
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
  
  
  userdata <- reactive({
    if (is.null(input$userfile$datapath)) {
      df <- read.csv("./data/iris_ct.csv")
      } else if (grepl(".sav", input$userfile$datapath)){
        df <- foreign::read.spss(input$userfile$datapath, to.data.frame = TRUE)
      }else if (grepl(".xls", input$userfile$datapath)){
        df <- read.table(input$userfile$datapath, sep="\t", header = TRUE)
      } else {
        df <- read.csv(input$userfile$datapath, header = TRUE)}
    })
  
  
  
  # ## Read In User-File (need to write function for other data types)
  # userdata <- reactive({
  #   if(input$inputType == "User Data"){
  #     if (is.null(input$userfile$datapath)) {
  #       return(NULL)
  #     } else if (grepl(".sav", input$userfile$datapath)){
  #       df <- foreign::read.spss(input$userfile$datapath, to.data.frame = TRUE)
  #     } else if (grepl(".xls", input$userfile$datapath)){
  #       df <- read.table(input$userfile$datapath, sep="\t", header = TRUE)
  #     } else {
  #       df <- read.csv(input$userfile$datapath, header = TRUE)
  #       print("here")
  #       print(input$inputType)
  #     }}
  #   else (input$inputType == "Iris Data")
  #   print("here2")
  #     df <- read.csv("./data/iris_ct.csv")
  #   # if(input$toggle_tidy == TRUE){
  #   #   gather(df)} 
  #   #   else{df}
  # })
  # 
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
      DT::renderDataTable(userdata())
      # div(style = "overflow-x: scroll", tableOutput("dataTable")) # style = "overflow-x: scroll",
    )
  })

  output$picker_variable <- renderUI({
    pickerInput(inputId = 'pick_var', 
                label = 'Choose Variable', 
                choices = colnames(userdata()),
                options = list(`actions-box` = TRUE),
                multiple = F)
  })
  
  output$picker_group <- renderUI({
    pickerInput(inputId = 'pick_grp', 
                label = 'Choose Group',
                selected = NULL,
                multiple = TRUE,
                options = list(pickerOptions(maxOptions = 1)),
                choices = colnames(userdata())[as.logical(sapply(userdata(), is.character) + sapply(userdata(), is.factor) == 1)],
                #options = list(`actions-box` = TRUE)
                )
  })
  
  # ct <- reactive({input$pick_var})

  
  rain_plot <- reactive({
    if(is.null(input$pick_grp)){
      ggplot(userdata(), 
             aes(y = .data[[input$pick_var]], 
                 x = 1)) + 
        geom_rain(rain.side = input$side, alpha = input$alpha,
          boxplot.args = list(fill = input$colorSingle, outlier.shape = NA),
          violin.args = list(fill = input$colorSingle)) +
        theme_minimal(base_size = input$basesize)
    } else {
      if(input$overlap == TRUE){
        ggplot(userdata(), aes(y = .data[[input$pick_var]], 
                               x = 1,
                               fill = .data[[input$pick_grp]])) + 
          geom_rain(rain.side = input$side, alpha = input$alpha,
                    boxplot.args.pos = list(position = ggpp::position_dodgenudge(x = .1), width = 0.1)) +
          theme_minimal(base_size = input$basesize) +
          scale_fill_brewer(palette = input$colorPal)
      }else{
        ggplot(userdata(), aes(y = .data[[input$pick_var]], 
                               x = .data[[input$pick_grp]],
                               fill = .data[[input$pick_grp]])) + 
          geom_rain(rain.side = input$side, alpha = input$alpha) +
          theme_minimal(base_size = input$basesize) +
          scale_fill_brewer(palette = input$colorPal)
      }
    }
  })

  
  ##### making nested options
  # flipping
  rain_plot_o1 <- reactive({
    if(input$flip == TRUE){
    rain_plot() + coord_flip()
  } else
    rain_plot()
  })
  
  
  # Add title, xlab or ylab
  # really annoying https://stackoverflow.com/questions/54987424/in-a-shiny-input-function-can-i-set-the-initial-value-to-be-missing-rather-tha
  rain_plot_o2 <- reactive({
    if(nchar(input$title) != 0){ # wants a title
      if(nchar(input$xlab) !=0 && nchar(input$ylab) != 0){ # wants both labs
        rain_plot_o1() + labs(title = input$title, x = input$xlab, y = input$ylab, caption  = is.character(input$title))
      } else if (nchar(input$xlab) == 0 && nchar(input$ylab) != 0){ #only wants ylab
        rain_plot_o1() + labs(title = input$title, y = input$ylab)
      } else if (nchar(input$xlab) != 0 && nchar(input$ylab) == 0){ #only wants xlab
        rain_plot_o1() + labs(title = input$title, x = input$xlab)
      } else { # wants no labs changing yet a title
        rain_plot_o1() + labs(title = input$title)
      }
    } else if (nchar(input$title) == 0){ # wants no title
      if(nchar(input$xlab) != 0 && nchar(input$ylab) != 0){ # yet wants both labs
        rain_plot_o1() + labs(x = input$xlab, y = input$ylab)
      } else if (nchar(input$xlab) == 0 && nchar(input$ylab) != 0){ #only wants ylab
        rain_plot_o1() + labs(y = input$ylab)
      } else if (nchar(input$xlab) != 0 && nchar(input$ylab) ==0){ #only wants xlab
        rain_plot_o1() + labs(x = input$xlab)
      } else { # wants no labs changing at all
        rain_plot_o1()
      }
    }
  })
  
  # display axis?   # remove x-axis, remove y-axis
  rain_plot_o3 <- reactive({
    if(input$xaxis == TRUE && input$yaxis == TRUE){
      rain_plot_o2() +
        theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
    }else if(input$xaxis == TRUE && input$yaxis == FALSE){
      rain_plot_o2() +
        theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
    }else if(input$xaxis == FALSE && input$yaxis == TRUE){
      rain_plot_o2() +
          theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
    }else
      rain_plot_o2()
  })

  rain_plot_o4 <- reactive({
    if(!is.null(input$pick_grp)){
      if(nchar(input$llab) != 0 && input$leg == FALSE){
        rain_plot_o3() + labs(fill = input$llab)
      } else if (input$leg == TRUE) {
        rain_plot_o3() + theme(legend.position = "none")
      } else
        rain_plot_o3()
    } else
    rain_plot_o3()
  })

  
  ## Display the rain plot
  output$rain <- renderPlot({
    rain_plot_o4()
  },
    # width = 700,
    # height = 550,
    width = function() input$width,
    height = function() input$height
    # res = 96,
)
  
  w <- function(){
    return(input$width)
  }
  h <- function(){
    return(input$height)
  }
  
  output$downloadPlotPDF <- downloadHandler(
    filename = function(file) {
      "rain_plot.pdf"
      #ifelse(is.null(input$DataFile), return(), str_c(input$Title, ".png"))
    },
    content = function(file) {
      
      ggsave(file, plot = rain_plot_o4(), units = "mm", device = "pdf", 
             width = w(), height = h()) # width = 290, height = 265, 
    }
  )
  output$downloadPlotSVG <- downloadHandler(
    filename = function(file) {
      "rain_plot.svg"
    },
    content = function(file) {
      ggsave(file, plot = rain_plot_o4(), units = "mm", device = "svg",
             width = w(), height = h())
    }
  )
  output$downloadPlotPNG <- downloadHandler(
    filename = function(file) {
      "rain_plot.png"
    },
    content = function(file) {
      ggsave(file, plot = rain_plot_o4(), units = "mm", device = "png", bg = "white",
             width = w(), height = h())
    }
  )
}


# Run the shiny application ----
shinyApp(ui = ui, server = server, options = list(launch.browser = T))




# theme_bw()
# theme_classic()
# theme_dark()

# d_width,etc
# I am a bit afraid this will override the rain.side arg 

ggplot(iris, aes(1, Sepal.Length)) +
  geom_rain(point.args.pos = rlang::list2(position = ggpp::position_jitternudge(width = 0.04, nudge.from = "jittered", seed = 42, x = -.2)),
            boxplot.args.pos = rlang::list2(width = 0.05, position = ggpp::position_dodgenudge(x = 0.1)),
            violin.args.pos = rlang::list2(side = "r", width = 0.7, position = position_nudge(x = .2)))
            

ggplot(iris, aes(1, Sepal.Length, fill = Species)) +
  geom_rain()


ggplot(iris, aes(Species, Sepal.Length, fill = Species)) +
  geom_rain()



