#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(data.table)
library(DT)
library(shinyalert)

header = dashboardPage(skin = 'purple',
  
  dashboardHeader(title = "YOLO"),
  
  sidebar =  dashboardSidebar(
    tags$style(".well {background-color:'green';}"),
    sidebarMenu( id = "items",
                 menuItem("Introduction", tabName = "introduction", icon = icon("fa fa-info-circle")),
                 menuItem("Preprocess BIOM", tabName = "PrePrBIOM", icon = icon("fa fa-info-circle")),
                 menuItem("PCA Analysis", tabName = "PCA", icon = icon("fa fa-info-circle")),
                 menuItem("Prepare for ML", tabName = "prepML", icon = icon("fa fa-info-circle")),
                 menuItem("Machine Learning", tabName = "MLearning", icon = icon("fa fa-info-circle")),
                 menuItem("TeamYOLO", tabName = "Team", icon = icon("fa fa-info-circle" ))
                 # menuItem("Downloads", tabName = "Downloads", icon = icon("fa fa-info-circle"),
                 #          # list(
                 #          #   downloadButton('randdownload', 'Randomized Data'),
                 #          #   br(),
                 #          #   br(),
                 #          #   downloadButton("finaldownload",'Final Design'),
                 #          #   
                 #          #   br(),
                 #          #   br(),
                 #            
                 #            downloadButton("plotdownload",'Plot')
                 #          # )
                 # )
    )),
  
  body = dashboardBody(
    
    tags$head(tags$style(
      type="text/css",
      "logo img {max-width: 100%; width: 100%; height: auto}"
    )),
    conditionalPanel(
      condition = "input.items == 'introduction'",
      
      h2(strong("From Team YOLO", style = "color:gray")),
      
      # img(src='Prediction.png', align = "center"),
      br(),
      # imageOutput("Prediction.png"),
      
      # img(src='Prediction.png', align = "center"),
      h3(p("Studies have revealed significant diversity in the gut microbiome composition 
           related to various phenotypes. For example, studies of lean and obese mice 
           suggest a strong relationship between gut microbiome and obesity. 
           Obesity has been associated with changes in the microbiota at phylum-level, 
           reduction in bacterial diversity, and different representations of bacterial genes. 
           There also have been some reports of a strong association between the skin/gut microbiome 
           and aging.")),
      h3(p("We aim to desing a pipeline to analyze microbiome OTU base data using machine learning. 
           The pipleline has three different steps. Data was in the form of OTU table was downloaded
           from ", tags$a(href = "http://www.earthmicrobiome.org", "Earth Microbiome Project"),  "published in", 
           tags$a(href = "https://www.nature.com/articles/nature07540", "Nature!"))),
      
      tags$ol(
        h4(tags$li("Preprocessing OTU data to make it ready for PCA analysis and machine learning",
                   tags$ul(
                   tags$li(
                   "BIOM file format is widely used and is designed to be a general-use format for 
                           representing biological sample by observation contingency tables. 
                           BIOM is a recognized standard for the Earth Microbiome Project and 
                           is a Genomics Standards Consortium supported project")))), 
        h4(tags$li("Perform PCA analysis e.g. based on age, sex etc",
                   tags$ul(
                     tags$li(
                       "Principal component analysis is a statistical procedure that uses an 
                       orthogonal transformation to convert a set of observations of possibly 
                       correlated variables into a set of values of linearly uncorrelated 
                       variables called principal components"
                     )
                   ))), 
        h4(tags$li("Machine Learning to predict response variable e.g disease class etc",
                   tags$ul(
                     tags$li(
                       "Machine learning is the scientific study of algorithms and statistical 
                       models that computer systems use to perform a specific task without 
                       using explicit instructions, relying on patterns and inference instead. 
                       It is seen as a subset of artificial intelligence"
                     )
                   ))
      )),
      
      br(),
      br(),
    ),
    
    conditionalPanel(
      condition = "input.items == 'Team'",
      h3(tags$b("Team YOLO"),
         tags$ol(
           tags$li(
             "Agaz Wani"),
           tags$li(
             "Alex Dean"),
           tags$li(
             "Chang L"),
           tags$li(
             "Nathan Van Bidder"
           ),
           tags$li(
             "Peter Radulovic"
           ),
           tags$li(
             "Yibo Dong"
           ))),
      
      h3(tags$b("Consulting Team:"),
         tags$ol(
           tags$li(
             "Dr. Greg Herbert,"),
           tags$li(
             "Dr. Xiaoming Liu")
         )),
      br(),
      sliderInput("slider", label = "Image Magnifier", min = 100, max = 2000, value = "NULL"),
      uiOutput("pic1"),
      uiOutput("pic2"),
      uiOutput("pic3"),
      uiOutput("pic4"),
      
      ),
      
      
    
    # conditionalPanel(
    #   condition = "input.items == 'help'",
    #   h2(strong("Tutorial", style = "color:gray")),
    #   h4(p("Sample data and tutorial is available to download.")
    #   ),
    #   
    #   br(),
    #   
    #   downloadButton("sampledata",'Sample Data'),
    #   
    #   downloadButton("TutorialDownl",'Tutorial'),
    #   
    #   br(),
    #   br(),
    #   h5(p(" In case of any issue, it can be raised at", 
    #        a("Github page issues", href = "https://github.com/uddin-research-group-at-usf/RANDOMIZE/issues"),".",
    #        "Additional queries can be sent to ahwani@usf.edu."))
    # ),
    
    conditionalPanel(

      condition = "input.items == 'visualize' | input.items = 'shuffle'",

      fluidRow(
          uiOutput("Data"),
          uiOutput("loaddata")
      ),
      fluidRow(
        uiOutput("ControlLOC"),
        useShinyalert(),
        uiOutput("Plots"),
        uiOutput("PlotLabels"),
        uiOutput('Display_final_design'),
        uiOutput("Divide"),
        br(),
        uiOutput("Download"),
        uiOutput("DownloadPCA"),
        uiOutput("MLParameters"),
        # downloadButton("plotdownload",'Plot'),

        br(),

      ),
      fluidRow(
        column(6,div(style = "height:200px", "")))

    )
  )
)
  

