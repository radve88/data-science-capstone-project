

suppressWarnings(library(shiny))
suppressWarnings(library(markdown))
shinyUI(navbarPage(
  "Coursera Data Science Capstone: Course Project",
  tabPanel(
    "Word Prediction Application",
    img(src = "./logo.png"),
    br(),
    HTML("<strong>Author: Rashmikant Dave</strong>"),
    br(),
    HTML("<strong>Date: 12 December 2016</strong>"),
    br(),
    
    sidebarLayout(
      sidebarPanel(
        helpText("Enter Word or Phrase"),
        textInput("inputString", "Please Enter Your Text Here", value = ""),
        br(),
        br(),
        br()
        
      ),
      mainPanel(
        h2("Predicted Next Word"),
        verbatimTextOutput("prediction"),
        strong("Sentence Input:"),
        tags$style(
          type = 'text/css',
          '#text1 {background-color: rgba(255,200,0,0.40); color: red;}'
        ),
        textOutput('text1'),
        br(),
        strong("N-Gram Used"),
        tags$style(
          type = 'text/css',
          '#text2 {background-color: rgba(255,255,0,0.40); color: black;}'
        ),
        textOutput('text2')
      )
    )
    
  ),
  tabPanel("Information",
           mainPanel(
             img(src = "./logo.png"),
             includeMarkdown("info.md")
           ))
))