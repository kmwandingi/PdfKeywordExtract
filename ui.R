library(shinyFiles)
library(shinycssloaders)
library(dplyr)

ui <- shinyUI(fluidPage(
  navbarPage("Keyword Extraction Application",
  tabPanel("Multiple", 
    sidebarLayout(
    sidebarPanel(
      wellPanel(shinyDirButton("dir", "Select A Directory", title = "Upload"), icon = icon("align-justify")),
      p(" "),
      p("The program will look through a directory, upload all pdfs and/or docxs contained, 
        parses through the pdfs to pick out keywords against words that describe them in each file. 
        The list of keywords and adjectives is pre-loaded in the keywords csv file. 
        The file can be edited to suit ones needs before running the program.")
    ),
    mainPanel(
      #div(DT::dataTableOutput("phrases"), style = "font-size: 65%; width: 100%"),
      div(DT::dataTableOutput("drkwclass")%>% withSpinner(color="#0dc5c1"), style = "font-size: 70%; width: 100%") 
     )
    )
   ),
  tabPanel("Single",
   sidebarLayout(
     sidebarPanel(
       wellPanel(fileInput('file1', 'Upload A Document', accept = c('.pdf'))
        ),
       p(" "),
       p("The program will look through the uploaded pdf,
        parses through it to pick out keywords against words that describe them in each pdf.
         The list of keywords and adjectives is pre-loaded in the keywords csv file. 
        The file can be edited to suit ones needs before running the program.")),
       mainPanel(
         #div(DT::dataTableOutput("phrases"), style = "font-size: 65%; width: 100%"),
          div(DT::dataTableOutput("kwclass"), style = "font-size: 85%; width: 100%"))
      )
    ),
  tabPanel("Custom", 
   sidebarLayout(
     sidebarPanel(
       wellPanel(
         fileInput('file2', 'Upload A Document', accept = c('.pdf')),
         textInput('words', "Enter Your Keywords Here",""), 
         actionButton("submit","Submit")
        ),
       p(" "),
       p("Upload a pdf and enter the keywords you want to search for seperated by commas. 
         First word is your keyword and the following words are ones you want to see if 
         are connected to your keyword.")),
      mainPanel(
       div(DT::dataTableOutput("slfkwclass"), style = "width: 100%" )
        )
      )
    )
   )
  )
)