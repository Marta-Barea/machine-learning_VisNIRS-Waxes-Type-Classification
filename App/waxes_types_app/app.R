# This is a Shiny web application. You can run the application by clicking the 
# 'Run App' button above.

# Loading required packages ----

library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(randomForest)
library(rJava)
library(xlsx)
library(xlsxjars)
library(openxlsx)
library(data.table)
library(stringr)
library(prospectr)
library(caret)

# Loading saved model ----

model <- readRDS("rf.rds") 

# Server ----
addResourcePath("static", "static")

server <- function(input, output) {
  
  # Create a table output element
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    
    df <- if(stringr::str_ends(input$file1$datapath, "csv")) {
      read.csv(input$file1$datapath,
               eader = input$header,
               sep = input$sep,
               quote = input$quote)
    } 
    else if (stringr::str_ends(input$file1$datapath, "(xlsx|xls)")) {
      openxlsx::read.xlsx(input$file1$datapath, 
                         rowNames = input$header,
                         sheet = as.numeric(input$sheet))
    }
    df <- df[,unique(names(df))]
  }
)
  
  contents1 <- reactive({
    
    req(input$file1)
    
    df <- if(stringr::str_ends(input$file1$datapath, "csv")) {
      read.csv(input$file1$datapath,
               eader = input$header,
               sep = input$sep,
               quote = input$quote)
    } 
    else if (stringr::str_ends(input$file1$datapath, "(xlsx|xls)")) {
      openxlsx::read.xlsx(input$file1$datapath, 
                          rowNames = input$header,
                          sheet = as.numeric(input$sheet))
    }
    df <- df[,unique(names(df))]
    
    # First derivative with Savitzky-Golay Filter
    df_sg <- savitzkyGolay(X = df, p = 3, w = 11, m = 1)
    
    
    # Make predictions with the ML model
    predictions <- cbind.data.frame(`Sample` = rownames(df_sg),`Wax Type`= predict(model, df_sg))
    print(predictions)
  }
)
  
  # Prediction results table
  output$predictions <- renderTable({
    if (input$submitbutton > 0) { 
      isolate(contents1()) 
    } 
  })
  
  # Test data
  
  data <- openxlsx::read.xlsx("test_data.xlsx", 
                              sheet = 1)
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(paste0("test_data", Sys.Date(), sep = ""),".xlsx",sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(data, file)
    }
  )
}
# User Interface ----

# Add icons along with the title in the shinydashboard header

title_uca <- tags$a(href = 'https://www.uca.es/',
                    tags$img(src = "static/logo-uca.jpg", 
                             height = '50',
                             width = '40'),
                    "University of Cadiz", 
                    target = "_blank")

title_ivagro <- tags$a(href = 'https://ivagro.uca.es/',
                       tags$img(src = "static/logo-ivagro.jpg", 
                                height = '50',
                                width = '40'),
                       "Viticulture and Agri-Food Research Institute", 
                       target = "_blank")

ui <- fluidPage(
  
  # Logos
  titlePanel(fluidRow(column(3, title_uca),column(9, title_ivagro))),
  
  # Theme 
  theme = shinytheme("sandstone"),
  
  # Navigation bar
  navbarPage("Petroleum Wax Types"),
  
  # Input: File type and preprocessing instructions 
  helpText(style = "text-align: justify;", "This Shiny App allows the predictions of 
           the common types of petroleum waxes: macrocrystalline (macro_wax) and microcrystalline (micro_wax) waxes. The predictions are performed using a predictive model based on the RF algorithm."),
  
 # Dashboard
 
 # Social media 
 dashboardHeader(
   title = "", 
   tags$li(class = "dropdown", 
           tags$a(href = 'https://github.com/Marta-Barea',
                  "Source Code",
                  icon("github"),
                  target = "_blank")
           ),
   tags$li(class = "dropdown", 
           tags$a(href = 'https://orcid.org/0000-0003-3508-6038',
                  "Orcid",
                  icon("orcid"),
                  target = "_blank")
   ),
   tags$li(class = "dropdown", 
           tags$a(href = 'https://www.researchgate.net/profile/Marta-Barea-Sepulveda',
                  "ResearchGate",
                  icon("researchgate"),
                  target = "_blank")
   )
 ),

  # Title: Data upload title
  titlePanel("Upload Data"),
  
  # Sidebar layout with input and output definitions 
  sidebarLayout(
    
    # Sidebar panel for inputs 
    sidebarPanel(
      
      # Input: Download test set
      
      downloadButton("downloadData", label = "Download"),
      
      # Input: Download button instructions 
      helpText("Clicking on the 'Download' button will automatically download a test 
               dataset for using the App."),
      
      # Horizontal line 
      tags$hr(),
      
      # Input: Select a file 
      fileInput("file1", "File Input:",
                multiple = FALSE,
                accept = c("text/csv", "xlsx/xls",
                           "text/comma-separated-values,text/plain",
                           ".csv",
                           ".xlsx",
                           ".xls")),
      
      # Input: File type and preprocessing instructions 
      helpText(style = "text-align: justify;", "This App supports uploading files in .csv/txt and .xlsx/xls formats."),
            
      # Horizontal line 
      tags$hr(),
      
      # Submit data
      actionButton("submitbutton", "Submit"),
      
      # Input: File type instructions 
      helpText("Please click on the submit button once the data file has been uploaded."),

      # Horizontal line 
      tags$hr(),
      
      # Input: Checkbox if file has header 
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator 
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select sheet number 
      radioButtons("sheet", "Sheet",
                   choices = c("Sheet 1" = 1,
                               "Sheet 2" = 2,
                               "Sheet 3" = 3),
                   selected = 1),
      
      # Input: Select quotes 
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line
      tags$hr(),
    
    # Input: Select number of rows to display and submit button 
    radioButtons("disp", "Display",
                 choices = c(Head = "head",
                             All = "all"),
                 selected = "head")
    ),
  
    # Main panel for displaying outputs 
    mainPanel(
      
      #Output: Text
      tags$label(h5('• The type of petroleum wax is:')),
    
      # Output: Predictions
      tableOutput("predictions")),
    )
  )

# Run the application 
shinyApp(ui = ui, server = server)