library(EpiEstim)
library(ggplot2)
library(incidence)
library(markdown)
library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("WHO / PAHO"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      helpText("Toggle Settings for uploading CSV"),
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      helpText("Toggle Settings for viewing results"),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tabsetPanel( #type = "tabs",
                  tabPanel("Welcome", 
                           withMathJax(includeMarkdown("Modeling-COVID19.md")),
                           downloadButton("downloadData", "Download Sample COVID-19 CSV File"),
                           h3(textOutput("contents1")),
                           tableOutput("contents")
                           ),
                  tabPanel("Graphs", 
                           plotOutput("contents3"),
                           plotOutput("contents4")
                           ),
                  tabPanel("Statistics", 
                           h3("Summary Statistics"),
                           verbatimTextOutput("contents5"),
                           tableOutput("contents2"),
                           downloadButton("downloadData2", "Download Summary Statistics of Transmission Rates"))
      )
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) { 
    csv <- reactive({
        req(input$file1)
           read.csv(input$file1$datapath,
                 header = input$header,
                 sep = input$sep,
                 quote = input$quote)
    })

    df <- reactive({
        req(input$file1)
        x = csv()
        x[,1]<-as.Date(x[,1], "%d/%m/%Y")
        dfR <- estimate_R(x, method = "parametric_si", config = make_config(list(mean_si = 4.8, std_si = 2.3)))
        return(dfR)
    })

    plt <- function(){
        req(input$file1)
        x = read.csv(input$file1$datapath,
                 header = input$header,
                 sep = input$sep,
                 quote = input$quote)
        x[,1]<-as.Date(x[,1], "%d/%m/%Y")
        dfR <- estimate_R(x, method = "parametric_si", config = make_config(list(mean_si = 4.8, std_si = 2.3)))
        return(plot(dfR, what=c("incid")))
    }

  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
      if (is.null(csv())) {
          return(NULL)
      }
    
    if(input$disp == "head") {
      return(head(csv()))
    }
    else {
      return(csv())
    }
    
  })

  output$contents1 <- renderPrint({
      if (is.null(csv())) {
          return(NULL)
      }
      return(writeLines("Uploaded File"))
  })
  
  output$contents2 <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
      if (is.null(csv()) || is.null(df()) ) {
          return(NULL)
      }

    Rt <- df()$R
    if(input$disp == "head") {
      return(head(Rt))
    }
    else {
      return(Rt)
    }
  })


  output$contents3 <- renderPlot({
    plot(df(), what=c("incid"))
  })
  output$contents4 <- renderPlot({
    plot(df(), what=c("R"))
  })
  output$contents5 <- renderPrint({
      x <- df()$R$Mean
    return(writeLines(c("The current rate of transmission is estimated to be", x[length(x)], "people per day")))
     # return(, x[length(x)])
  })  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename =  "COVID19-Cases.csv",
    content = function(file) {
    sample <- read.csv("sample.csv")
      write.csv(sample, file, row.names = FALSE)
    }
  )

  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("summary-", input$file1, sep = "")
    },
    content = function(file) {
    Rt <- df()$R
      write.csv(Rt, file, row.names = FALSE)
    })
}

# Create Shiny app ----
shinyApp(ui, server)


