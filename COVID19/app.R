library(EpiEstim)
library(ggplot2)
library(incidence)
library(markdown)
library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("WHO-PAHO"),
  
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
                               All = "all",
                               Tail = "tail"
                               ),
                   selected = "tail")
      
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
                           downloadButton("downloadData2", "Download Summary Statistics of Transmission Rates")),
                  tabPanel("Public Policy Analysis",
                           h3("Estimating the Impact of Public Health Measures on COVID-19 Transmission as Modeled in the CovidSIM Interface"),
                           withMathJax(includeMarkdown("policy.md")),
                           h3("Period Duration Parameters (Days)"),
                           fluidRow(
                                    column(3,

                                           # Copy the line below to make a slider bar 
                                           sliderInput("Dp", label = h4("Prodromal Period (Days)"), min = 0, 
                                                       max = 31, value = 2)
                                           ),
                                    column(3,

                                           # Copy the line below to make a slider range 
                                           sliderInput("Di", label = h4("Early Infective (Days)"), min = 0, 
                                                       max = 31, value = 5)
                                    ),
                                    column(3,

                                           # Copy the line below to make a slider range 
                                           sliderInput("Dl", label = h4("Late Infective (Days)"), min = 0, 
                                                       max = 31, value = 7)
                                    )
                                    ),
                           h3("Relative Contagiousness Parameters (%)"),
                           fluidRow(
                                    column(3,

                                           # Copy the line below to make a slider bar 
                                           sliderInput("Cp", label = h4("Prodromal (%)"), min = 0, 
                                                       max = 1, value = 1)
                                           ),
                                    column(3,

                                           # Copy the line below to make a slider range 
                                           sliderInput("Cl", label = h4("Early Infective (%)"), min = 0, 
                                                       max = 1, value = 0.05)
                                    ),
                                    column(3,

                                           # Copy the line below to make a slider range 
                                           sliderInput("R0", label = h4("Initial Rate of Infection"), min = 0, 
                                                       max = 10, value = 3.7, step=0.1)
                                    )
                                    ),
                           h3("Transmission Parameters"),
                           fluidRow(
                                    column(3,

                                           # Copy the line below to make a slider bar 
                                           sliderInput("Fsick", label = h4("Infections Which Will Lead to Sickness (%)"), min = 0, 
                                                       max = 1, value = 0.67),
                                           ),
                                    column(3,

                                           # Copy the line below to make a slider range 
                                           sliderInput("Fiso", label = h4("Probability Sick Person is Isolated (%)"), min = 0, 
                                                       max = 1, value = 0.5)
                                    ),
                                    column(3,

                                           # Copy the line below to make a slider range 
                                           sliderInput("Phome", label = h4("Contact Reduction for Isolated Cases (%)"), min = 0, 
                                                       max = 1, value = 0.75)
                                    ),
                                    ),
                           h3("Impact of Isolating Cases Only"),
                           verbatimTextOutput("impactcaseisolation"),
                           h3("Impact of Contact Reduction Policies on the Transmission"),
                           verbatimTextOutput("impactcontactreduction"),
                           h3("Percentage of General Contact Reduction (to be used in CovidSIM)"),
                           verbatimTextOutput("pctcontactreduction")
                  )
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
    if(input$disp == "tail") {
      return(tail(csv()))
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
    Rt <- Rt[c(1:4, 8)]
    if(input$disp == "head") {
      return(head(Rt))
    }
    if(input$disp == "tail") {
      return(tail(Rt))
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
    return(writeLines(c("The current reproductive number (R) is estimated to be", round(x[length(x)],digits=2))))
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

    contact <- reactive({
        impact_case_isolation<-(input$Cp*input$Dp+(1-input$Fsick*input$Fiso*input$Phome)*(input$Di+input$Cl*input$Dl))/(input$Cp*input$Dp+input$Di+input$Cl*input$Dl)
        return(impact_case_isolation)
    })

  output$impactcaseisolation <- renderPrint({
        # impact_case_isolation<-(input$Cp*input$Dp+(1-input$Fsick*input$Fiso*input$Phome)*(input$Di+input$Cl*input$Dl))/(input$Cp*input$Dp+input$Di+input$Cl*input$Dl)
        y <- contact()
      # return(writeLines("Case Isolation Number Obtained", contact()))
      return(writeLines(c("Case Isolation Number Obtained",y)))
       # return(y)
      # return(impact_case_isolation)
  })
  output$impactcontactreduction <- renderPrint({
      x <- df()$R$Mean
      Rt_observed <- x[length(x)]
      impact_contact_reduction <- Rt_observed / (input$R0 * contact())
    return(writeLines(c("The Reduction of COVID-19 Cases is estimated to be", round(impact_contact_reduction,digits=2))))
     # return(, x[length(x)])
  })  
  output$pctcontactreduction <- renderPrint({
      x <- df()$R$Mean
      Rt_observed <- x[length(x)]
      impact_contact_reduction <- (1 - Rt_observed / (input$R0 * contact()))*100
    return(writeLines(c("Percent Reduction of COVID-19 Cases is estimated to be", round(impact_contact_reduction,digits=2), "Percent")))
     # return(, x[length(x)])
  })  
}

# Create Shiny app ----
shinyApp(ui, server)


