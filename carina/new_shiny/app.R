# Load libraries
library(shiny)
library(EpiEstim)
library(ggplot2)
library(incidence)
library(cluster.datasets)

# Define UI for application
ui <- fluidPage(navbarPage("WHO / PAHO",
                           tabPanel("First task",
                                    sidebarLayout(
                                        sidebarPanel(
                                            
                                            # Input: Select a file ----
                                            fileInput("file1", "Insert CSV File",
                                                      multiple = FALSE,
                                                      accept = c("text/csv",
                                                                 "text/comma-separated-values,text/plain",
                                                                 ".csv")),
                                            
                                            # Horizontal line ----
                                            tags$hr(),
                                            
                                            # Input: Checkbox if file has header ----
                                            checkboxInput(inputId = "header", label = "Header", value = TRUE),
                                            
                                            # Input: Select separator ----
                                            radioButtons(inputId = "sep", 
                                                         label = "Separator",
                                                         choices = c(Comma = ",",
                                                                     Semicolon = ";",
                                                                     Tab = "\t"),
                                                         selected = ","),
                                            
                                            # Input: Select quotes ----
                                            radioButtons(inputId = "quote", 
                                                         label = "Quote",
                                                         choices = c(None = "",
                                                                     "Double Quote" = '"',
                                                                     "Single Quote" = "'"),
                                                         selected = '"'),
                                            
                                            # Horizontal line ----
                                            tags$hr(),
                                            
                                            # Input: Select number of rows to display ----
                                            radioButtons(inputId = "disp", 
                                                         label = "Display",
                                                         choices = c(Head = "head",
                                                                     All = "all"),
                                                         selected = "head")
                                            
                                        ),
                                        
                                        mainPanel(
                                            tabsetPanel(
                                                tabPanel("Table",
                                                         # https://shiny.rstudio.com/gallery/mathjax.html
                                                         withMathJax(includeMarkdown("/Users/carinapeng/Harvard-WHO/carina/first_page.Rmd")),
                                                         tableOutput("content1")),
                                                tabPanel("Plot",
                                                         plotOutput("content2"),
                                                         downloadButton("downloadPlot", "Download plot")),
                                                tabPanel("Summary",
                                                         h3("Summary Statistics"),
                                                         verbatimTextOutput("content3"),
                                                         tableOutput("content4"),
                                                         downloadButton("downloadData", "Download summary statistics"))
                                            )))),
                           
                           
                           tabPanel(
                               "Second task",
                               sidebarPanel(
                                   "CovidSIM",
                                   numericInput("Dp", "Prodromal period [days]:", 2),
                                   numericInput("Di", "Early infective period [days]:", 5),
                                   numericInput("Dl", "Late infective period [days]:", 7),
                                   numericInput("Cp", "Relative contagiousness in the prodromal period:", 1),
                                   numericInput(
                                       "Cl",
                                       "Relative contagiousness in the late infective period:",
                                       0.05
                                   ),
                                   numericInput("Fsick", "Infections which will lead to sickness:", 0.67),
                                   numericInput("Fiso", "Probability that a sick patient is isolated:", 0.5),
                                   numericInput("Phome", "Contact reduction for cases in home isolation:", 0.75),
                                   actionButton("submitbutton",
                                                "submit",
                                                class = "btn btn-primary")
                                   
                               ),
                               mainPanel(
                                   tags$label(h3("Status/Output")),
                                   verbatimTextOutput("calculation"),
                                   tableOutput("tabledata")
                               )
)
))

server <- function(input, output, session){
    
    datasetInput <- reactive({
        
        impact_case_isolation <- (input$Cp*input$Dp+(1-input$Fsick*input$Fiso*input$Phome)*(input$Di+input$Cl*input$Dl))/(input$Cp*input$Dp+input$Di+input$Cl*input$Dl)
        impact_case_isolation <- data.frame(impact_case_isolation)
        names(impact_case_isolation) <- "BMI"
        print(impact_case_isolation)
        
    })
    
    output$calculation <- renderPrint({
        if(input$submitbutton>0) {
            isolate("Calculation complete.")
        } else{
            return("Server is ready for calculation.")
        }
    })
    
    output$tabledata <- renderTable({
        if(input$submitbutton>0) {
            isolate(datasetInput())
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
