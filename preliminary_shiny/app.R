
# Load libraries
library(shiny)
library(EpiEstim)
library(ggplot2)
library(incidence)
library(cluster.datasets)


# Define UI for application
ui <- fluidPage(
    
    # Application title
    titlePanel("WHO / PAHO"),
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
                         includeMarkdown("Modeling-COVID19.md"),
                         tableOutput("contents")),
                tabPanel("Plot", plotOutput("plot")),
                tabPanel("Summary", verbatimTextOutput("Summary"))
                
            )
    
)))

# Define server logic required to read data and produce plot
server <- function(input, output) {
    
    csv <- reactive({
        req(input$file1)
        read.csv(input$file1$datapath,
                 header = input$header,
                 sep = input$sep,
                 quote = input$quote)
    })
    
    # Apply parametric_si to the uploaded CSV input
    df <- reactive({
        req(input$file1)
        x = csv()
        x[,1]<-as.Date(x[,1], "%d/%m/%Y")
        dfR <- estimate_R(x, 
                          method = "parametric_si", 
                          config = make_config(list(
                              mean_si = 4.8, 
                              std_si = 2.3)))
        return(dfR)
    })
    
    output$contents <- renderTable({
        
        req(input$file1)
        
        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
        
    })
    
    
    # Plotting with original EpiEstim plots
    output$plot <- renderPlot({
        
        plot(df())
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)