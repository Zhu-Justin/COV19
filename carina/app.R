
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
                         downloadButton("downloadData", "Download summary statistics")
                         )
                
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
        # Subset first column and convert to dates
        x[,1] <- as.Date(x[,1], "%d/%m/%y")
        dfR <- estimate_R(x, 
                          method = "parametric_si", 
                          config = make_config(list(
                              mean_si = 4.8, 
                              std_si = 2.3)))
        return(dfR)
    })
    
    output$content1 <- renderTable({
        
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
    output$content2 <- renderPlot({
        
        plot(df())
        
    })
    
    output$content3 <- renderText({
        x <- df()$R$Mean
        # what is x[length(x)]?
        paste("The current effective reproductive number is estimated to be", round(x[length(x)],digits=2))
        
    })
    
    output$content4 <- renderTable({
        
        if (is.null(csv()) || is.null(df())){
            return(NULL)
        }
        else (
            return(df()$R)
        )
        
        if(input$disp == "head") {
            return(head(df()$R))
        }
        else {
            return(df()$R)
        }
    })
    
    output$downloadPlot <- downloadHandler(
        filename = function(){
            paste("R_plot", "png", sep = ".")
        },
        
        content = function(file){
            png(file)
            plot(df())
            dev.off()
        }
    )
    
    output$downloadData <- downloadHandler(
        filename = function(){
            paste("summary-statistics", "csv", sep = ".")
        },
        
        content = function(file){
            write.csv(df()$R, file)
        }
    )

}

# Run the application 
shinyApp(ui = ui, server = server)