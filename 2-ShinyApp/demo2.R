ui <- fluidPage(
  titlePanel("title panel"),
  
  sidebarLayout(
    sidebarPanel("sidebar panel"),
    mainPanel("main panel",       p("p creates a paragraph of text."),
              p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
              strong("strong() makes bold text."),
              em("em() creates italicized (i.e, emphasized) text."),
              br(),
              code("code displays your text similar to computer code"),
              div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
              br(),
              p("span does the same thing as div, but it works with",
                span("groups of words", style = "color:blue"),
                "that appear inside a paragraph."),
              h1('In a long time ago'),
              h2('in a galaxy'),
              h3('far, far'),
              h4('away'),
              h5('...')
    )
  )
)

# Define server logic ----
server <- function(input, output) {
}

# Run the app ----
shinyApp(ui = ui, server = server)