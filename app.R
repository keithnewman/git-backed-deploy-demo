library(shiny)
library(bslib)
library(ggplot2)

# UI
ui <- page_sidebar(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#3498db",
    base_font = font_google("Inter")
  ),
  title = "Data Explorer Demo",
  
  sidebar = sidebar(
    title = "Controls",
    selectInput(
      "dataset",
      "Choose Dataset:",
      choices = c("mtcars", "iris", "faithful")
    ),
    sliderInput(
      "n_points",
      "Number of Points:",
      min = 10,
      max = 100,
      value = 50,
      step = 5
    ),
    checkboxInput(
      "show_smooth",
      "Show Trend Line",
      value = TRUE
    )
  ),
  
  # Main content
  card(
    card_header("Dataset Overview"),
    verbatimTextOutput("summary")
  ),
  
  card(
    card_header("Visualization"),
    plotOutput("plot", height = "400px")
  ),

  card(
    card_header("Wisdom from Hadley"),
    tags$blockquote(textOutput("hadleyQuote")),
    actionButton("newQuote", "Get more wisdom")
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive dataset selection
  selected_data <- reactive({
    data <- switch(input$dataset,
                   "mtcars" = mtcars,
                   "iris" = iris,
                   "faithful" = faithful)
    
    # Limit rows based on slider
    n <- min(input$n_points, nrow(data))
    cli::cli_alert_info("Data changed to display {n} data point{?s} from {input$dataset}.")
    data[1:n, ]
  })
  
  # Summary output
  output$summary <- renderPrint({
    summary(selected_data())
  })
  
  # Plot output
  output$plot <- renderPlot({
    data <- selected_data()
    
    # Create different plots based on dataset
    p <- if (input$dataset == "mtcars") {
      ggplot(data, aes(x = wt, y = mpg)) +
        geom_point(color = "#3498db", size = 3, alpha = 0.7) +
        labs(title = "MPG vs Weight", x = "Weight (1000 lbs)", y = "Miles per Gallon")
    } else if (input$dataset == "iris") {
      ggplot(data, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
        geom_point(size = 3, alpha = 0.7) +
        labs(title = "Iris Sepal Dimensions", x = "Sepal Length", y = "Sepal Width")
    } else {
      ggplot(data, aes(x = waiting, y = eruptions)) +
        geom_point(color = "#3498db", size = 3, alpha = 0.7) +
        labs(title = "Old Faithful Eruptions", x = "Waiting Time (min)", y = "Eruption Time (min)")
    }
    
    # Add smooth line if selected
    if (input$show_smooth && input$dataset != "iris") {
      p <- p + geom_smooth(method = "lm", se = TRUE, color = "#e74c3c")
    }
    
    p + theme_minimal(base_size = 14)
  })

  # Quote from Hadley
  output$hadleyQuote <- renderText({
    # TODO: Obtain a quote from the Hadley API.
    # Bonus points if we can display this message when we get a response status that is not 200
    "Sorry, the Hadley API is unavailable. Please try again later."
  }) |>
    bindEvent(input$newQuote, ignoreNULL = FALSE)
}

# Run the app
shinyApp(ui = ui, server = server)
