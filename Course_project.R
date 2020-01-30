library(shiny)
library(gapminder)
library(ggplot2)

ui <- fluidPage(
  h1("Gapminder"),
  # Create a container for tab panels
  tabsetPanel(
    # Create "User Guide" tab
    tabPanel(
      title = "User Guide",
      h1("the Gapminder Dataset"),
      p("This is an app that shows the gapminder dataset. The gapminder dataset contains the life expectancy per
        year for different countries."),
      h1("the App"),
      p("The app contains 4 tabs. First is a user guide tab. Second is an input tab. It takes the expectancy life estimate from
        the user and also if he wants to filter out the continent. This filtering will be applied on the gapminder
        dataset. The download button will allow the user to download the file resulting from the filtering. The plot tab
        will scatter plot the GdpPerCapital vs life_expectancy. The table tab will show the dataset resulting from the filtering
        .The data is constructed with the DT package, which makes me very convenient to show")
      ),
    # Create an "Inputs" tab
    tabPanel(
      title = "Inputs",
      sliderInput(inputId = "life", label = "Life expectancy",
                  min = 0, max = 120,
                  value = c(30, 50)),
      selectInput("continent", "Continent",
                  choices = c("All", levels(gapminder$continent))),
      downloadButton("download_data")
    ),
    # Create a "Plot" tab
    tabPanel(
      title = "Plot",
      plotOutput("plot")
    ),
    # Create "Table" tab
    tabPanel(
      title = "Table",
      DT::dataTableOutput("table")
    )
    
      )
      )

server <- function(input, output) {
  filtered_data <- reactive({
    data <- gapminder
    data <- subset(
      data,
      lifeExp >= input$life[1] & lifeExp <= input$life[2]
    )
    if (input$continent != "All") {
      data <- subset(
        data,
        continent == input$continent
      )
    }
    data
  })
  
  output$table <- DT::renderDataTable({
    data <- filtered_data()
    data
  })
  
  output$download_data <- downloadHandler(
    filename = "gapminder_data.csv",
    content = function(file) {
      data <- filtered_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$plot <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(gdpPercap, lifeExp)) +
      geom_point() +
      scale_x_log10()
  })
}

shinyApp(ui, server)