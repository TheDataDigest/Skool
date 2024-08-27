```{r, message=FALSE, echo = FALSE}

# User Interface
ui <- fluidPage(
  # Dropdown for selecting a search term
  selectInput("selected_term", "Select a Search Term:",
              choices = unique(as.character(multi_df_paid_USD$group)),
              selected = unique(as.character(multi_df_paid_USD$group))[1]),
  
  # Plotly output
  plotlyOutput("scatterplot")
)

# Server logic
server <- function(input, output) {
  # Reactive expression to filter data based on selected search term
  filtered_data <- reactive({
    multi_df_paid_USD %>%
      filter(group == input$selected_term)
  })
  
  # Render the scatterplot
  output$scatterplot <- renderPlotly({
    plot_ly(data = filtered_data(),
            x = ~log10(members_n),
            y = ~log10(price),
            type = "scatter",
            mode = "markers",
            text = ~paste("Community:", community, "<br>",
                          "Members:", members_n, "<br>",
                          "Price:", price),  # Hover text
            hoverinfo = "text") %>%
      layout(
        title = "Scatterplot of log10(members_n) vs. log10(price)",
        xaxis = list(
          title = "Members (log10)",
          tickvals = log10(c(10, 50, 100, 500, 1000, 10000, 100000, 200000)),
          ticktext = c("10", "50", "100", "500", "1k", "10k", "100k", "200k")
        ),
        yaxis = list(
          title = "Price in USD (log10)",
          tickvals = log10(c(10, 50, 100, 500, 1000)),
          ticktext = c("$10", "$50", "$100", "$500", "$1k"),
          tickformat = "$~s"  # Format as dollar with scale
        )
      )
  })
}

# Create the Shiny app
shinyApp(ui, server)
```