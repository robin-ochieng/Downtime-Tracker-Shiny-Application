
# Server logic for data tables
downtimeTrackerServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
  observe({
    month_choices <- data$Month[!is.na(data$Month)] %>% unique()
    month_choices <- c("All" = "All", month_choices)
    quarter_choices <- data$Quarter[!is.na(data$Quarter)] %>% unique()
    quarter_choices <- c("All" = "All", quarter_choices)
    year_choices <- data$Year[!is.na(data$Year)] %>% unique()
    year_choices <- c("All" = "All", year_choices)

    updateSelectInput(session, "month", choices = month_choices, selected = "All")
    updateSelectInput(session, "quarter", choices = quarter_choices, selected = "All")
    updateSelectInput(session, "year", choices = year_choices, selected = "All")
  })



  # Reactive expression to filter the data based on selected month, quarter, and year
    filtered_data <- reactive({
      req(data)
      filtered <- data
      
      if (input$month != "All") {
        filtered <- filtered %>% filter(Month == input$month)
      }
      
      if (input$quarter != "All") {
        filtered <- filtered %>% filter(Quarter == input$quarter)
      }
      
      if (input$year != "All") {
        filtered <- filtered %>% filter(Year == as.numeric(input$year))
      }
      
      filtered
    })


#Premium-------------------------------------------------------------------------------------------------------------
    totalIncidents <- reactive({
      nrow(filtered_data())
    })

    averageDowntimeDuration <- reactive({
      mean(filtered_data()$`Downtime (Resolution Time)`, na.rm = TRUE)
    })   

    resolvedIssuesRate <- reactive({
      resolved <- sum(filtered_data()$Status == "Resolved", na.rm = TRUE)
      totalIssues <- nrow(filtered_data())
      rate <- (resolved / totalIssues) * 100  # Convert to percentage
      rate
    })

  output$totalIncidents <- renderValueBox({
    valueBox(
      comma(totalIncidents(), accuracy = 1), 
      subtitle = "Total Incidents",
      color = "white"
    )
  })

  output$averageDowntimeDuration <- renderValueBox({
    valueBox(
      comma(averageDowntimeDuration(), accuracy = 1), 
      subtitle = "Avg. Downtime (Days)",
      color = "white"
    )
  })

  output$resolvedIssuesRate <- renderValueBox({
    valueBox(
      value = paste0(comma(round(resolvedIssuesRate(), 1)), "%"),
      subtitle = "Issue Resolution Rate",
      color = "white"
    )
  })



# The plots-------------------------------------------------------------------------------------------------------------
    custom_colors_status <- c("#17a2b8", "#008b8b")  # Blue for resolved, red for unresolved
    output$statusBreakdown <- renderPlotly({
      data <- filtered_data()  # Load the necessary issues data
      # Filter out rows where Status might be NA
      data <- data[!is.na(data$Status), ]
      # Group data by 'Status' and count the number of issues
        status_counts <- data %>%
            group_by(Status) %>%
            dplyr::summarise(IssueCount = n(), .groups = 'drop')

      # Create the donut chart
      p <- plot_ly(status_counts, labels = ~Status, values = ~IssueCount, type = 'pie', hole = 0.4,
                  textposition = 'inside', 
                  textinfo = 'label+value+percent',  
                  insidetextorientation = 'tangential', 
                  marker = list(colors = custom_colors_status),
                  textfont = list(color = 'white', family = "Mulish", size = 12))

      # Add title and display the plot
      p <- p %>% layout(title = "Status Breakdown of Issues", showlegend = TRUE,
                        font = list(family = "Mulish"))
      p
    })

output$distributionOfIssues <- renderPlotly({
    req(filtered_data())
    
    data <- filtered_data()

    # Aggregate data to count issues
    aggregated_data <- aggregate(x = list(Count = data$Issue), by = list(Issue = data$Issue), FUN = length)

    # Sort data in descending order of count to order the bars by issue frequency
    aggregated_data <- aggregated_data[order(-aggregated_data$Count), ]

    # Creating the Plotly plot for horizontal bar chart
    plot <- plot_ly(
      aggregated_data,
      y = ~Issue,  # Swap x and y for horizontal bar
      x = ~Count,
      type = 'bar',
      orientation = 'h',  # Change orientation to horizontal
      text = ~paste0(format(round(Count, 0), big.mark = ","), " "),
      textposition = 'auto',
      hoverinfo = 'text',
      marker = list(color = '#48d1cc')
    ) %>%
      layout(
        title = "Distribution of Issues",
        yaxis = list(title = 'Issue Type', automargin = TRUE, tickfont = list(size = 10),
                     categoryorder = 'total ascending'),  # Ensure categories are ordered by totals
        xaxis = list(title = 'Number of Issues', title_standoff = 20, tickformat = ',.0f'),
        showlegend = FALSE,
        margin = list(t = 20, r = 25, b = 40, l = 100),  # Adjust margins to fit layout
        font = list(family = "Mulish", size = 12)
      )
    plot
})



output$avgDowntimeByIssue <- renderPlotly({
    req(filtered_data())  # Ensure data is available before proceeding
    
    # Convert the Downtime column to numeric for averaging
    data <- filtered_data()
    data$Downtime <- as.numeric(as.character(data$`Downtime (Resolution Time)`))
    data$Issue <- factor(data$Issue, levels = unique(data$Issue))  
    
    # Aggregate data by 'Issue' and calculate average downtime
    aggregated_data <- aggregate(Downtime ~ Issue, data = data, FUN = function(x) mean(x, na.rm = TRUE))

    # Rename the column for average downtime
    colnames(aggregated_data)[2] <- "AvgDowntime"

    # Filter out rows where AvgDowntime is greater than 0
    aggregated_data <- subset(aggregated_data, AvgDowntime > 0)

    # Sort the data in descending order of AvgDowntime (for descending order)
    aggregated_data <- aggregated_data[order(-aggregated_data$AvgDowntime), ]

    # Check if there is data to plot
    if (nrow(aggregated_data) > 0) {
        plot_ly(aggregated_data,
                y = ~Issue,
                x = ~AvgDowntime,
                type = 'bar',
                orientation = 'h',
                text = ~paste0(round(AvgDowntime, 2), " days"),
                textposition = 'auto',
                hoverinfo = 'text',
                marker = list(color = '#48d1cc')
        ) %>%
        layout(title = "Average Downtime Duration by Issue Type",
               xaxis = list(title = 'Average Downtime (Days)', tickformat = ',.2f'),
               yaxis = list(title = 'Issue Type', automargin = TRUE),
               showlegend = FALSE,
               margin = list(t = 40, r = 25, b = 40, l = 100),
               font = list(family = "Arial", size = 12)
        )
    } else {
        plot_ly() %>%
            layout(title = "No data available to display",
                   font = list(family = "Arial", size = 12)
            )
    }
})


    output$funnelPlot <- renderPlotly({
        # Assume data is obtained via a reactive expression or similar
        req(filtered_data())
        data <- filtered_data()

        # Aggregate to find count of issues by attributable cause
        cause_data <- data %>%
          group_by(`Attributable Cause`) %>%
          dplyr::summarise(Issue_Count = n(), .groups = 'drop') %>%
          arrange(desc(Issue_Count))  # Arrange might be unnecessary for a funnel plot but helps ordering

        # Creating the funnel plot
        plot <- plot_ly(
          cause_data,
          x = ~Issue_Count,
          y = ~fct_inorder(`Attributable Cause`),  # Ensures the plot follows the order of appearance which is arranged by count
          type = 'bar',
          orientation = 'h',
          marker = list(color = ~Issue_Count, colorscale = 'Blues')  # Color can be adjusted as needed
        ) %>%
        layout(
          title = "Funnel Plot of Attributable Causes",
          xaxis = list(title = 'Number of Issues'),
          yaxis = list(title = 'Attributable Cause'),
          margin = list(l = 150)  # Left margin, adjust as necessary to fit y-axis labels
        )
        plot
    })



  })
}




 