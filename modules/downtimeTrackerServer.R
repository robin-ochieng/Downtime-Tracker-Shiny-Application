
# Server logic for data tables
downtimeTrackerServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
  observe({
    # Define month order
    month_choices <- data$Month[!is.na(data$Month)] %>% unique() %>% factor(levels = month.name)
    month_choices <- levels(month_choices)
    month_choices <- c("All" = "All", month_choices)
    quarter_choices <- data$Quarter[!is.na(data$Quarter)] %>% unique()
    quarter_choices <- c("All" = "All", quarter_choices)
    year_choices <- data$Year[!is.na(data$Year)] %>% unique() %>% sort()
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
      subtitle = "Avg. Downtime (Hours)",
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
    output$Issues_over_time <- renderPlotly({
      data <- filtered_data() %>%
        mutate(Date = as.Date(Date)) %>%
        group_by(Date) %>%
        dplyr::summarize(IssuesCount = n()) %>%  # Count of sales per day
        arrange(Date)
      
      plot_ly(data, x = ~Date, y = ~IssuesCount, type = 'scatter', mode = 'lines+markers',
              line = list(color = '#1CA4F8'), marker = list(color = '#0d6efd')) %>%
        layout(
          title = "Issues Count Over Time",
          xaxis = list(title = "Date", tickfont = list(size = 10, color = "#333333")),
          yaxis = list(title = "Count of Issues", tickfont = list(size = 10, color = "#333333")),
          font = list(family = "Mulish", color = "#333333"),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })


    output$averageResolutionTimeOverTime <- renderPlotly({
      data <- filtered_data() %>%
        mutate(Date = as.Date(Date)) %>%
        group_by(Date) %>%
        dplyr::summarize(avgResTime = mean(`Downtime (Resolution Time)`)) %>%  # Count of sales per day
        arrange(Date)
      
      plot_ly(data, x = ~Date, y = ~avgResTime, type = 'scatter', mode = 'lines+markers',
              line = list(color = '#1CA4F8'), marker = list(color = '#0d6efd')) %>%
        layout(
          title = "Average Downtime Over Time (Hours)",
          xaxis = list(title = "Date", tickfont = list(size = 10, color = "#333333")),
          yaxis = list(title = "Average Downtime", tickfont = list(size = 10, color = "#333333")),
          font = list(family = "Mulish", color = "#333333"),
          plot_bgcolor = "white",
          paper_bgcolor = "white"
        )
    })
 
 
 
 
    custom_colors_status <- c("#5f9ea0", "#0000cd")  # Blue for resolved, red for unresolved
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
        title = "",
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
    req(filtered_data()) 
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
    # Create funnel chart in Plotly
    if (nrow(aggregated_data) > 0) {
        plot_ly(aggregated_data,
                y = ~reorder(Issue, -AvgDowntime),
                x = ~AvgDowntime,
                type = 'funnel',
                textinfo = "value+percent",
                marker = list(color = '#48d1cc')) %>%
        layout(
            title = "Average Downtime Duration (Hours) by Issue Type",
            yaxis = list(title = "Issue Type", automargin = TRUE),
            xaxis = list(title = "Average Downtime (Days)", tickformat = ',.2f'),
            hoverlabel = list(bgcolor = '#48d1cc', font = list(color = 'white')),
            plot_bgcolor = 'white',
            paper_bgcolor = 'white',
            font = list(family = "Arial")
        )
    } else {
        plot_ly() %>%
            layout(title = "No data available to display",
                   font = list(family = "Arial", size = 12)
            )
    }
})


output$funnelPlot <- renderPlotly({
    req(filtered_data())
    data <- filtered_data()

    # Aggregate to find count of issues by attributable cause
    cause_data <- data %>%
      group_by(`Attributable Cause`) %>%
      dplyr::summarise(Issue_Count = n(), .groups = 'drop') %>%
      arrange(desc(Issue_Count))  # Sort in descending order for better visual impact

    # Creating the bar chart with text labels on top
    plot <- plot_ly(
      cause_data,
      x = ~`Attributable Cause`,
      y = ~Issue_Count,
      type = 'bar',
      marker = list(color = 'blue'),  # Adjust color as needed
      text = ~Issue_Count,  # Text to display (Issue Count)
      textposition = 'outside',  # Position text outside of the bar
      hoverinfo = 'text',  # Show text on hover
      texttemplate = '%{text}'  # Custom template to show only the text
    ) %>%
    layout(
      title = "Distribution of Attributable Causes of Issues",
      xaxis = list(title = 'Attributable Cause'),
      yaxis = list(title = 'Number of Issues'),
      margin = list(l = 150, b = 100)  # Adjust margins to fit labels
    )
    plot
})

output$distributionOfdiscoveries <- renderPlotly({
    req(filtered_data())
    data <- filtered_data()

    # Aggregate to find count of issues by attributable cause
    cause_data <- data %>%
      group_by(Discovered_by) %>%
      dplyr::summarise(discoveree_Count = n(), .groups = 'drop') %>%
      arrange(desc(discoveree_Count))  # Sort in descending order for better visual impact

    # Creating the bar chart with text labels on top
    plot <- plot_ly(
      cause_data,
      x = ~Discovered_by,
      y = ~discoveree_Count,
      type = 'bar',
      marker = list(color = 'blue'),  
      text = ~discoveree_Count,  
      textposition = 'outside',  
      hoverinfo = 'text', 
      texttemplate = '%{text}'  
    ) %>%
    layout(
      title = "Distribution of Discoverees of Issues",
      xaxis = list(title = 'Issue Discoveree'),
      yaxis = list(title = 'Number of Issues'),
      margin = list(l = 150, b = 100)  # Adjust margins to fit labels
    )
    plot
})



  })
}




 