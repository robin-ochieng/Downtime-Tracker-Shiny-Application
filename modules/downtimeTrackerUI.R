downtimeTrackerUI <- function(id) {
 
  ns <- NS(id)
  
  sidebarLayout(
    div(class = "container",
    sidebarPanel(class = 'sidebar-panel',
                 selectInput(inputId = ns("month"), label = "Select Month:", choices = NULL),
                 selectInput(inputId = ns("quarter"), label = "Select Quarter:", choices = NULL),
                 selectInput(inputId = ns("year"), label = "Select Year:", choices = NULL)
 )),
    mainPanel(
      list(
  div(class = "container",
  column(12,
    fluidRow(
      valueBoxOutput(ns("totalIncidents"), width = 4),
      valueBoxOutput(ns("averageDowntimeDuration"), width = 4),
      valueBoxOutput(ns("resolvedIssuesRate"), width = 4),
    ),
    fluidRow(
      box(
        title = "Status Breakdown of Issues",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        height = "500px",
        plotlyOutput(ns("statusBreakdown")) %>% withSpinner(type = 6)
      )
    ),
    fluidRow(
      box(
        title = "Average Downtime by Issue",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        height = "500px",
        plotlyOutput(ns("avgDowntimeByIssue")) %>% withSpinner(type = 6)
      )
    ),
    fluidRow(
      box(
        title = "Distribution of Issues",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        height = "500px",
        plotlyOutput(ns("distributionOfIssues")) %>% withSpinner(type = 6)
      )
    ),
    fluidRow(
      box(
        title = "Distribution of Causes for the Issues",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        height = "500px",
        plotlyOutput(ns("funnelPlot")) %>% withSpinner(type = 6)
      )
    )   
    )
      )
    )
      )
  )

}
