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
        title = "How Has Average Downtime Changed Over Time?",
        status = "white",
        solidHeader = TRUE,
        width = 12,
        height = "500px",
        plotlyOutput(ns("averageResolutionTimeOverTime")) %>% withSpinner(type = 6)
      )
    ),
    fluidRow(
      box(
        title = "What Does the Distribution of Issues Over Time Tell Us?",
        status = "white",
        solidHeader = TRUE,
        width = 12,
        height = "500px",
        plotlyOutput(ns("Issues_over_time")) %>% withSpinner(type = 6)
      )
    ),
    fluidRow(
      box(
        title = "Which Factors Contribute Most to Operational Issues?",
        status = "white",
        solidHeader = TRUE,
        width = 12,
        height = "500px",
        plotlyOutput(ns("funnelPlot")) %>% withSpinner(type = 6)
      )
    ),
    fluidRow(
      box(
        title = "What Percentage of Issues Remain Unresolved?",
        status = "white",
        solidHeader = TRUE,
        width = 12,
        height = "500px",
        plotlyOutput(ns("statusBreakdown")) %>% withSpinner(type = 6)
      )
    ), 
    fluidRow(
      box(
        title = "How Does Downtime Vary Across Different Types of Issues?",
        status = "white",
        solidHeader = TRUE,
        width = 12,
        height = "500px",
        plotlyOutput(ns("avgDowntimeByIssue")) %>% withSpinner(type = 6)
      )
    ),
    fluidRow(
      box(
        title = "What Are the Common Types of Issues Reported?",
        status = "white",
        solidHeader = TRUE,
        width = 12,
        height = "500px",
        plotlyOutput(ns("distributionOfIssues")) %>% withSpinner(type = 6)
      )
    )  
    )
      )
    )
      )
  )

}
