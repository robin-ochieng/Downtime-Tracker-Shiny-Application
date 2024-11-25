library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(tidyverse)
library(ggthemes)
library(hrbrthemes)
library(plotly)
library(readxl)
library(writexl)
library(janitor)
library(plyr)
library(dplyr)
library(tidyquant)
library(lubridate)
library(toOrdinal)
library(scales)
library(xkcd)
library(lifecontingencies)
library(bs4Dash)
library(bslib)
options(scipen=999)

source("modules/downtimeTrackerUI.R", local = TRUE)[1]
source("modules/downtimeTrackerServer.R", local = TRUE)[1] 
source("modules/functions.R", local = TRUE)[1]


  
# Define a custom theme using bslib
my_theme <- bs_theme(
  bootswatch = "minty", 
  bg = "#E1E1E1", 
  fg = "#202123", 
  primary = "#EA80FC", 
  secondary = "#00BFA5",
  success = "#4CAF50",   
  info = "#2196F3",     
  warning = "#FFC107",   
  danger = "#F44336",   
  base_font = font_google("Mulish"),
  heading_font = font_google("Mulish"),
  code_font = font_google("Mulish"),
  navbar_bg = "#333333", 
  navbar_fg = "#ffffff"
)

ui <- bs4DashPage(
  title = "IBS Tracker",
  dark = NULL,
  help = NULL,
  fullscreen = FALSE,
  scrollToTop = TRUE,
  freshTheme = my_theme,
  header = bs4DashNavbar(
    fixed = TRUE,
    border = FALSE,
    title = dashboardBrand(
      title = tags$li(
        class = "text-center header-title-container",  # Added a new class for more specific styling
        tags$h4("IBS Tracker", class = "header-title")
      ),
      color = "white",
      image = NULL  
    ),
    .rightUi = div(
        tags$img(src = "images/kenbright.png", class = "right-logo"),
        class = "rightUi"
    )   
),
  sidebar = bs4DashSidebar(disable = TRUE),
  body = bs4DashBody(
    useShinyjs(),
    tags$head(
      includeCSS("www/css/custom_styles.css"),
      tags$link(href = "https://fonts.googleapis.com/css2?family=Mulish:wght@400;700&display=swap", rel = "stylesheet"),
      tags$link(rel = "shortcut icon", href = "favicon/kenbright2.ico", type = "image/x-icon")
      ),
      downtimeTrackerUI("downtimeTrackerReport")   
  ),
  footer = bs4DashFooter(
    div(class="footer", 
        "© 2024 Downtime Tracker | Powered by Kenbright Actuarial & Financial Services Ltd")
  )
)


# Define server logic
server <- function(input, output, session) {
  # Load the data from this path data\data.xlsx
  data <- load_data("data/Downtime Tracker.xlsx")

  
  downtimeTrackerServer("downtimeTrackerReport", data)


}

# Run the application
shinyApp(ui = ui, server = server)