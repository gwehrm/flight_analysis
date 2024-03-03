library(shiny)
library(shinydashboardPlus)
library(shinydashboard)
library(stringr)
library(plotly)
library(shinycssloaders)
library(lubridate)
library(shinyWidgets)
library(DT)
library(shinyjs)
library(dplyr)
library(tidyr)
library(shinyBS)
library(leaflet)
library(randomForest)

dashelp::source_files(c("helper", "modules"))


# UI
ui <- dashboardPage(
  title = "Slot Coordinator Dashboard",

  shinydashboard::dashboardHeader(
    title = dashelp::add_title_header("Flight Analysis", dev = FALSE),
    titleWidth = "350px",
    dashelp::add_logo_header(),
    shinydashboard::dropdownMenuOutput("notif")),
  skin = c("green"),
  shinydashboard::dashboardSidebar(
    collapsed = TRUE,
    shinydashboard::sidebarMenu(id = "tabs",
                                shinydashboard::menuItem("Slot Coordinator",
                                                         tabName = "slot_coordinator",
                                                         icon = icon("chart-line")),
                                shinydashboard::menuItem("Prediction for Customer",
                                                         tabName = "prediction",
                                                         icon = icon("brain"))
                              
                                
                                
                                )),
  shinydashboard::dashboardBody(
    dashelp::use_company_colors(dev = FALSE),
    tags$head(includeHTML("www/google_analytics.html")),
    uiOutput("content")
    )
)


server <- function(input, output, session) {
  output$notif <- shinydashboard::renderMenu({
    dashelp::create_menu(header_text = "MVP",
                         link_release = "release_notes.html",
                         link_req = "")

  })

  output$content <- renderUI({

    if (input$tabs == "slot_coordinator") {
      overview_ui("slot_coordinator")
    } else if (input$tabs == "prediction") {
      prediction_ui("prediction")
    }

  })
  overview_server("slot_coordinator", parent_session = session)
  prediction_server("prediction", parent_session = session)
  }

shinyApp(ui, server)


