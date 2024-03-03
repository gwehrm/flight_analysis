df_delays <- readRDS("data/delays.rds")

# module uebersicht

overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    fluidRow(

             shinydashboard::box(
               title = "Filter",
               status = "success",
               solidHeader = TRUE,
               width = 12,
               column(3,
                      pickerInput(ns("airport"),
                                  "My Airport",
                                  choices = df_delays$airport_name_to_abbr %>% unique %>% sort,
                                  selected = c("Seattle-Tacoma International - SEA"),
                                  multiple = FALSE,
                                  options = list(
                                    `live-search`=TRUE,
                                    `actions-box` = TRUE,
                                    `deselect-all-text` = "None...",
                                    `select-all-text` = "Yeah, all !",
                                    `none-selected-text` = "None Selected")),
                      
                      pickerInput(ns("airline"),
                                  "Airline",
                                  choices = c(),
                                  selected = c(),

                                  multiple = TRUE,
                                  options = list(
                                    `live-search`=TRUE,
                                    `actions-box` = TRUE,
                                    `deselect-all-text` = "None...",
                                    `select-all-text` = "Yeah, all !",
                                    `none-selected-text` = "None Selected"))
                      ),
               
               
               uiOutput(ns("kpi_boxes")),
               leafletOutput(ns("map"), width = "100%", height = "100%"),
               
               
               

             ),
             
             
      #        shinydashboard::box(
      #          title = "Map",
      #          status = "success",
      #          solidHeader = TRUE,
      #          width = 10,
      #          column(12,
      #                 uiOutput(ns("kpi_boxes"))
      #                 ),
      #          column(12,
      #                 plotlyOutput(ns("map"),
      #                              height = "800px")
      #                 )
      # 
      #        )
      #        
      
      shinydashboard::box(
        title = "Airlines",
        status = "success",
        solidHeader = TRUE,
        width = 12,
        plotlyOutput(ns("plot_delay_airline"))),
      
      shinydashboard::box(
        title = "Incoming Routes",
        status = "success",
        solidHeader = TRUE,
        width = 12,
        plotlyOutput(ns("plot_delay_incoming"))),
      
      shinydashboard::box(
        title = "Time",
        status = "success",
        solidHeader = TRUE,
        width = 12,
        plotlyOutput(ns("plot_delay_time")))
      
    )
 
    

  )

}
overview_server <- function(id, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    values <- reactiveValues()
    
    toListen <- reactive({
      list(input$airport,input$airline)
    })
    observeEvent(toListen(), {
      
      values$df_flights <- df_delays %>% 
        filter(airport_name_to_abbr %in% input$airport,
               airline_name_abbr %in% input$airline)
      
      
    })
    
    output$kpi_boxes <- renderUI({
      validate(need(input$airport, message = "Select Aiport"))
      
      number_flights <- values$df_flights %>% 
        nrow
      
      number_routes <-  values$df_flights %>% 
        summarise(total = length(unique(Airport.Name.From))) %>% 
        pull(total)
      
      number_delay <- values$df_flights %>% 
        summarise(total = round(sum(Delay) / n(), 2)*100) %>% 
        pull(total)
      
      tagList(
        column(3,
               valueBox(
                 width = 12,
                 number_flights, 
                 "Number of Flights",
                 icon = icon("plane"),
                 color = "maroon"
               )
        ),
        
        column(3,
               valueBox(
                 width = 12,
                 number_flights, 
                 "Number of Routes",
                 icon = icon("route"),
                 color = "maroon"
               )
        ),
        
        column(3,
               valueBox(
                 width = 12,
                 paste0(number_delay, "%"),
                 "Flights Delayed",
                 icon = icon("clock"),
                 color = "maroon"
               )
        )
        
      )
      
    })
    
    
    observeEvent(input$airport, {
      
      airlines <- df_delays %>% 
        filter(airport_name_to_abbr == input$airport) %>% 
        pull(airline_name_abbr) %>% unique %>% sort

      updatePickerInput(
        session = parent_session,
        inputId = ns("airline"),
                        choices = airlines,
                        selected = airlines)      
      
    }, ignoreInit = TRUE)
    

    

    
    # # output$map <- renderPlotly({
    # #  
    # #   validate(need(input$airport, message = "Select Airport"))
    # #   validate(need(input$airline, message = "Select Airline"))
    # # 
    # # 
    # #   df_air_summary_to <- values$df_flights %>%
    # #     group_by(Airport.Name.To) %>%
    # #     summarise(counter = n(),
    # #               long = min(end_lon),
    # #               lat = min(end_lat),
    # #               "Delays (Percentage)" = round(sum(Delay) / n(), 2) * 100) %>%
    # #     rename("airport" = "Airport.Name.To")
    # #   
    # #   
    # #   # map projection
    # #   geo <- list(
    # #     scope = 'north america',
    # #     projection = list(type = 'azimuthal equal area'),
    # #     showland = TRUE,
    # #     landcolor = toRGB("gray95"),
    # #     countrycolor = toRGB("gray80")
    # #   )
    # #   
    # #   fig <- plot_geo(locationmode = 'USA-states')
    # #   fig <- fig %>% add_markers(
    # #     data = df_air_summary_to, 
    # #     x = ~long,
    # #     y = ~lat,
    # #     text = ~paste0("Airport: ",airport,
    # #                    "<br>Delayed Arrivals: ", `Delays (Percentage)`, "%", 
    # #                    "<br>Total Arrivals: ", counter),
    # #     size = ~counter,
    # #     color = ~`Delays (Percentage)`,
    # #     colors="YlOrRd",
    # #     hoverinfo = "text"
    # #   )
    # #   # fig <- fig %>% add_segments(
    # #   #   data = df_fligts_summary,
    # #   #   x = ~start_lon, xend = ~end_lon,
    # #   #   y = ~start_lat, yend = ~end_lat,
    # #   #   alpha = 0.3, size = I(1), hoverinfo = "none"
    # #   # )
    # #   fig <- fig %>% layout(
    # #     title = "Delays per Arrival Airport",
    # #     geo = geo, showlegend = TRUE, height=800
    # #   )
    # # 
    # #   if(length(input$airport) == 1) {
    # #     
    # #     df_air_start <- values$df_flights %>%
    # #       group_by(Airport.Name.From) %>%
    # #       summarise(counter = n(),
    # #                 long = min(start_lon),
    # #                 lat = min(start_lat)) %>%
    # #       rename("airport" = "Airport.Name.From")
    # #     
    # #    
    # #     
    # #     
    # #     fig %>% 
    # #       add_markers(data = df_air_start,
    # #                   x = ~long,
    # #                   y = ~lat,
    # #                   text = ~paste0("Airport: ", airport,
    # #                                  "<br>Total Departures: ", counter),
    # #                   size = ~counter,
    # #                   colors = "blue",
    # #                   hoverinfo = "text",
    # #                   legend = FALSE,
    # #                   inherit = FALSE
    # #                   
    # #                   )
    # #     
    # #   } else {
    # #     fig
    # #   }
    #   
    # 
    #   
    # })
    
    
    
    output$plot_delay_airline <- renderPlotly({
      
      sub_df_delay <- values$df_flights %>%
        # sub_df_delay <- df_delays %>%
        mutate(Delay = case_when(Delay == 0 ~ "Punctual",
                                 Delay == 1 ~ "Delay")) %>% 
        group_by(airline_name_abbr, Delay) %>%
        summarise(counter = n(),
                  long = min(end_lon),
                  lat = min(end_lat))
      
      plot_ly(name = "plot_delay_airline") %>% 
        add_trace(data = sub_df_delay,
              x = ~airline_name_abbr,
              y = ~counter,
              type = "bar",
              name = ~Delay,
              colors = c("#D95F02", "#1B9E77"),
              color = ~Delay) %>% 
        layout(
          title = "Delay per Airline",
          yaxis = list(title = 'Flights'),
          barmode = 'stack',
          xaxis = list(title = "Airlines",
                       categoryorder = "total descending")) %>%
        event_register("plotly_click")
      
    })
    
  
    observeEvent(event_data(event = "plotly_click",
                            source = "plot_delay_airline"), {
                              browser()
                              
                              clicked <- event_data(event = "plotly_click",
                                                    source = "plot_delay_airline")
                              
                              if (!is.null(clicked)) {
                                values$selected_airline <- clicked$x
                              }
                            })
    
    
    output$plot_delay_incoming <- renderPlotly({
      
      sub_df_delay <- values$df_flights %>%
        mutate(Delay = case_when(Delay == 0 ~ "Punctual",
                                 Delay == 1 ~ "Delay")) %>% 
        group_by(airport_name_from_abbr, AirportFrom, Delay) %>%
        summarise(counter = n(),
                  long = min(end_lon),
                  lat = min(end_lat))
      
      plot_ly(name = "plot_delay_incoming") %>% 
        add_trace(data = sub_df_delay,
                  x = ~AirportFrom,
                  y = ~counter,
                  type = "bar",
                  name = ~Delay,
                  colors = c("#D95F02", "#1B9E77"),
                  color = ~Delay) %>% 
        layout(
          title = "Departure Airport",
          yaxis = list(title = 'Flights'),
          barmode = 'stack',
          xaxis = list(title = "Airport",
                       categoryorder = "total descending")) %>%
        event_register("plotly_click")
      
    })
    
    output$plot_delay_time <- renderPlotly({
      
      sub_df_delay <- values$df_flights %>%
      # sub_df_delay <- df_delays %>%
        mutate(Delay = case_when(Delay == 0 ~ "Punctual",
                                 Delay == 1 ~ "Delay"),
               time_24 = round(time_24, 0)) %>% 
        group_by(time_24, Delay) %>% 
        summarise(counter = n())
      
      sub_df_delay <- values$df_flights %>%
        # sub_df_delay <- df_delays %>%
        mutate(Delay = case_when(Delay == 0 ~ "Punctual",
                                 Delay == 1 ~ "Delay"),
               time_24 = round(time_24, 0)) %>% 
        group_by(time_24, Delay) %>% 
        summarise(counter = n()) %>% 
        ungroup %>% 
        group_by(time_24) %>% 
        mutate(percentage = counter / sum(counter),
               total = sum(counter))
      
      
        ay <- list(
          tickfont = list(color = "red"),
          overlaying = "y",
          side = "right",
          title = "<b>secondary</b> yaxis title")
        
      
      plot_ly(data = sub_df_delay,
              x = ~time_24,
              y = ~counter,
              type = "bar",
              name = ~Delay,
              colors = c("#D95F02", "#1B9E77"),
              color = ~Delay) %>% 
        # 
        # add_trace(data = sub_df_delay %>% filter(Delay == "Punctual"),
        #           x = ~time_24,
        #           y = ~total,
        #           type = "scatter",
        #           line = list(color = 'black', width = 4),
        #           name = "Total Flights",
        #           mode = "line",
        #           inherit = FALSE,
        #           yaxis = "y2") %>% 
        # 
        layout(yaxis = list(title = 'Flights'), barmode = 'stack',
               # yaxis2 = ay,
               xaxis = list(categoryorder = "total descending",
                            title = "Time"))
      
    })
    
  
    

  })
}

