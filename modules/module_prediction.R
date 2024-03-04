fit_rf <- readRDS("rf_model.rds")
# module uebersicht

prediction_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    
    fluidRow(

      column(12,
             shinydashboard::box(
               title = "Filter",
               status = "success",
               solidHeader = TRUE,
               width = 12,
               pickerInput(ns("airport_from"),
                           "From",
                           choices = df_delays$airport_name_to_abbr %>% unique %>% sort,
                           multiple = FALSE,
                           selected = c("Chicago O'hare International - ORD"),
                           options = list(
                             `live-search`=TRUE)),
               
                      pickerInput(ns("airport_to"),
                                  "To",
                                  choices = c(),
                                  multiple = FALSE,
                                  options = list(
                                    `live-search`=TRUE)),
               
               pickerInput(ns("day_week"),
                           "Day of the Week",
                           choices = c("Monday" = 1, "Tuesday" = 2, "Wednesday" = 3, "Thursday" = 4,
                                       "Friday" = 5, "Saturday" = 6, "Sunday" = 7),
                           multiple = F),
               
                      pickerInput(ns("time"),
                                  "Time",
                                  choices = c(1:24),
                                  selected = 18,
                                  multiple = F)

             ),
             
             shinydashboard::box(
               title = "Prediction Delay Airlines",
               status = "success",
               solidHeader = TRUE,
               width = 12,
               plotlyOutput(ns("prediction_delay_airline"))),
             
             shinydashboard::box(
               title = "Ticket",
               status = "success",
               solidHeader = TRUE,
               width = 12,
               htmlOutput(ns("text_ticket")))           
             
             
             
             
             
             ))

  )

}

prediction_server <- function(id, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    
    ns <- session$ns

    values <- reactiveValues()
    
    
    
    observeEvent(input$airport_from, {
      validate(need(input$airport_from, message = "select airport"))

      airport_to <- df_delays %>% 
        filter(airport_name_from_abbr == input$airport_from) %>% 
        pull(airport_name_to_abbr) %>% unique %>% sort
      
      updatePickerInput(
        session = parent_session,
        inputId = ns("airport_to"),
        choices = airport_to,
        selected = c())      
      
    }, ignoreInit = TRUE)
    
    
    toListen <- reactive({
      list(input$airport_from, 
           input$airport_to,
           input$time,
           input$day_week
           )
    })
    
    
    
    observeEvent(toListen(), {
      validate(need(input$airport_from, message = "select airport"))
      validate(need(input$airport_to, message = "select airport"))
      validate(need(input$day_week, message = "select Day"))
      validate(need(input$time, message = "select airport"))
      

      df_sub <- df_delays %>% 
        filter(airport_name_from_abbr == input$airport_from,
               airport_name_to_abbr == input$airport_to) %>% 
        dplyr::select(Airline, AirportFrom, AirportTo, Length) %>% 
        mutate(Length = mean(Length, na.rm = TRUE),
               Time = as.numeric(input$time)*60,
               DayOfWeek = input$day_week) %>% 
        distinct()
      
      
      sub_pred = predict(fit_rf ,newdata=df_sub, type = "prob")
      
      
      df_sub$prediction_delay <- sub_pred[, 2]
      
      
      values$df_sub <- df_sub
      
      
      

      
    }, ignoreInit = TRUE)
    
    
    output$prediction_delay_airline <- renderPlotly({
      validate(need(values$df_sub, message = "select airport"))
      
      airlines <- df_delays %>% 
        dplyr::select(Airline, airline_name_abbr) %>% 
        distinct()
      
      
      df_plot <- values$df_sub %>% 
        left_join(airlines, by ="Airline")
      
      plot_ly() %>% 
        add_trace(data = df_plot,
                  x = ~airline_name_abbr,
                  y = ~prediction_delay,
                  type = "bar") %>% 
        layout(
          title = "Probability of Delay per Airline",
          yaxis = list(title = 'Probability'),
          barmode = 'stack',
          xaxis = list(title = "Airlines",
                       categoryorder = "total ascending"))
        
      
      
      
      
    })
    
    
    output$text_ticket <- renderText({
      validate(need(values$df_sub, message = ""))
      
      airlines <- df_delays %>% 
        dplyr::select(Airline, airline_name_abbr) %>% 
        distinct()
      
      
      df_plot <- values$df_sub %>% 
        left_join(airlines, by ="Airline")
      
      
      airline <- df_plot %>% 
        filter(prediction_delay == min(prediction_delay)) %>% 
        pull(airline_name_abbr)
      
      
      paste0('Buy your ', airline, ' ticket <a href="https://www.expedia.com/Flights?locale=en_US&siteid=1&semcid=US.MULTILOBF.GOOGLE.GT-c-EN.FLIGHT&semdtl=a114882516568.b1129846236522.g1kwd-297780792895.e1c.m1CjwKCAiA3JCvBhA8EiwA4kujZirouZA9PPg-9dcIONyfKk7QN5SASbP50UKcxW1n6iFvLjX5MOoHRxoCf4wQAvD_BwE.r1b5856f9c17121a52daa22ea50be509d0c3afec73eb924fdc49ba16b7986c55c8.c1HmxhXvb1NR4X6FiQU5Pnpg.j19060229.k1.d1550480922523.h1e.i1.l1.n1.o1.p1.q1.s1.t1.x1.f1.u1.v1.w1&gad_source=1&gclid=CjwKCAiA3JCvBhA8EiwA4kujZirouZA9PPg-9dcIONyfKk7QN5SASbP50UKcxW1n6iFvLjX5MOoHRxoCf4wQAvD_BwE" target="_blank">here!</a>')
      
      
      
      
      
    })


  })
}

