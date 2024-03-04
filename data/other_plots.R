# other plots

################# Delay per airline ############################

sub_df_delay <- df_delays %>%
  mutate(Delay = case_when(Delay == 0 ~ "Punctual",
                           Delay == 1 ~ "Delayed"),
         airline_name = case_when(is.na(airline_name) ~ "NA",
                                  TRUE ~ airline_name)) %>% 
  group_by(airline_name, Delay) %>%
  summarise(counter = n(),
            long = min(end_lon),
            lat = min(end_lat)) %>% 
  ungroup %>% 
  group_by(airline_name) %>% 
  mutate(percentage = counter / sum(counter),
         total = sum(counter)) %>% 
  arrange(percentage)


plot_ly(name = "plot_delay_airline") %>% 
  add_trace(data = sub_df_delay,
            x = ~airline_name,
            y = ~percentage,
            type = "bar",
            name = ~Delay,
            colors = c("#D95F02", "#0f52ba"),
            color = ~Delay) %>% 
  layout(
    title = list(text = "Delay per Airline",
                 font = list(size = 35)),
    yaxis = list(title = list(text = "Prop",
                              font = list(size = 35)),
                 tickfont = list(size = 35)),
    barmode = 'stack',
    xaxis = list(title = list(text = "Airline",
                              font = list(size = 35)),
                 categoryorder = "total descending",
                 tickfont = list(size = 25)),
    legend = list(font = list(size = 30)),
    margin = list("t" = 100)) %>%
  event_register("plotly_click")





################# Delay per Weekday ############################

sub_df_delay <- df_delays %>%
  mutate(Delay = case_when(Delay == 0 ~ "Punctual",
                           Delay == 1 ~ "Delayed"),
         
         DayOfWeek_long = case_when(DayOfWeek == 1 ~ "Mon",
                               DayOfWeek == 2 ~ "Tue",
                               DayOfWeek == 3 ~ "Wed",
                               DayOfWeek == 4 ~ "Thu",
                               DayOfWeek == 5 ~ "Fri",
                               DayOfWeek == 6 ~ "Sat",
                               DayOfWeek == 7 ~ "Sun",
                               )
         
         ) %>% 
  group_by(DayOfWeek,DayOfWeek_long,  Delay) %>%
  summarise(counter = n(),
            long = min(end_lon),
            lat = min(end_lat)) %>% 
  ungroup %>% 
  group_by(DayOfWeek) %>% 
  mutate(percentage = counter / sum(counter),
         total = sum(counter)) %>% 
  arrange(desc(DayOfWeek))


plot_ly(name = "plot_delay_airline") %>% 
  add_trace(data = sub_df_delay,
            x = ~DayOfWeek_long,
            y = ~percentage,
            type = "bar",
            name = ~Delay,
            colors = c("#D95F02", "#0f52ba"),
            color = ~Delay) %>% 
  layout(
    title = list(text = "Delay per Day of the Week",
                 font = list(size = 35)),
    yaxis = list(title = list(text = "Prop",
                              font = list(size = 35)),
                 tickfont = list(size = 35)),
    barmode = 'stack',
    xaxis = list(title = list(text = "Day of the Week",
                              font = list(size = 35)),
                 categoryorder = "total descending",
                 tickfont = list(size = 25)),
    legend = list(font = list(size = 30)),
    margin = list("t" = 100)
    ) %>%
  event_register("plotly_click")





####### Delay per Hour ###############################



# sub_df_delay <- values$df_flights %>%
sub_df_delay <- df_delays %>%
  mutate(Delay = case_when(Delay == 0 ~ "Punctual",
                           Delay == 1 ~ "Delayed"),
         Length_cat = case_when(Length < 60 ~ "< 1h",
                                Length >=60 & Length < 120 ~ "1h - 2h",
                                Length >=120 & Length < 180 ~ "2h - 3h",
                                Length >=180 & Length < 240 ~ "3h - 4h",
                                Length >=240 & Length < 300 ~ "4h - 5h",
                                Length >=300  ~ " > 5h",
                                
                                
         )
  ) %>% 
  group_by(Length_cat, Delay) %>%
  summarise(counter = n(),
            long = min(end_lon),
            lat = min(end_lat)) %>% 
  ungroup %>% 
  group_by(Length_cat) %>% 
  mutate(percentage = counter / sum(counter),
         total = sum(counter)) %>% 
  arrange(percentage, Delay)


plot_ly(name = "plot_delay_airline") %>% 
  add_trace(data = sub_df_delay,
            x = ~Length_cat,
            y = ~percentage,
            type = "bar",
            name = ~Delay,
            colors = c("#D95F02", "#0f52ba"),
            color = ~Delay) %>% 
  layout(
    title =  list(text = "Delay per Flight Duration",
                  font = list(size = 35)),
    yaxis = list(title = list(text = "Prop",
                              font = list(size = 35)),
                 tickfont = list(size = 35)),
    barmode = 'stack',
    xaxis = list(title = list(text = "Duration",
                              font = list(size = 35)),
                 categoryorder = "array",
                 categoryarray = c("< 1h", "1h - 2h", "2h - 3h", "3h - 4h", "4h - 5h", " > 5h"),
                 tickfont = list(size = 35)),
    legend = list(font = list(size = 30)),
    margin = list("t" = 100)
  ) %>%
  event_register("plotly_click")
