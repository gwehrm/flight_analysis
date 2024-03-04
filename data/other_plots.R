# other plots

################# Delay per airline ############################

sub_df_delay <- df_delays %>%
  mutate(Delay = case_when(Delay == 0 ~ "Punctual",
                           Delay == 1 ~ "Delayed")) %>% 
  group_by(Airline, Delay) %>%
  summarise(counter = n(),
            long = min(end_lon),
            lat = min(end_lat)) %>% 
  ungroup %>% 
  group_by(Airline) %>% 
  mutate(percentage = counter / sum(counter),
         total = sum(counter)) %>% 
  arrange(percentage)


plot_ly(name = "plot_delay_airline") %>% 
  add_trace(data = sub_df_delay,
            x = ~Airline,
            y = ~percentage,
            type = "bar",
            name = ~Delay,
            colors = c("#D95F02", "#1B9E77"),
            color = ~Delay) %>% 
  layout(
    title = "Delay per Airline",
    yaxis = list(title = 'Proportion',
                 tickfont = list(size = 35)),
    barmode = 'stack',
    xaxis = list(title = "Airlines",
                 categoryorder = "total descending",
                 tickfont = list(size = 25)),
    legend = list(font = list(size = 30))) %>%
  event_register("plotly_click")





################# Delay per Weekday ############################

sub_df_delay <- df_delays %>%
  mutate(Delay = case_when(Delay == 0 ~ "Punctual",
                           Delay == 1 ~ "Delayed")) %>% 
  group_by(DayOfWeek, Delay) %>%
  summarise(counter = n(),
            long = min(end_lon),
            lat = min(end_lat)) %>% 
  ungroup %>% 
  group_by(DayOfWeek) %>% 
  mutate(percentage = counter / sum(counter),
         total = sum(counter)) %>% 
  arrange(percentage)


plot_ly(name = "plot_delay_airline") %>% 
  add_trace(data = sub_df_delay,
            x = ~DayOfWeek,
            y = ~percentage,
            type = "bar",
            name = ~Delay,
            colors = c("#D95F02", "#1B9E77"),
            color = ~Delay) %>% 
  layout(
    title = "Delay per DayOfWeek",
    yaxis = list(title = 'Proportion',
                 tickfont = list(size = 35)),
    barmode = 'stack',
    xaxis = list(title = "Day of Week",
                 categoryorder = "total descending",
                 tickfont = list(size = 25)),
    legend = list(font = list(size = 30))) %>%
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
            colors = c("#D95F02", "#1B9E77"),
            color = ~Delay) %>% 
  layout(
    title = "Delay per Length",
    yaxis = list(title = 'Prop',
                 tickfont = list(size = 35)),
    barmode = 'stack',
    xaxis = list(title = "Length Flight",
                 categoryorder = "array",
                 categoryarray = c("< 1h", "1h - 2h", "2h - 3h", "3h - 4h", "4h - 5h", " > 5h"),
                 tickfont = list(size = 35)),
    legend = list(font = list(size = 30))
  ) %>%
  event_register("plotly_click")
