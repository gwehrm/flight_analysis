df_flights <- read.csv("data/Airlines.csv")

df_airport_codes <- read.csv("data/airport_codes.csv", sep = ";") %>% 
  dplyr::select(Airport.Code, Airport.Name, Latitude, Longitude)



df_airlines <- data.frame("airline_name" = c("Alaska Airlines",
                            "American Airlines",
                            "Air Canada",
                            "Aeromexico",
                            "Continental Airlines",
                            "Delta Airlines",
                            "FedEx",
                            "Hawaiian Airlines",
                            "Northwest Airlines",
                            "Polar Air Cargo",
                            "Southwest Airlines",
                            "United Airlines",
                            "United Parcel (UPS)",
                            "Virgin Atlantic",
                            "VivaAerobús",
                            "WestJet",
                            "Endeavor Air Inc.",
                            "JetBlue Airways",
                            "ExpressJet Airlines",
                            "Frontier Airlines Inc.",
                            "Envoy Air",
                            "PSA Airlines Inc.",
                            "SkyWest Airlines Inc.",
                            "Southwest Airlines Co.",
                            "Mesa Airlines Inc."),
                          "Airline" = c("AS",
                                        "AA",
                                        "AC",
                                        "AM",
                                        "CO",
                                        "DL",
                                        "FX",
                                        "HA" ,
                                        "NW" ,
                                        "PO",
                                        "SW" ,
                                        "UA",
                                        "5X",
                                        "VS" ,
                                        "VB" ,
                                        "WS",
                                        "9E",
                                        "B6",
                                        "EV",
                                        "F9",
                                        "MQ",
                                        "OH",
                                        "OO",
                                        "WN",
                                        "YV")
                          
                          )



df_flights <- df_flights %>% 
  left_join(df_airport_codes, by = c("AirportFrom" = "Airport.Code")) %>% 
  rename("Airport.Name.From" = "Airport.Name",
         "start_lon" = "Longitude",
         "start_lat" = "Latitude") %>% 
  left_join(df_airport_codes, by = c("AirportTo" = "Airport.Code")) %>% 
  rename("Airport.Name.To" = "Airport.Name",
         "end_lon" = "Longitude",
         "end_lat" = "Latitude") %>% 
  left_join(df_airlines, by = "Airline") %>% 
  mutate(airline_name_abbr = paste0(airline_name," - ", Airline),
         airport_name_from_abbr = paste0(Airport.Name.From, " - ", AirportFrom),
         airport_name_to_abbr = paste0(Airport.Name.To, " - ", AirportTo),
         time_24 = Time/60)


saveRDS(df_flights, "data/delays.rds")



