

# NOx - vehicle type ------------------------------------------------------


if (input$selectall_vehicle > 0) {
  if (input$selectall_vehicle %% 2 == 0){
    updateCheckboxGroupInput(session=session, 
                             inputId="Vehicle_type",
                             choices = c("Combination Long-haul Trucks",
                                         "Refuse Trucks",
                                         "Transit Buses",
                                         "Combination Short-haul Trucks",
                                         "Intercity Buses",
                                         "Passenger Cars",
                                         "Single Unit Long-haul Trucks",
                                         "Light Commercial Trucks",
                                         "Passenger Trucks",
                                         "Single Unit Short-haul Trucks"),
                             selected = c("Combination Long-haul Trucks",
                                          "Refuse Trucks",
                                          "Transit Buses",
                                          "Combination Short-haul Trucks",
                                          "Intercity Buses",
                                          "Passenger Cars",
                                          "Single Unit Long-haul Trucks",
                                          "Light Commercial Trucks",
                                          "Passenger Trucks",
                                          "Single Unit Short-haul Trucks"))
    
  } else {
    updateCheckboxGroupInput(session=session, 
                             inputId="Vehicle_type",
                             choices = c("Combination Long-haul Trucks",
                                         "Refuse Trucks",
                                         "Transit Buses",
                                         "Combination Short-haul Trucks",
                                         "Intercity Buses",
                                         "Passenger Cars",
                                         "Single Unit Long-haul Trucks",
                                         "Light Commercial Trucks",
                                         "Passenger Trucks",
                                         "Single Unit Short-haul Trucks"),
                             selected = c())
    
  }}


# NOx - states ------------------------------------------------------------


# PM - Vehicle type -------------------------------------------------------
if (input$selectall_vehicle_pm > 0) {
  if (input$selectall_vehicle_pm %% 2 == 0){
    updateCheckboxGroupInput(session=session, 
                             inputId="Vehicle_type_pm",
                             choices = c("Combination Long-haul Trucks",
                                         "Refuse Trucks",
                                         "Transit Buses",
                                         "Combination Short-haul Trucks",
                                         "Intercity Buses",
                                         "Passenger Cars",
                                         "Single Unit Long-haul Trucks",
                                         "Light Commercial Trucks",
                                         "Passenger Trucks",
                                         "Single Unit Short-haul Trucks"),
                             selected = c("Combination Long-haul Trucks",
                                          "Refuse Trucks",
                                          "Transit Buses",
                                          "Combination Short-haul Trucks",
                                          "Intercity Buses",
                                          "Passenger Cars",
                                          "Single Unit Long-haul Trucks",
                                          "Light Commercial Trucks",
                                          "Passenger Trucks",
                                          "Single Unit Short-haul Trucks"))
    
  } else {
    updateCheckboxGroupInput(session=session, 
                             inputId="Vehicle_type_pm",
                             choices = c("Combination Long-haul Trucks",
                                         "Refuse Trucks",
                                         "Transit Buses",
                                         "Combination Short-haul Trucks",
                                         "Intercity Buses",
                                         "Passenger Cars",
                                         "Single Unit Long-haul Trucks",
                                         "Light Commercial Trucks",
                                         "Passenger Trucks",
                                         "Single Unit Short-haul Trucks"),
                             selected = c())
    
  }}

# PM - states -------------------------------------------------------------


