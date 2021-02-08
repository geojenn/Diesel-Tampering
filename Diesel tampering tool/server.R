#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.

library(shiny)
library(tidyverse)
library(magrittr)

  

shinyServer(function(input, output, session) {
  
  
  url1 <- a("click here. ", href="http://www.epa.gov/enforcement/national-compliance-initiative-stopping-aftermarket-defeat-devices-vehicles-and-engines")
  url2 <-a("click here,", href="https://marama.org/diesel/")
  url3 <-a("click here.", href="https://www.northeastdiesel.org/tampering.html")


  
  output$info <- renderUI({
    tagList("For more information from the US EPA regarding after-market defeat devices: ", url1, 
            "To learn about the Mid-Atlantic Diesel Collaborative, ", url2,
            " and for information on the Northeast Diesel Collaborative regarding tampering: ", url3)

    # tagList("For more information from the Northeast Diesel Collaborative regarding tampering: ", url2)
  })
  
  
  observe({
    
    
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
    if (input$selectall_states > 0) {
      if (input$selectall_states %% 2 == 0){
        updateCheckboxGroupInput(session=session, 
                                 inputId="States",
                                 choices = c("CT" = "CT", "DC" = "DC", "DE" = "DE", "MA" = "MA", "MD" = "MD",
                                             "ME" = "ME", "NC" = "NC", "NH" = "NH", "NJ" = "NJ", "NY" = "NY",
                                             "PA" = "PA", "RI" = "RI", "VA" = "VA", "VT" = "VT", "WV" = "WV"),
                                 selected = c("CT" = "CT", "DC" = "DC", "DE" = "DE", "MA" = "MA", "MD" = "MD",
                                              "ME" = "ME", "NC" = "NC", "NH" = "NH", "NJ" = "NJ", "NY" = "NY",
                                              "PA" = "PA", "RI" = "RI", "VA" = "VA", "VT" = "VT", "WV" = "WV"))
        
      } else {
        updateCheckboxGroupInput(session=session, 
                                 inputId="States",
                                 choices = c("CT" = "CT", "DC" = "DC", "DE" = "DE", "MA" = "MA", "MD" = "MD",
                                             "ME" = "ME", "NC" = "NC", "NH" = "NH", "NJ" = "NJ", "NY" = "NY",
                                             "PA" = "PA", "RI" = "RI", "VA" = "VA", "VT" = "VT", "WV" = "WV"),
                                 selected = c())
        
      }}
    
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
    
    if (input$selectall_states_pm > 0) {
      if (input$selectall_states_pm %% 2 == 0){
        updateCheckboxGroupInput(session=session, 
                                 inputId="States_pm",
                                 choices = c("CT" = "CT", "DC" = "DC", "DE" = "DE", "MA" = "MA", "MD" = "MD",
                                             "ME" = "ME", "NC" = "NC", "NH" = "NH", "NJ" = "NJ", "NY" = "NY",
                                             "PA" = "PA", "RI" = "RI", "VA" = "VA", "VT" = "VT", "WV" = "WV"),
                                 selected = c("CT" = "CT", "DC" = "DC", "DE" = "DE", "MA" = "MA", "MD" = "MD",
                                              "ME" = "ME", "NC" = "NC", "NH" = "NH", "NJ" = "NJ", "NY" = "NY",
                                              "PA" = "PA", "RI" = "RI", "VA" = "VA", "VT" = "VT", "WV" = "WV"))
        
      } else {
        updateCheckboxGroupInput(session=session, 
                                 inputId="States_pm",
                                 choices = c("CT" = "CT", "DC" = "DC", "DE" = "DE", "MA" = "MA", "MD" = "MD",
                                             "ME" = "ME", "NC" = "NC", "NH" = "NH", "NJ" = "NJ", "NY" = "NY",
                                             "PA" = "PA", "RI" = "RI", "VA" = "VA", "VT" = "VT", "WV" = "WV"),
                                 selected = c())
        
      }}
    
  })
  
  data <- read_csv("data/HDD_shiny_data.csv") 
  output$distPlot <- renderPlot({
    
    
    
    data %<>% 
      filter(Vehicle_type %in% input$Vehicle_type)  %>% 
      filter(State %in% input$States) %>%
      mutate(tampered_value = Value * (1 - input$percent_tampered / 100) + (Value * input$tampered_factor) * input$percent_tampered / 100)%>%
      pivot_longer(cols = c("tampered_value", "Value"), names_to = "Type") 
    
    data$Type %<>% as.factor()
    data$Type %<>% fct_relevel("Value", "tampered_value")
    
    data %<>%
      group_by(State, Type) %>% summarise(sum = sum(value)) 



    if(input$scale_opt == FALSE){
      
      plot <- data %>% ggplot(aes(State, sum))+
        geom_col(aes(fill=Type), position = "dodge")+
        theme_bw()+
        theme(axis.text.x = element_text(size = 16),
              legend.text = element_text(size = 14),
              legend.position="bottom")+
        labs(title = "", x = "", y = "", fill = "")+
        scale_fill_manual(values=c("#c9c9cb", "#01888b"), 
                          name="",
                          breaks=c("Value", "tampered_value"),
                          labels=c("2017 NEI NOx (tons/year)", "Estimated NOx with tampering (tons/year)"))+
        geom_text(aes(label = round(sum), 
                      group=Type),size=5.5, vjust=-.1, hjust=.5,angle=90, position = position_dodge2(width = .9, padding = .2))+
        ggtitle("Annual NOx emissions by state for the selected vehicle type(s) (tons/year)")
      
       print(plot)
       
    }
    
    if(input$scale_opt == TRUE){
      
      data %<>% filter(sum <= input$scale)
      
      plot <- data %>% ggplot(aes(State, sum))+
        geom_col(aes(fill=Type), position = "dodge")+
        theme_bw()+
        theme(axis.text.x = element_text(size = 16),
              legend.text = element_text(size = 14),
              legend.position="bottom")+
        labs(title = "", x = "", y = "", fill = "")+
        scale_fill_manual(values=c("#c9c9cb", "#01888b"), 
                          name="",
                          breaks=c("Value", "tampered_value"),
                          labels=c("2017 NEI NOx (tons/year)", "Estimated NOx with tampering (tons/year)"))+
        geom_text(aes(label = round(sum), 
                      group=Type),size=5.5, vjust=-.1, hjust=.5,angle=90, position = position_dodge2(width = .9, padding = .2))+
        ggtitle("Annual NOx emissions by state for the selected vehicle type(s) (tons/year)")
      
      
      print(plot + ylim(0, input$scale))
    }
    
  })
  
data_pm <- read_csv("data/HDD_shiny_data_PM1.csv")   
  output$distPlotPM <- renderPlot({
    
    data_pm %<>% 
      filter(Vehicle_type %in% input$Vehicle_type_pm)  %>% 
      filter(State %in% input$States_pm) %>%
      mutate(tampered_value = Value * (1 - input$percent_tampered_pm / 100) + (Value * input$tampered_factor_pm) * input$percent_tampered_pm / 100)%>%
      pivot_longer(cols = c("tampered_value", "Value"), names_to = "Type") 
    
    data_pm$Type %<>% as.factor()
    data_pm$Type %<>% fct_relevel("Value", "tampered_value")
    
    data_pm %<>%
      group_by(State, Type) %>% summarise(sum = sum(value)) 
    
    
    if(input$scale_opt_pm == FALSE){
      
      plot_pm <- data_pm %>% ggplot(aes(State, sum))+
        geom_col(aes(fill=Type), position = "dodge")+
        theme_bw()+
        theme(axis.text.x = element_text(size = 16),
              legend.text = element_text(size = 14),
              legend.position="bottom")+
        labs(title = "", x = "", y = "", fill = "")+
        scale_fill_manual(values=c("#c9c9cb", "#ad4f4d"), 
                          name="",
                          breaks=c("Value", "tampered_value"),
                          labels=c("2017 NEI PM2.5 (tons/year)", "Estimated PM2.5 with tampering (tons/year)"))+
        geom_text(aes(label = round(sum), 
                      group=Type),size=5.5, vjust=-.1, hjust=.5,angle=90,position = position_dodge2(width = .9, padding = .2))+
        ggtitle("Annual PM emissions by state for the selected vehicle type(s) (tons/year)")
      
      print(plot_pm)
      
    }
    
    if(input$scale_opt_pm == TRUE){
      
      data_pm %<>% filter(sum <= input$scale_pm)
      
      plot_pm <- data_pm %>% ggplot(aes(State, sum))+
        geom_col(aes(fill=Type), position = "dodge")+
        theme_bw()+
        theme(axis.text.x = element_text(size = 16),
              legend.text = element_text(size = 14),
              legend.position="bottom")+
        labs(title = "", x = "", y = "", fill = "")+
        scale_fill_manual(values=c("#c9c9cb", "#ad4f4d"), 
                          name="",
                          breaks=c("Value", "tampered_value"),
                          labels=c("2017 NEI PM2.5 (tons/year)", "Estimated PM2.5 with tampering (tons/year)"))+
        geom_text(aes(label = round(sum), 
                      group=Type),size=5.5, vjust=-.1, hjust=.5,angle=90,position = position_dodge2(width = .9, padding = .2))+
        ggtitle("Annual PM emissions by state for the selected vehicle type(s) (tons/year)")
      
      print(plot_pm + ylim(0, input$scale_pm))
    }
    
  })
  
  
  output$contents <- renderTable({
    
    data %<>% 
      filter(Vehicle_type %in% input$Vehicle_type)  %>% 
      filter(State %in% input$States) %>%
      mutate(tampered_value = Value * (1 - input$percent_tampered / 100) + (Value * input$tampered_factor) * input$percent_tampered / 100)%>%
      pivot_longer(cols = c("tampered_value", "Value"), names_to = "Type") 
   
      #group_by(State, Type) %>% summarise(sum = sum(value))%>% 
      #pivot_wider(names_from = Type, values_from = sum) 
    
  })
  
  output$contents_pm <- renderTable({

    data_pm %<>% 
      filter(Vehicle_type %in% input$Vehicle_type_pm)  %>% 
      filter(State %in% input$States_pm) %>%
      mutate(tampered_value = Value * (1 - input$percent_tampered_pm / 100) + (Value * input$tampered_factor_pm) * input$percent_tampered_pm / 100)%>%
      pivot_longer(cols = c("tampered_value", "Value"), names_to = "Type") 
    
     # group_by(State, Type) %>% summarise(sum = sum(value))%>% 
    #  pivot_wider(names_from = Type, values_from = sum) 
    
  })
  
  # select/deselect all using action button
  

  
})
