#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.

library(shiny)
library(tidyverse)
library(magrittr)

  

shinyServer(function(input, output, session) {
  
  
  url1 <- a("click here. ", href="http://www.epa.gov/enforcement/national-compliance-initiative-stopping-aftermarket-defeat-devices-vehicles-and-engines")
  url2 <-a("click here.", href="https://marama.org/diesel/")
  url3 <-a("click here.", href="https://www.northeastdiesel.org/tampering.html")
  url4 <- a("click here.", href="https://cleanairact.org/wp-content/uploads/2019/09/Tampering-and-Aftermarket-Defeat-Devices-Phil-Brooks.pdf")
 
  output$info <- renderUI({
    tagList("For more information from the US EPA regarding after-market defeat devices: ", url1)
    
    tagList("To learn about the Mid-Atlantic Diesel Collaborative, ", url2)
            
    tagList("To learn about tampering from the Northeast Diesel Collaborative", url3)
    
    tagList("Presentation: 'Tampering and after-market defeat devices' by Phillip Brooks, Air Enforcement Division, EPA - August 2019", url4)

  })
  

# select/deselect all buttons ----------------------------------------------
  
  observe({
    
    if (input$selectall_vehicle > 0) {
      if (input$selectall_vehicle %% 2 != 0){
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
                                              "Single Unit Short-haul Trucks"),
                                 inline = TRUE)
        
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
                                 selected = c(),
                                 inline = TRUE)
        
      }}
    
    
    if (input$selectall_states > 0) {
      if (input$selectall_states %% 2 != 0){
        updateCheckboxGroupInput(session=session, 
                                 inputId="States",
                                 choices = c("CT" = "CT", "DC" = "DC", "DE" = "DE", "MA" = "MA", "MD" = "MD",
                                             "ME" = "ME", "NC" = "NC", "NH" = "NH", "NJ" = "NJ", "NY" = "NY",
                                             "PA" = "PA", "RI" = "RI", "VA" = "VA", "VT" = "VT", "WV" = "WV"),
                                 selected = c("CT" = "CT", "DC" = "DC", "DE" = "DE", "MA" = "MA", "MD" = "MD",
                                              "ME" = "ME", "NC" = "NC", "NH" = "NH", "NJ" = "NJ", "NY" = "NY",
                                              "PA" = "PA", "RI" = "RI", "VA" = "VA", "VT" = "VT", "WV" = "WV"),
                                 inline = TRUE)
        
      } else {
        updateCheckboxGroupInput(session=session, 
                                 inputId="States",
                                 choices = c("CT" = "CT", "DC" = "DC", "DE" = "DE", "MA" = "MA", "MD" = "MD",
                                             "ME" = "ME", "NC" = "NC", "NH" = "NH", "NJ" = "NJ", "NY" = "NY",
                                             "PA" = "PA", "RI" = "RI", "VA" = "VA", "VT" = "VT", "WV" = "WV"),
                                 selected = c(),
                                 inline = TRUE)
        
      }}
    

    if (input$selectall_vehicle_pm > 0) {
      if (input$selectall_vehicle_pm %% 2 != 0){
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
                                              "Single Unit Short-haul Trucks"),
                                 inline = TRUE)
        
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
                                 selected = c(),
                                 inline = TRUE)
        
      }}
    
    
    if (input$selectall_states_pm > 0) {
      if (input$selectall_states_pm %% 2 != 0){
        updateCheckboxGroupInput(session=session, 
                                 inputId="States_pm",
                                 choices = c("CT" = "CT", "DC" = "DC", "DE" = "DE", "MA" = "MA", "MD" = "MD",
                                             "ME" = "ME", "NC" = "NC", "NH" = "NH", "NJ" = "NJ", "NY" = "NY",
                                             "PA" = "PA", "RI" = "RI", "VA" = "VA", "VT" = "VT", "WV" = "WV"),
                                 selected = c("CT" = "CT", "DC" = "DC", "DE" = "DE", "MA" = "MA", "MD" = "MD",
                                              "ME" = "ME", "NC" = "NC", "NH" = "NH", "NJ" = "NJ", "NY" = "NY",
                                              "PA" = "PA", "RI" = "RI", "VA" = "VA", "VT" = "VT", "WV" = "WV"),
                                 inline = TRUE)
        
      } else {
        updateCheckboxGroupInput(session=session, 
                                 inputId="States_pm",
                                 choices = c("CT" = "CT", "DC" = "DC", "DE" = "DE", "MA" = "MA", "MD" = "MD",
                                             "ME" = "ME", "NC" = "NC", "NH" = "NH", "NJ" = "NJ", "NY" = "NY",
                                             "PA" = "PA", "RI" = "RI", "VA" = "VA", "VT" = "VT", "WV" = "WV"),
                                 selected = c(),
                                 inline = TRUE)
        
      }}
    
  })
  

# plots -------------------------------------------------------------------
  
  data <- read_csv("data/HDD_shiny_data.csv") 
  output$distPlot <- renderPlot({
    
    data %<>% 
      filter(Vehicle_type %in% input$Vehicle_type)  %>% 
      filter(State %in% input$States) %>%
      mutate(tampered_value = Value * (1 - input$percent_tampered / 100) + (Value * input$tampered_factor) * input$percent_tampered / 100,
             ev_value = Value * (1 - input$percent_tampered / 100 - input$percent_ev/100) + (Value * input$tampered_factor) * input$percent_tampered / 100 +  (Value * 0) * input$percent_ev/100)%>%
      pivot_longer(cols = c("tampered_value", "ev_value", "Value"), names_to = "Type") 
    
    data$Type %<>% as.factor()
    data$Type %<>% fct_relevel("Value", "tampered_value", "ev_value")

    
    data %<>%
      group_by(State, Type) %>% summarise(sum = sum(value)) 



  if(input$ev_opt == FALSE){
      
      data1 <- data %>% 
        filter(Type != "ev_value")
    
      plot <- data1 %>%
      ggplot(aes(reorder(State, sum), sum))+
        geom_col(aes(fill=Type), position = "dodge")+
        theme_minimal()+
        theme(axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 14),
              legend.text = element_text(size = 14),
              legend.position="bottom")+
        labs(title = "Annual NOx emissions by state for the selected vehicle type(s) (tons/year)", x = "", y = "", fill = "")+
        scale_fill_manual(values=c("#c9c9cb", "#01888b", "#00e58f"), 
                          name="",
                          breaks=c("Value", "tampered_value", "ev_value"),
                          labels=c("2017 NEI NOx (tons/year)", "Estimated NOx with tampering (tons/year)", "Estimated NOx with tampering and electric vehicle offset (tons/year)"))+
        geom_text(aes(label = round(sum), 
                      group=Type),size=4.5, vjust=-.1, hjust=-.1,angle=0, position = position_dodge2(width = .9, padding = .2))+
        coord_flip()+
        ylim(0, max(data1$sum *1.03))
      
       print(plot)
       
    }
    
  if(input$ev_opt == TRUE){
      
      plot <- data %>% 
        ggplot(aes(reorder(State, sum), sum))+
        geom_col(aes(fill=Type), position = "dodge")+
        theme_minimal()+
        theme(axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 14),
              legend.text = element_text(size = 14),
              legend.position="bottom")+
       labs(title = "Annual NOx emissions by state for the selected vehicle type(s) (tons/year)", x = "", y = "", fill = "")+
        scale_fill_manual(values=c("#c9c9cb", "#01888b", "#00e58f" ), 
                          name="",
                          breaks=c("Value", "tampered_value", "ev_value"),
                          labels=c("2017 NEI NOx", "Estimated NOx with tampering", "Estimated NOx with tampering and electric vehicle offset"))+
       geom_text(aes(label = round(sum), 
                      group=Type),size=4.5, vjust=-.1, hjust=-.1,angle=0, position = position_dodge2(width = .9, padding = .2))+
        coord_flip()+
        ylim(0, max(data$sum *1.03))
      
      print(plot)
    }
    
  })
  
data_pm <- read_csv("data/HDD_shiny_data_PM1.csv")   
  output$distPlotPM <- renderPlot({
    
    data_pm %<>% 
      filter(Vehicle_type %in% input$Vehicle_type_pm)  %>% 
      filter(State %in% input$States_pm) %>%
      mutate(tampered_value = Value * (1 - input$percent_tampered_pm / 100) + (Value * input$tampered_factor_pm) * input$percent_tampered_pm / 100,
             ev_value = Value * (1 - input$percent_tampered_pm / 100 - input$percent_ev_pm/100) + (Value * input$tampered_factor_pm) * input$percent_tampered_pm / 100 +  (Value * 0) * input$percent_ev_pm/100)%>%
      pivot_longer(cols = c("tampered_value", "ev_value", "Value"), names_to = "Type") 
    
    data_pm$Type %<>% as.factor()
    data_pm$Type %<>% fct_relevel("Value", "tampered_value", "ev_value")
    
    data_pm %<>%
      group_by(State, Type) %>% summarise(sum = sum(value)) 
    
    
    if(input$ev_opt_pm == FALSE){
      
      data_pm1 <- data_pm %>% 
        filter(Type != "ev_value")
      
      plot_pm <- data_pm1 %>%  
      ggplot(aes(reorder(State, sum), sum))+
        geom_col(aes(fill=Type), position = "dodge")+
        theme_minimal()+
        theme(axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 14),
              legend.text = element_text(size = 14),
              legend.position="bottom")+
        labs(title = "Annual PM emissions by state for the selected vehicle type(s) (tons/year)", x = "", y = "", fill = "")+
        scale_fill_manual(values=c("#c9c9cb", "#ad4f4d", "#38d5d8"), 
                          name="",
                          breaks=c("Value", "tampered_value", "ev_value"),
                          labels=c("2017 NEI PM2.5", "Estimated PM2.5 with tampering", "Estimated PM2.5 with tampering and electric vehicle offset"))+
        geom_text(aes(label = round(sum), 
                      group=Type),size=4.5, vjust=-.1, hjust=-.1,angle=0,position = position_dodge2(width = .9, padding = .2))+
        coord_flip()+
        ylim(0, max(data_pm1$sum *1.03))
      
      print(plot_pm)
      
    }
    
    if(input$ev_opt_pm == TRUE){
    
      plot_pm <- data_pm %>%
      ggplot(aes(reorder(State, sum), sum))+
        geom_col(aes(fill=Type), position = "dodge")+
        theme_minimal()+
        theme(axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 14),
              legend.text = element_text(size = 14),
              legend.position="bottom")+
        labs(title = "Annual PM emissions by state for the selected vehicle type(s) (tons/year)", x = "", y = "", fill = "")+
        scale_fill_manual(values=c("#c9c9cb", "#ad4f4d", "#38d5d8"), 
                          name="",
                          breaks=c("Value", "tampered_value", "ev_value"),
                          labels=c("2017 NEI PM2.5", "Estimated PM2.5 with tampering", "Estimated PM2.5 with tampering and electric vehicle offset"))+
        geom_text(aes(label = round(sum), 
                      group=Type),size=4.5, vjust=-.1, hjust=-.1,angle=0,position = position_dodge2(width = .9, padding = .2))+
        coord_flip()+
        ylim(0, max(data_pm$sum *1.03))
     
      print(plot_pm)
    }
    
  })

  
  data_allyears <- read_csv("data/nei_diesel_111417_shiny.csv")   
  output$distPlot_allyears <- renderPlot({
    
    
    data_allyears %<>% 
      filter(Vehicle_type %in% input$Vehicle_type_allyears)  %>% 
      filter(State %in% input$States_allyears) 
    
    
    
    data2011 <- data_allyears %>%
      filter(Year == 2011) %>%
      mutate(tampered_value = Value * (1 - input$percent_tampered_2011 / 100) + (Value * input$tampered_factor_allyears) * input$percent_tampered_2011 / 100)
    
    data2014 <- data_allyears %>%
      filter(Year == 2014) %>%
      mutate(tampered_value = Value * (1 - input$percent_tampered_2014 / 100) + (Value * input$tampered_factor_allyears) * input$percent_tampered_2014 / 100)
    
    data2017 <- data_allyears %>%
      filter(Year == 2017) %>%
      mutate(tampered_value = Value * (1 - input$percent_tampered_2017 / 100) + (Value * input$tampered_factor_allyears) * input$percent_tampered_2017 / 100)
        
    
    data_allyears <- bind_rows(data2011, data2014, data2017)
    
    
    data_allyears %<>% pivot_longer(cols = c("tampered_value", "Value"), names_to = "Type") 
    
    data_allyears$Year %<>% 
      as.factor() %>%
      fct_relevel("2011", "2014", "2017")
    
    data_allyears$Type %<>% 
      as.factor() %>% 
      fct_relevel("Value","tampered_value")
    
    data_allyears %<>%
      group_by(State, Type, Year) %>% 
      summarise(sum = sum(value))

    
    
    data_allyears %>%
      ggplot(aes(Year, sum))+
      geom_col(aes(fill = Type), position = "dodge")+
      theme(axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            legend.text = element_text(size = 14),
            legend.position="bottom")+
      scale_fill_manual(values=c("#c9c9cb", "#01888b"), 
                        name="",
                        breaks=c("Value", "tampered_value"),
                        labels=c("Annual NEI NOx (tons)", "Estimated annual NOx (tons) with tampering"))+
      labs(title = "Annual NOx emissions in tons for NEI years 2011, 2014, and 2017",
           subtitle = paste0('State = ', input$States_allyears, ', Vehicle type = ', input$Vehicle_type_allyears),
           x = "",
           y = "",
           fill = "")+
      geom_text(aes(label = round(sum)),size=4.5, vjust=.3, hjust=-.1,angle=0, position = position_dodge2(width = .9, padding = .2))+
      theme_minimal()+
      ylim(0, max(data_allyears$sum) *1.03)+
      coord_flip()

    
  })
  
# tables ------------------------------------------------------------------

  output$contents <- renderTable({
    
    
    data %<>% 
      filter(Vehicle_type %in% input$Vehicle_type)  %>% 
      filter(State %in% input$States) %>%
      mutate(tampered_value = Value * (1 - input$percent_tampered / 100) + (Value * input$tampered_factor) * input$percent_tampered / 100,
             ev_value = Value * (1 - input$percent_tampered / 100 - input$percent_ev/100) + (Value * input$tampered_factor) * input$percent_tampered / 100 +  (Value * -1) * input$percent_ev/100)
    
  })
  
  output$contents_pm <- renderTable({

    data_pm %<>% 
      filter(Vehicle_type %in% input$Vehicle_type_pm)  %>% 
      filter(State %in% input$States_pm) %>%
      mutate(tampered_value = Value * (1 - input$percent_tampered_pm / 100) + (Value * input$tampered_factor_pm) * input$percent_tampered_pm / 100,
             ev_value = Value * (1 - input$percent_tampered_pm / 100 - input$percent_ev_pm/100) + (Value * input$tampered_factor_pm) * input$percent_tampered_pm / 100 +  (Value * -1) * input$percent_ev_pm/100)

  })
  

  
})
