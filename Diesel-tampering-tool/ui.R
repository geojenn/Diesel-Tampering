
# reference: https://shiny.rstudio.com/articles/layout-guide.html

library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("simplex"),
                  #theme = "style.css",
                 # div(
                  #  style = "padding: 0px 0px; width: '100%'",
                      titlePanel(
                        title = "Diesel Vehicle Emissions Control Equipment Tampering - Annual Impact",
                        windowTitle = "Tampering"
                   #   )
                  ),
                  navbarPage(
                    
                    # Application title.
                    title = div(img(src = "MARAMA_logo_small2.png"),
                                         #,
                   #                  "Diesel Vehicle Emissions Control Equipment Tampering - Annual Impact",
                                      style = "position: relative; top: -85%; transform: translateY(-50%);"
                   
                    ),
                   
                   tabPanel("2017 NEI NOx",
                                fluidRow(
                                  
                                  column(8,
                                         plotOutput("distPlot", height = "825px")),
                                  
                                  column(4,
                                         sliderInput("percent_tampered",
                                                     "Percentage of fleet with tampered emissions control equipment (approx.):",
                                                     min = 0,
                                                     max = 100,
                                                     value = 0),
                                         numericInput("tampered_factor",
                                                      "Tampering increases NOx emissions by this factor (approx.):",
                                                      value = 310),
                                         checkboxInput("ev_opt",
                                                       "Estimate electric vehicle offset?",
                                                       value = FALSE),
                                         sliderInput("percent_ev",
                                                     "Percentage of un-tampered fleet to be replaced with electric vehicles:",
                                                     min = 0,
                                                     max = 100,
                                                     value = 0),
                                         #  numericInput("scale",
                                         #              "Y-axis max limit:",
                                         #             value = 27000),
                                         checkboxGroupInput("States", 
                                                            
                                                            "Choose state(s) to display:",
                                                            
                                                            c("CT" = "CT", "DC" = "DC", "DE" = "DE", "MA" = "MA", "MD" = "MD",
                                                              "ME" = "ME", "NC" = "NC", "NH" = "NH", "NJ" = "NJ", "NY" = "NY",
                                                              "PA" = "PA", "RI" = "RI", "VA" = "VA", "VT" = "VT", "WV" = "WV"),
                                                            c("DC" = "DC", "DE" = "DE", "MD" = "MD",
                                                              "NC" = "NC", "NJ" = "NJ","PA" = "PA", 
                                                              "VA" = "VA", "WV" = "WV"),
                                                            TRUE),
                                         actionButton("selectall_states", label="Select/deselect all states"),
                                         
                                         checkboxGroupInput("Vehicle_type", 
                                                            
                                                            "Choose vehicle type(s) to display:",
                                                            
                                                            c("Combination Long-haul Trucks",
                                                              "Refuse Trucks",
                                                              "Transit Buses",
                                                              "Combination Short-haul Trucks",
                                                              "Intercity Buses",
                                                              "Passenger Cars",
                                                              "Single Unit Long-haul Trucks",
                                                              "Light Commercial Trucks",
                                                              "Passenger Trucks",
                                                              "Single Unit Short-haul Trucks"),
                                                            
                                                            c("Combination Long-haul Trucks"),
                                                            
                                                            TRUE),
                                         actionButton("selectall_vehicle", label="Select/deselect all vehicle types"),
                                         tableOutput("contents")
                                         
                                  )
                                  
                                )
                    ),
                   
                   tabPanel("2017 NEI VOC",
                            fluidRow(
                              
                              column(8,plotOutput("distPlotVOC", height = "825px")),
                              
                              column(4,
                                     sliderInput("percent_tampered_VOC",
                                                 "Percentage of fleet with tampered emissions control equipment (approx.):",
                                                 min = 0,
                                                 max = 100,
                                                 value = 0),
                                     numericInput("tampered_factor_VOC",
                                                  "Tampering increases VOC emissions by this factor (approx.):",
                                                  value = 1140),
                                     checkboxInput("ev_opt_VOC",
                                                   "Estimate electric vehicle offset?",
                                                   value = FALSE),
                                     sliderInput("percent_ev_VOC",
                                                 "Percentage of un-tampered fleet to be replaced with electric vehicles:",
                                                 min = 0,
                                                 max = 100,
                                                 value = 0),
                                     #numericInput("scale_pm",
                                     #            "Y-axis max limit:",
                                     #            value = 1000),
                                     
                                     
                                     
                                     checkboxGroupInput("States_VOC", 
                                                        
                                                        "Choose state(s) to display:",
                                                        
                                                        c("CT" = "CT", "DC" = "DC", "DE" = "DE", "MA" = "MA", "MD" = "MD",
                                                          "ME" = "ME", "NC" = "NC", "NH" = "NH", "NJ" = "NJ", "NY" = "NY",
                                                          "PA" = "PA", "RI" = "RI", "VA" = "VA", "VT" = "VT", "WV" = "WV"),
                                                        c("DC" = "DC", "DE" = "DE", "MD" = "MD",
                                                          "NC" = "NC", "NJ" = "NJ","PA" = "PA", 
                                                          "VA" = "VA", "WV" = "WV"),
                                                        TRUE),
                                     actionButton("selectall_states_VOC", label="Select/deselect all states"),                  
                                     checkboxGroupInput("Vehicle_type_VOC", 
                                                        
                                                        "Choose vehicle type(s) to display:",
                                                        c("Combination Long-haul Trucks",
                                                          "Refuse Trucks",
                                                          "Transit Buses",
                                                          "Combination Short-haul Trucks",
                                                          "Intercity Buses",
                                                          "Passenger Cars",
                                                          "Single Unit Long-haul Trucks",
                                                          "Light Commercial Trucks",
                                                          "Passenger Trucks",
                                                          "Single Unit Short-haul Trucks"),
                                                        c("Combination Long-haul Trucks"),
                                                        TRUE),
                                     
                                     actionButton("selectall_vehicle_VOC", label="Select/deselect all vehicle types"),
                                     
                                     tableOutput("contents_VOC"))
                            )
                   ),
                   
                   tabPanel("2017 NEI PM2.5",
                            fluidRow(
                              
                              column(8,plotOutput("distPlotPM", height = "825px")),
                              
                              column(4,
                                     sliderInput("percent_tampered_pm",
                                                 "Percentage of fleet with tampered emissions control equipment (approx.):",
                                                 min = 0,
                                                 max = 100,
                                                 value = 0),
                                     numericInput("tampered_factor_pm",
                                                  "Tampering increases PM2.5 emissions by this factor (approx.):",
                                                  value = 40),
                                     checkboxInput("ev_opt_pm",
                                                   "Estimate electric vehicle offset?",
                                                   value = FALSE),
                                     sliderInput("percent_ev_pm",
                                                 "Percentage of un-tampered fleet to be replaced with electric vehicles:",
                                                 min = 0,
                                                 max = 100,
                                                 value = 0),
                                     #numericInput("scale_pm",
                                     #            "Y-axis max limit:",
                                     #            value = 1000),
                                     
                                     
                                     
                                     checkboxGroupInput("States_pm", 
                                                        
                                                        "Choose state(s) to display:",
                                                        
                                                        c("CT" = "CT", "DC" = "DC", "DE" = "DE", "MA" = "MA", "MD" = "MD",
                                                          "ME" = "ME", "NC" = "NC", "NH" = "NH", "NJ" = "NJ", "NY" = "NY",
                                                          "PA" = "PA", "RI" = "RI", "VA" = "VA", "VT" = "VT", "WV" = "WV"),
                                                        c("DC" = "DC", "DE" = "DE", "MD" = "MD",
                                                          "NC" = "NC", "NJ" = "NJ","PA" = "PA", 
                                                          "VA" = "VA", "WV" = "WV"),
                                                        TRUE),
                                     actionButton("selectall_states_pm", label="Select/deselect all states"),                  
                                     checkboxGroupInput("Vehicle_type_pm", 
                                                        
                                                        "Choose vehicle type(s) to display:",
                                                        c("Combination Long-haul Trucks",
                                                          "Refuse Trucks",
                                                          "Transit Buses",
                                                          "Combination Short-haul Trucks",
                                                          "Intercity Buses",
                                                          "Passenger Cars",
                                                          "Single Unit Long-haul Trucks",
                                                          "Light Commercial Trucks",
                                                          "Passenger Trucks",
                                                          "Single Unit Short-haul Trucks"),
                                                        c("Combination Long-haul Trucks"),
                                                        TRUE),
                                     
                                     actionButton("selectall_vehicle_pm", label="Select/deselect all vehicle types"),
                                     
                                     tableOutput("contents_pm"))
                            )
                   ),
                   
                   tabPanel("2011-2017 NEI NOx",
                            fluidRow(
                              
                              column(2,
                                     selectInput("States_allyears", 
                                                 
                                                 "Choose state to display:",
                                                 
                                                 c("CT" = "CT", "DC" = "DC", "DE" = "DE", "MA" = "MA", "MD" = "MD",
                                                   "ME" = "ME", "NC" = "NC", "NH" = "NH", "NJ" = "NJ", "NY" = "NY",
                                                   "PA" = "PA", "RI" = "RI", "VA" = "VA", "VT" = "VT", "WV" = "WV"),
                                                 NULL,
                                                 FALSE),
                                     
                                     selectInput("Vehicle_type_allyears", 
                                                 
                                                 "Choose vehicle type(s) to display:",
                                                 c("Combination Long-haul Trucks",
                                                   "Refuse Trucks",
                                                   "Transit Buses",
                                                   "Combination Short-haul Trucks",
                                                   "Intercity Buses",
                                                   "Passenger Cars",
                                                   "Single Unit Long-haul Trucks",
                                                   "Light Commercial Trucks",
                                                   "Passenger Trucks",
                                                   "Single Unit Short-haul Trucks"),
                                                 NULL,
                                                 FALSE),
                                     numericInput("tampered_factor_allyears",
                                                  "Tampering increases NOx emissions by this factor:",
                                                  value = 310)),
                              column(8,
                                     plotOutput("distPlot_allyears", height = "435px")),
                              
                              column(2,
                                     sliderInput("percent_tampered_2017",
                                                 "Percentage of fleet with tampered emissions control equipment in 2017:",
                                                 min = 0,
                                                 max = 100,
                                                 value = 0),
                                     sliderInput("percent_tampered_2014",
                                                 "Percentage of fleet with tampered emissions control equipment in 2014:",
                                                 min = 0,
                                                 max = 100,
                                                 value = 0),
                                     sliderInput("percent_tampered_2011",
                                                 "Percentage of fleet with tampered emissions control equipment in 2011:",
                                                 min = 0,
                                                 max = 100,
                                                 value = 0)
                                    #,
                                     
                                     #tableOutput("contents_allyears"))
                            )
                   )
                   ),
                   tabPanel("Info",
                            img(src = "EPA_estimated_increase_delete.png"),
                            uiOutput("info1"),
                            uiOutput("info2"),
                            uiOutput("info3"),
                            uiOutput("info4")
                            )
                  
  
  
 

  )
)
)


  

