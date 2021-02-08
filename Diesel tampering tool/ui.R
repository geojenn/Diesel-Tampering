
# reference: https://shiny.rstudio.com/articles/layout-guide.html

library(shiny)

shinyUI(fluidPage(theme = "style.css",
                  div(style = "padding: 1px 0px; width: '100%'",
                      titlePanel(
                        title = "",
                        windowTitle = "Tampering"
                      )
                  ),
                  navbarPage(
                    
                    # Application title.
                    title = div(span(img(src = "MARAMA_logo_small2.png"),
                                     "Diesel Vehicle Emissions Control Equipment Tampering - Annual Impact",
                                     style = "position: relative; top: 50%; transform: translateY(-50%);")),
                  
  
  
 tabPanel("NOx",
          fluidRow(
            
            column(2,
                   sliderInput("percent_tampered",
                               "Percentage of fleet with tampered emissions control equipment:",
                               min = 0,
                               max = 100,
                               value = 0),
                   numericInput("tampered_factor",
                                "Tampering increases NOx emissions by this factor:",
                                value = 1),
                   checkboxInput("scale_opt",
                                 "Change y-axis?",
                                 value = FALSE),
                   numericInput("scale",
                                "Y-axis max limit:",
                                value = 27000),
                   actionButton("selectall_states", label="Select/Deselect all"),
                   checkboxGroupInput("States", 
                                      
                                      "Choose state(s) to display:",
                                      
                                      c("CT" = "CT", "DC" = "DC", "DE" = "DE", "MA" = "MA", "MD" = "MD",
                                        "ME" = "ME", "NC" = "NC", "NH" = "NH", "NJ" = "NJ", "NY" = "NY",
                                        "PA" = "PA", "RI" = "RI", "VA" = "VA", "VT" = "VT", "WV" = "WV"),
                                      
                                      c("CT", "DC", "DE", "MA", "MD",
                                        "ME", "NC", "NH", "NJ", "NY",
                                        "PA", "RI", "VA", "VT", "WV"),
                                      TRUE)
                   
            
                   
            ),
            column(6,
                   plotOutput("distPlot", height = "570px"),
                   
                   actionButton("selectall_vehicle", label="Select/Deselect all"),
                   
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
                                      
                                      TRUE
                   )
                   
            ),
            column(4,
                   tableOutput("contents"))
          )
          
 ),
  tabPanel("PM2.5",
           fluidRow(
             
             column(2,
                    sliderInput("percent_tampered_pm",
                                "Percentage of fleet with tampered emissions control equipment:",
                                min = 0,
                                max = 100,
                                value = 0),
                    numericInput("tampered_factor_pm",
                                 "Tampering increases PM2.5 emissions by this factor:",
                                 value = 1),
                    checkboxInput("scale_opt_pm",
                                  "Change y-axis?",
                                         value = FALSE),
                    numericInput("scale_pm",
                                 "Y-axis max limit:",
                                  value = 1000),
                    
                    actionButton("selectall_states_pm", label="Select/Deselect all"),
                    
                    checkboxGroupInput("States_pm", 
                                       
                                       "Choose state(s) to display:",
                                       
                                       c("CT" = "CT", "DC" = "DC", "DE" = "DE", "MA" = "MA", "MD" = "MD",
                                         "ME" = "ME", "NC" = "NC", "NH" = "NH", "NJ" = "NJ", "NY" = "NY",
                                         "PA" = "PA", "RI" = "RI", "VA" = "VA", "VT" = "VT", "WV" = "WV"),
                                       
                                       c("CT", "DC", "DE", "MA", "MD",
                                         "ME", "NC", "NH", "NJ", "NY",
                                         "PA", "RI", "VA", "VT", "WV"),
                                       TRUE)
                    
             ),
           column(6,
                  plotOutput("distPlotPM", height = "570px"),
                  
                  actionButton("selectall_vehicle_pm", label="Select/Deselect all"),
                  
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
                                     
                                     TRUE
                  )
                  
           ),
           column(4,
                  tableOutput("contents_pm"))
           )
           
  ),
  tabPanel("Info",
           img(src = "EPA_estimated_increase_delete.png"),
           uiOutput("info")
           )

  )
)
)

  

