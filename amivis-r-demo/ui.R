


ui <-fluidPage(theme = shinytheme("united"),
               # Header logo
               headerPanel(
                 title = HTML('<center><img src="amivislogo.png" width="325" height="225"></center>')
               ),
               
               # Input widgets
               fluidRow(
                 
                 # consider a new file with CSS
                 tags$style(HTML('.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                                  background: #d9534f;
                                                  border-top: 1px solid #d9534f ;
                                                  border-bottom: 1px solid #d9534f ;}'
                                 
                 )),
                 tags$style(HTML('.js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {
                                                  background: #d9534f;
                                                  border-top: 1px solid #d9534f ;
                                                  border-bottom: 1px solid #d9534f ;}'
                                 
                 )),
                 tags$style(".btn-file { background-color:#d9534f; 
                            border-color: #d9534f; 
                          }
                .progress-bar {
                            background-color: #d9534f;
                              }
               "
                            
                 ),
                 
               ),
               
               # Left Side Panels
               column(5,
                      column(2,align="center",
                             selectInput("district_choice", "District and County", 
                                         choices = list("ALL" = 1, "Porto" = 2,
                                                        "Aveiro" = 3), selected = 1),
                             selectInput("county_choice", NULL,
                                         choices = list("ALL", "S.Joao da Madeira",
                                                        "Choice 3"), selected = 1),
                             
                      ),
                      column(3,align="center",
                             sliderTextInput(
                               inputId = "yearStartSlider_choice",
                               label = "Interval of Symptoms",
                               choices = seq(minYear, maxYear, by=1),
                               selected = minYear,
                             ),
                             sliderTextInput(
                               inputId = "yearEndSlider_choice",
                               label = NULL,
                               choices = seq(minYear, maxYear, by=1),
                               selected = maxYear
                             )
                      ),
                      column(2,
                             linebreaks(2),align="center",
                             actionGroupButtons(
                               inputIds = c("yearStartTextMinus", "yearStartTextPlus"),
                               labels = list(icon("minus"),icon("plus")),
                               status = "danger",
                               size = "xs"
                             ),
                             linebreaks(2),align="center",
                             actionGroupButtons(
                               inputIds = c("yearEndTextMinus", "yearEndTextPlus"),
                               labels = list(icon("minus"),icon("plus")),
                               status = "danger",
                               size = "xs"
                             ),
                             
                      ),
                      column(1,align="center",
                             awesomeRadio(
                               inputId = "location_choice",
                               label = "Location",
                               choices = c("Origin", "Residence"),
                               inline = F,
                               status = "danger"
                               
                             )
                      ),
                      column(1,
                             
                      ),
                      
                      column(1,align="center",
                             linebreaks(2),align="center",
                             switchInput(
                               inputId = "dateless",
                               label = "DATELESS",
                               onStatus = "success", 
                               offStatus = "danger",
                               size = "normal",
                               value = FALSE,
                             )
                      ),
                      
                      column(12,h6("Please add a file and click a site"),shinycssloaders::withSpinner(leaflet::leafletOutput("map", height="550px"),size=2, color="#d9534f"))
                      
                      
               ),
               
               # Right Side Panels
               column(7,tabsetPanel(id="plot_tabs",
                                    tabPanel("Intro",
                                             h3(tags$b("AmiVis' Goal")),
                                             h5("AmiVis aims to enable a geographical, statistical exploration of data concerning TTR-FAP disease. The application allows changing parameters according to the user's will in order to obtain representations of the filtered data and, through a visual comparison, draw conclusions."),
                                             h5("AmiVis is divided into several panels and has a menu for changing parameters."),
                                             h5("It is possible to switch between the origin and the residence of the patients, choose the interval (starting and ending year) of onset of symptoms, as well as the district and municipality in relation to the origin or residence. The choice of these parameters implies, or not, of individuals for the geographic representation of the global dataset versus the filtered dataset."),
                                             h3("Load Files"),
                                             h5("This panel allows you to upload .csv files related to the origin and residence of patients. The .csv must have the following columns named: 'famN', 'yearOfBirth', 'yearOfDeath', 'sex', 'mother_id', 'father_id', 'yearofBeginning', 'district_ori', 'county_ori', 'district_res', and 'county_res'."),
                                             h3("Static GVis"),
                                             h5("In this panel there is a geographical visualisation of the data from the global dataset and the dataset filtered by the chosen parameters. The visualisation gathers the incidence of the disease in relation to the number of cases per location. The bigger the diameter of the circle, the higher the incidence. Red points represent locations with incidence above the 90th percentile. There is also a representation of Portugal by district and municipality, regarding the number of cases per territorial area."),
                                             h3("Overtime GVis"),
                                             h5("This panel allows the temporal visualization of data filtered by a time interval given by the user. Once chosen, it is possible to check the evolution of the data over each time interval (e.g. every 10 years). "),
                                             h3("Statistics"),
                                             h5("This panel allows the display of simple graphical representations that express data characteristics."),
                                             h3("External Factors"),
                                             h5("In this panel, disease data is crossed with external data allowing the visualisation of possible correlations between them."),
                                             h3("Table"),
                                             h5("The last panel allows the visualization of some columns of the data according to the filtered dataset."),
                                             
                                             ),
                                    tabPanel("Load Files",
                                             h4("Output 1"),
                                             verbatimTextOutput("txtout"),
                                             downloadButton("downloadcsv", "Download .csv"),
                                             fileInput("fileOri", "Choose CSV Origin File",
                                                       multiple = FALSE,
                                                       accept = c("text/csv",
                                                                  "text/comma-separated-values,text/plain",
                                                                  ".csv"),
                                                      
                                             ),
                                             
                                             fileInput("fileRes", "Choose CSV Residence file",
                                                       multiple = FALSE,
                                                       accept = c("text/csv",
                                                                  "text/comma-separated-values,text/plain",
                                                                  ".csv")
                                             ),
                                             column(1,
                                                    awesomeRadio(
                                                      inputId = "level_choice",
                                                      label = "Level",
                                                      choices = c("District", "County"),
                                                      inline = F,
                                                      status = "danger"
                                                    )
                                                    
                                             ),
                                    ),                
                                    tabPanel("Static GVis",
                                             tabsetPanel(id="static_tabs",
                                                         tabPanel("Geo Map",            
                                                                  column(6,h4("Incidence of all cases"),shinycssloaders::withSpinner(leaflet::leafletOutput("mapIncidence", height="600px"),size=2, color="#d9534f")),
                                                                  column(6,h4("Incidence of filtered cases"),shinycssloaders::withSpinner(leaflet::leafletOutput("mapIncidenceSelected", height="600px"),size=2, color="#d9534f")),
                                                         ),
                                                         tabPanel("Territorial District Map ",
                                                                  column(5, shinycssloaders::withSpinner(plotOutput("terrMapDistrictAll",width = "550px", height = "700px"),size=2, color="#d9534f")),
                                                                  column(5, shinycssloaders::withSpinner(plotOutput("terrMapDistrictFiltered",width = "550px", height = "700px"),size=2, color="#d9534f")),
                                                         ),
                                                         tabPanel("Territorial County Map ",
                                                                  column(5, shinycssloaders::withSpinner(plotOutput("terrMapCountyAll",width = "550px", height = "700px"),size=2, color="#d9534f")),
                                                                  column(5, shinycssloaders::withSpinner(plotOutput("terrMapCountyFiltered",width = "550px", height = "700px"),size=2, color="#d9534f")),
                                                         ),
                                             )
                                             
                                             
                                             
                                             
                                    ),
                                    tabPanel("Overtime GVis",
                                             column(2,align="center",
                                                    linebreaks(8),
                                                    h3("Interval of cases (years)",align="center",),
                                                    numericInput("interval", label = NULL, value = 10, min = 1, max = maxYear-minYear),
                                                    linebreaks(2),
                                                    actionBttn(
                                                      inputId = "check_interval",
                                                      label = NULL,
                                                      style = "material-circle", 
                                                      color  = "success",
                                                      icon = icon("check"),
                                                      size = "lg",
                                                      block = FALSE,
                                                    ),
                                                    linebreaks(2),
                                                    h3("Interval of Time"),
                                                    actionGroupButtons(
                                                      inputIds = c("intervalPrevious", "intervalNext"),
                                                      labels = list(icon("minus"),icon("plus")),
                                                      status = "danger",
                                                      size = "normal"
                                                    ),
                                                    linebreaks(2),
                                                    verbatimTextOutput("txtInterval"),
                                             ),
                                             column(9,align="center",
                                                    linebreaks(2),
                                                    tabsetPanel(id="overtime_tabs",
                                                                tabPanel("Geo Map", align="center",
                                                                         column(12,h4("Incidence of INTERVAL cases"),shinycssloaders::withSpinner(leaflet::leafletOutput("mapIncidenceInterval", height="550px"),size=2, color="#d9534f")),),
                                                                tabPanel("Territorial District Map", align="center",
                                                                         column(12, shinycssloaders::withSpinner(plotOutput("terrMapDistrictInterval",width = "450px", height = "600px"),size=2, color="#d9534f")),
                                                                ),
                                                                tabPanel("Territorial County Map", align="center",
                                                                         column(12, shinycssloaders::withSpinner(plotOutput("terrMapCountyInterval",width = "450px", height = "600px"),size=2, color="#d9534f")),
                                                                ),
                                                                
                                                    )
                                                    
                                                    
                                             ),
                                             
                                             
                                             
                                    ),
                                    tabPanel("Statistics",
                                             tabsetPanel(id="statistics_tabs",
                                                         tabPanel("Time Series",                          
                                                                  column(5, plotOutput("plotTimeSeriesAll",width = "450px", height = "350px")),
                                                                  column(5, plotOutput("plotTimeSeriesFiltered",width = "450px", height = "350px")),
                                                         ),
                                                         tabPanel("Decade Plots",
                                                                  column(5, plotOutput("plotDistrictAllDecades",width = "450px", height = "350px")),
                                                                  column(5, plotOutput("plotDistrictFilteredDecades",width = "450px", height = "350px")),
                                                         ),
                                                         tabPanel("Area Plots",
                                                                  column(5, plotOutput("plotDistrictsAll",width = "450px", height = "350px")),
                                                                  column(5, plotOutput("plotDistrictsFiltered",width = "450px", height = "350px")),
                                                         ),
                                                         
                                                         
                                                         
                                                         
                                             )
                                             
                                             
                                    ),
                                    tabPanel("External Factors",),
                                    tabPanel("Table",
                                             column(6,
                                                    h4("Choose the parameters."),
                                                    h4("Click a map's location to show only entries for that location."),
                                                    div(DT::dataTableOutput("table_input"), 
                                                        style = "font-size:80%"))
                                    ),
               )),
)