

ui <-fluidPage(theme = shinytheme("united"),
               # Header logo
               headerPanel(
                 windowTitle = "AmiVis",
                 title = ""
                 #title = HTML('<center><img src="amivislogo.png" width="365" height="205"></center>')
               ),
               
               # Input widgets
               fluidRow(
                 
                 # consider a new file with CSS
                 # style for year bars
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
                 #style for upload buttons and upload bar that shows
                 tags$style(".btn-file { background-color:#d9534f; 
                            border-color: #d9534f;
                            display:center-align; 
                          }
                .progress-bar {
                            background-color: #d9534f;
                            display:center-align;
                              }
               "
                            
                 ),
            
                   tags$style("body{
                   font-size: 16px !important;
                   }

               "
                            
                 ),
               ),
               
               # Left Side Panels
               column(5,
                      fluidRow(
                        column(4,  
                               HTML('<center><img src="amivislogo.png" width="60%" height="60%"></center>')),

                        column(3,align="center",
                               linebreaks(3),
                               awesomeRadio(
                                 inputId = "location_choice",
                                 label = "Location",
                                 choices = c("Origin", "Residence"),
                                 inline = F,
                                 status = "danger",
                                 checkbox = T,
                                 
                                 
                               )
                        ),
                        column(1,
                               
                        ),
                        
                        column(2, align="center",
                               linebreaks(3),
                               pickerInput(
                                 inputId = "sex_choice",
                                 label = "Sex", 
                                 choices = c("all", "male", "female","unknown"),
                                 options = list(
                                   style = "btn-danger")
                               ),
                               prettyCheckbox(
                                 inputId = "dateless",
                                 label = "Dateless", 
                                 value = TRUE,
                                 icon = icon("check"),
                                 status = "danger",
                                 animation = "rotate"
                               )
                        ),
                      ),
                      fluidRow(
                        column(4,align="center",
                               #placeholders
                               selectInput("district_choice", "District and County", 
                                           choices = list("ALL" = 1, "Porto" = 2,
                                                          "Aveiro" = 3), selected = 1, width = "200%"),
                               linebreaks(2),
                               
                               selectInput("county_choice", NULL,
                                           choices = list("ALL", "S.Joao da Madeira",
                                                          "Choice 3"), selected = 1),
                               
                        ),
                        column(8,align="center",
                               sliderTextInput(
                                 inputId = "yearStartSlider_choice",
                                 label = "Interval of Symptoms",
                                 choices = seq(minYear, maxYear, by=1),
                                 selected = minYear,
                               ),
                               actionGroupButtons(
                                 inputIds = c("yearStartTextMinus", "yearStartTextPlus"),
                                 labels = list(icon("minus"),icon("plus")),
                                 status = "danger",
                                 size = "xs"
                               ),
                              
                               sliderTextInput(
                                 inputId = "yearEndSlider_choice",
                                 label = NULL,
                                 choices = seq(minYear, maxYear, by=1),
                                 selected = maxYear
                               ),
                               actionGroupButtons(
                                 inputIds = c("yearEndTextMinus", "yearEndTextPlus"),
                                 labels = list(icon("minus"),icon("plus")),
                                 status = "danger",
                                 size = "xs"
                               ),
                        ),
                        
                      ),
                      
                      column(12,
                             h3("Unique Locations",style="text-align: center;" ),
                             shinycssloaders::withSpinner(leaflet::leafletOutput("map", height="450px"),size=2, color="#d9534f"))
                      
                      
               ),
               
               # Right Side Panels
               column(7,tabsetPanel(id="plot_tabs",
                                    tabPanel("About",
                                             linebreaks(1),
                                             h3(tags$b("AmiVis' Goal")),
                                             h5("AmiVis aims to enable a geographical, statistical exploration of data concerning TTR-FAP disease. The application allows changing parameters according to the user's will in order to obtain representations of the filtered data and, through a visual comparison, draw conclusions."),
                                             h5("AmiVis is divided into several panels and has a menu for changing parameters."),
                                             h5("It is possible to switch between the origin and the residence of the patients and their the sex, consider dateless recorrds, choose the interval (starting and ending year) of onset of symptoms, as well as the district and municipality in relation to the origin or residence. The choice of these parameters creates, or not, subsets of individuals for the geographic representation of the global dataset versus the filtered dataset."),
                                             h3("Load Files"),
                                             h5("This panel allows you to upload .csv files related to the origin and residence of patients. The .csv must have the following columns named: 'famN', 'yearOfBirth', 'yearOfDeath', 'sex', 'mother_id', 'father_id', 'yearofBeginning', 'district_ori', 'county_ori', 'district_res', and 'county_res'."),
                                             h3("Static GVis"),
                                             h5("In this panel there is a geographical visualisation of the data from the global dataset and the dataset filtered by the chosen parameters. The visualisation gathers the incidence of the disease in relation to the number of cases per location. The bigger the diameter of the circle, the higher the incidence. Red points represent locations with incidence above the 90th percentile. There is also a representation of Portugal by district and municipality, regarding the number of cases per territorial area."),
                                             h3("Overtime GVis"),
                                             h5("This panel allows the temporal visualization of data filtered by a time interval given by the user. Once chosen, it is possible to check the evolution of the data over each time interval (e.g. every 10 years). "),
                                             h3("Statistics"),
                                             h5("This panel allows the display of simple graphical representations that express data characteristics, such as information about the incidence for each Year, each district and each county as well the time series of the cases."),
                                             h3("Table"),
                                             h5("The last panel allows the visualization of some columns of the data according to the filtered dataset."),
                                             linebreaks(1),
                                             h3(tags$b("Acknowledgements")),
                                             h5("This project was developed during a Master's dissertation by Rúben X. Lôpo with the supervision of Alípio Jorge and the additional help of Maria Pedroto at Faculdade de Ciências da Universidade do Porto, DCC-FCUP and INESC TEC (LIAAD - Artificial Intelligence and Decision Support).
                                                 The code is available on GitHub, with a set of dummy data for testing, and is online at shinyapps.io."),
                                             h5("A paper 'Geovisualisation tools for reporting and monitoring Transthyretin-associated Familial Amyloid Polyneuropathy disease' was written and presented at The 7th Workshop on Data Science for Social Good held in conjunction with ECML PKDD 2022, with publication pending in a Springer journal."),
                                             h5("This work is financed by National Funds through the Portuguese funding agency, FCT - Fundação para a Ciência e a Tecnologia, within project LA/P/0063/2020."),
                                             HTML("<a href='https://www.linkedin.com/in/rubenlopo/'>Rúben X Lôpo</a>"),
                                             HTML("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),
                                             HTML("<a href='https://www.linkedin.com/in/alípio-jorge-29085813/'>Alípio Jorge</a>"),
                                             HTML("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),
                                             HTML("<a href='https://www.linkedin.com/in/mpedroto/'>Maria Pedroto</a>"),
                                             HTML("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),
                                             HTML("<a href='https://github.com/raluxu/AmiVis_Demo_Paper'>GitHub</a>"),
                                             HTML("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),
                                             HTML("<a href='https://raluxu.shinyapps.io/amivis-r-demo/'>shinyapps.io</a>"),
                                             
                                             ),
                                    tabPanel("Load Files",
                                             linebreaks(3),
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
                                             downloadButton("downloadcsv", "Download .csv"),
                                    ),                
                                    tabPanel("Static GVis",
                                             tabsetPanel(id="static_tabs",
                                                         tabPanel("Geo Map",
                                                                  linebreaks(2),
                                                                  column(6,h4("Incidence of all cases",style="text-align: center;"),shinycssloaders::withSpinner(leaflet::leafletOutput("mapIncidence", height="700px"),size=2, color="#d9534f")),
                                                                  column(6,h4("Incidence of filtered cases",style="text-align: center;"),shinycssloaders::withSpinner(leaflet::leafletOutput("mapIncidenceSelected", height="700px"),size=2, color="#d9534f")),
                                                         ),
                                                         tabPanel("Territorial District Map ",
                                                                  linebreaks(2),
                                                                  column(5, h4("Incidence of ALL cases for each District",style="text-align: center;"),shinycssloaders::withSpinner(plotOutput("terrMapDistrictAll", width = "130%", height = "700px"),size=2, color="#d9534f")),
                                                                  column(1,),
                                                                  column(5, h4("Incidence of FILTERED cases for each District",style="text-align: center;"),shinycssloaders::withSpinner(plotOutput("terrMapDistrictFiltered", width = "130%", height = "700px"),size=2, color="#d9534f")),
                                                         ),
                                                         tabPanel("Territorial County Map ",
                                                                  linebreaks(2),
                                                                  column(5, h4("Incidence of ALL cases for each County",style="text-align: center;"),shinycssloaders::withSpinner(plotOutput("terrMapCountyAll",width = "130%", height = "700px"),size=2, color="#d9534f")),
                                                                  column(5, h4("Incidence of FILTERED cases for each County",style="text-align: center;"),shinycssloaders::withSpinner(plotOutput("terrMapCountyFiltered",width = "130%", height = "700px"),size=2, color="#d9534f")),
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
                                                      size = "normal",
                                                      fullwidth = T,
                                                    ),
                                                    linebreaks(2),
                                                    verbatimTextOutput("txtInterval"),
                                             ),
                                             column(10,align="center",
                                                    linebreaks(2),
                                                    tabsetPanel(id="overtime_tabs",
                                                                tabPanel("Geo Map", align="center",
                                                                         column(12,h4("Incidence of INTERVAL cases",style="text-align: center;"),shinycssloaders::withSpinner(leaflet::leafletOutput("mapIncidenceInterval", height="650px"),size=2, color="#d9534f")),),
                                                                tabPanel("Territorial District Map", align="center",
                                                                         column(12, h4("Incidence of INTERVAL cases for each District",style="text-align: center;"),shinycssloaders::withSpinner(plotOutput("terrMapDistrictInterval",width = "450px", height = "600px"),size=2, color="#d9534f")),
                                                                ),
                                                                tabPanel("Territorial County Map", align="center",
                                                                         column(12,h4("Incidence of INTERVAL cases for each County",style="text-align: center;"), shinycssloaders::withSpinner(plotOutput("terrMapCountyInterval",width = "450px", height = "600px"),size=2, color="#d9534f")),
                                                                ),
                                                                
                                                    )
                                                    
                                                    
                                             ),
                                             
                                             
                                             
                                    ),
                                    tabPanel("Statistics",
                                             tabsetPanel(id="statistics_tabs",
                                                         tabPanel("Time Series",                          
                                                                  fluidRow( h4("Incidence for each Year for ALL data",style="text-align: center;"),plotOutput("plotTimeSeriesAll", height = "350px")),
                                                                  fluidRow( h4("Incidence for each Year for FILTERED data",style="text-align: center;"),plotOutput("plotTimeSeriesFiltered", height = "350px")),
                                                         ),
                                                         tabPanel("Decade Plots",
                                                                  fluidRow(h4("Incidence for each Decade for ALL data",style="text-align: center;"),plotOutput("plotDistrictAllDecades",height = "350px")),
                                                                  fluidRow(h4("Incidence for each Decade for FILTERED data",style="text-align: center;"),plotOutput("plotDistrictFilteredDecades",height = "350px")),
                                                         ),
                                                         tabPanel("District Area Plots",
                                                                  fluidRow(h4("Incidence for each District for ALL data",style="text-align: center;"),plotOutput("plotDistrictsAll",height = "350px")),
                                                                  fluidRow(h4("Incidence for each District for FILTERED data",style="text-align: center;"),plotOutput("plotDistrictsFiltered",height = "350px")),
                                                         ),
                                                         tabPanel("County Area Plots",
                                                                  fluidRow(h4("Incidence for each County for ALL data",style="text-align: center;"),plotOutput("plotCountyAll",height = "350px")),
                                                                  fluidRow(h4("Incidence for each County for FILTERED data",style="text-align: center;"),plotOutput("plotCountyFiltered",height = "350px")),
                                                         ),
                                                         
                                                         
                                                         
                                                         
                                                         
                                             )
                                             
                                             
                                    ),
                                    tabPanel("Table",
                                             column(6,
                                                    h4("Choose the parameters."),
                                                    h4("Click a map's location to show only entries for that location."),
                                                    div(DT::dataTableOutput("table_input"), 
                                                        style = "font-size:80%"))
                                    ),
               )),
)