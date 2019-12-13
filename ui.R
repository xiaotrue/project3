#
#Jenny Xiao
# 
#
#   
#

library(shiny)
library(leaflet)
library(plotly)


# Choices for drop-downs
vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)



  navbarPage("ShinyApp Project 3", id="nav",
                    tabPanel("About my Project",   
                       fluidRow(column(12,
                                includeHTML("project3.html"),
                                br()
                                
                           ),
                      h2("The following is some mathematics about the linear regression."),
                      fluidRow(uiOutput('ex1'))
                           
                       )
                       
           ),
           navbarMenu("Data",
                      tabPanel("Data explorer",
                                  fluidRow(
                                            column(3,selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
                                            ),
                                            column(3,conditionalPanel("input.states",
                                                                        selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
                                                                      )
                                            ),
                                            column(3,conditionalPanel("input.states",
                                                                        selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
                                                                      )
                                            )
                                  ),
                                  fluidRow(
                                    column(1,
                                           numericInput("minScore", "Min score", min=0, max=100, value=0)
                                    ),
                                    column(1,
                                           numericInput("maxScore", "Max score", min=0, max=100, value=100)
                                    ),
                                    br(),
                                    br(),
                                    downloadButton("downloadData", "Download")
                                  ),
                               hr(),
                               
                               DT::dataTableOutput("ziptable")
                              
                                ),
                      tabPanel("Data Summeries",
                               sidebarLayout( position = "right",fluid = TRUE,
                                
                                 sidebarPanel(
                                  
                                   selectInput("Summery_states", "Select a State", 
                                               c(structure(state.abb, names=state.name), 
                                                 "Washington, DC"="DC"), multiple=FALSE),
                                   selectInput("var",label="Choose a variable",choice=c("Score",
                                                                                        "Population",
                                                                                        "College",
                                                                                        "Income"), selectize=FALSE),
                                   
                                  
                                   br()
                                 ),
                                 mainPanel(
                                   
                                   h2("What you can do in this page:", style = "color:blue;"),
                                   h3("When you select a State and a variable", style = "color:blue;"),
                                   h3("1. The Scatter Plot shows the Correlation Between Income and College, you can find that they are linearly correlated.",style = "color:blue;"),
                                   h3("2. For the histogram, I use the Ploty package, you can click and select a region of the histogram and hover it.
                                          You can download the plot as a png by clicking the camera button on the upper corrner", style = "color:blue;"),
                                   h3("3. The box plot shows the common numeric summery of the selected variable, you can see others by changing the selections", style = "color:blue;"),
                                   h3("4. The total variable numeric summeries were showed in the table", style = "color:blue;"),
                                   plotOutput("scatterCollegeIncome_state", height = 250),
                                   plotlyOutput("Hist",height=520,width=1200),
                                   h3(textOutput("var", container = span)),
                                   plotOutput("box"),
                                   
                                  verbatimTextOutput("summerytable") 
                                   
                                 )
                               )
                               )
           ),
           navbarMenu("Data Analysis",
                  tabPanel( "PCA Analysis",
                            h1("PCA Analysis"),
                            fluidRow(
                                    checkboxInput(inputId = 'center',
                                                  label = h4('Shift variables to be zero-centered', style = "color:red;"),
                                                  value = TRUE),
                                    checkboxInput(inputId = 'scale_data',
                                                  label = h4('Scale variables to have unit variance', style = "color:red;"),
                                                  value = TRUE)
                                  ),
                                  
                            br(),
                            h2("Scree Plot"),
                            fluidRow(column(8,
                                           plotOutput("SCREE_PLOT", height = "300px")
                                  ),
                                  column(4,
                                         numericInput("pc_range",
                                                      "Number of PCs to plot",
                                                      value=4,
                                                      min = 1,
                                                      max = 4,
                                                      width= '120px') 
                                  )
                               ),
                            br(),
                            #start 2
                            h2("Biplot"),
                              fluidRow(column(8,
                                              plotOutput ("PCA_PLOT", height = 400,
                                                          brush = brushOpts(
                                                            id = "PCA_PLOTBrush",
                                                            resetOnNew = TRUE))
                                      ),
                                      column(4,
                                             wellPanel(
                                               
                                               uiOutput("the_pcs_to_plot_x"),
                                               uiOutput("the_pcs_to_plot_y"),
                                               checkboxInput(inputId = 'draw_ellipse',
                                                             label = 'Draw ellipse around groups',
                                                             value = TRUE)
                                       )
                                      
                                    )
                              )
                            
                    ),##end of PCA
              
                  tabPanel( "Supervised Modeling",
                            
                            h1("Supervised Modeling:"),
                                 sidebarPanel(
                                   
                                   selectInput("MLT", "Choose a Model:", choice=c("Linear regression"=1,
                                                                                  "Random Forest"=2,
                                                                                  "KNN"=3), selectize=FALSE),
                                   uiOutput("NumPredictors"),
                                   uiOutput("Predictors"),
                                   br()
                                  
                                 ),
                                 mainPanel(
                                   h2("Scatter explore"),
                                   h3("Please kindly wait for a while due to a scatter output in the first output"),
                                   plotOutput ("ScatterAllPairs", height = 500,width=500),
                                   verbatimTextOutput("Model") 
                                 ),
                            
                            
                            br() ###end 1

                      ), ## end of supervised Modeling
                  tabPanel("Predict",
                           sidebarPanel(
                             
                             selectInput("PredictMLT", "Choose a Model to predict:", choice=c("Linear regression"=1,
                                                                                              "Random Forest"=2,
                                                                                              "KNN"=3), selectize=FALSE),
                             
                             
                             numericInput("collegepredictor",label = "College percentdage (0-100):",
                                           value = NA,
                                          min=0,max = 100),
                             
                             numericInput("populationpredictor",label = "Adult population in the area:",
                                          value = NA,
                                          min=0,max = NA),
                             
                             br(),

                             actionButton("Go", "Submit")
                             
                           ),
                           mainPanel(
                             h2("Predict explore"),
                             verbatimTextOutput("predictOutput")
                             #dataTableOutput("predictOutput")
                           ),
                          br()       
                    
                  )## End of predict
                  
           ),
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 10, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("ZIP explorer"),
                                      
                                      selectInput("color", "Color", vars),
                                      selectInput("size", "Size", vars, selected = "adultpop"),
                                      conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                       # Only prompt for threshold when coloring or sizing by superzip
                                                       numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                      ),
                                      
                                      plotOutput("histCentile", height = 200),
                                      plotOutput("scatterCollegeIncome", height = 250)
                        ),
                        
                        tags$div(id="cite",
                                 'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
                        )
                    )
           ),
           conditionalPanel("false", icon("crosshair"))
  
)

