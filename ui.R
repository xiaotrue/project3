#
#Jenny Xiao
# 
#
#   
#

library(shiny)
library(leaflet)

# Choices for drop-downs
vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)



  navbarPage("ShinyApp Project 3", id="nav",
           
           tabPanel( "About my Project",
                       
                     # Leftside 
                    fluidRow( 
                       titlePanel("About the data and Code "),
                       
                       a(href="http://www.rstudio.com",target="_blank","Link to RStudio"),
                       print("The objective of this project is to explore the Kaggle Movies dataset
                              and answers some interesting questions like which countries produce most movies, 
                              profitability analysis, kind of movies are most produced,
                              most produced genres etc. I want to explore few of the 28 columns in detail.")
                    
                    ), 
                    
                    fluidRow( 
                           titlePanel("What does this app do?")
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
                                    )
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
                                   selectInput("var",label="Choose a variable",choice=c("Score"=1,
                                                                                        "Population"=2,
                                                                                        "College"=3,
                                                                                        "Income"=4), selectize=FALSE),
                                   
                                  
                                   br(),
                                   downloadButton("downloadData", "Download")
                                 ),
                                 mainPanel(
                                   textOutput("info_s"),
                                   h3(textOutput("Summery_states")),
                                   
                                   plotOutput("scatterCollegeIncome_state", height = 250),
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
                           # end row end 2
                            
                            
                    ),##end of PCA
                                 
                                  
                     
                        tabPanel( "Supervised Modeling and Predicting"
                     
                                )
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

