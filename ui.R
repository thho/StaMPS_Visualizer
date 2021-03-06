########################
##setup user interface##
##StaMPS-Visualizer   ##
########################

library(leaflet)
library(shinydashboard)
library(shinycssloaders)
library(shinyalert)

# prepare UI
shinyUI(fluidPage(
  
  # load custom css stylesheet
  includeCSS("www/style.css"),
  
  # use shiny alert for messages
  useShinyalert(),
  
  # load dashboard page layout
  dashboardPage(
    skin = "blue",
    dashboardHeader(title="StaMPS-Visualizer 3.0", titleWidth = 200),
    dashboardSidebar(width = 200,
                     sidebarMenu(HTML(paste0(
                       "<br>",
                       "<a href='https://github.com/thho/StaMPS_Visualizer' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='stamps-visualizer.png' width = '100'></a>",
                       "<br>",
                       "<p style = 'text-align: center;'><small><a href='https://github.com/thho/StaMPS_Visualizer' target='_blank'>3.0</a></small></p>",
                       "<table style='margin-left:auto; margin-right:auto;'>",
                       "<tr>",
                       "<td style='padding: 5px;'><a href='https://www.researchgate.net/publication/327939547_Analysing_the_Capabilities_and_Limitations_of_InSAR_using_Sentinel-1_Data_for_Landslide_Detection_and_Monitoring' target='_blank'><i class='fab fa-researchgate fa-lg'></i></a></td>",
                       "<td style='padding: 5px;'><a href='https://github.com/thho/StaMPS_Visualizer/blob/master/LICENSE.md' target='_blank'><i class='fab fa-creative-commons-by fa-lg'></i></a></td>",
                       "<td style='padding: 5px;'><a href='https://github.com/thho/StaMPS_Visualizer' target='_blank'><i class='fab fa-github fa-lg'></i></a></td>",
                       "</tr>",
                       "</table>",
                       "<br>")),
                       menuItem("Visualizer", tabName = "vismap", icon = icon("map marked alt")),
                       #menuItem("TS Explorer", tabName = "tsexp", icon = icon("chart-line")),
                       menuItem("Baseline Plot", tabName = "blplot", icon = icon("project-diagram")),
                       #menuItem("Data Manager", tabName = "datman", icon = icon("folder-open")),
                       #menuItem("Explanation", tabName = "expl", icon = icon("comment")),
                       menuItem("Settings", tabName = "settings", icon = icon("sliders-h")),
                       menuItem("Manual", tabName = "manual", icon = icon("book-open")),
                       #menuItem("Literature", tabName = "lit", icon = icon("book")),
                       menuItem("Cite", tabName = "cite", icon = icon("graduation-cap"))
                     )# end dashboard Meu
    ),# end dashboard sidebar
    # begin dashboard body
    dashboardBody(
      tabItems(
        # begin visualizer map tab item
        tabItem(tabName = "vismap",
                # map with PS time series tools
                leafletOutput("map") %>% withSpinner(color = "#2a84b5"),
                # begin container box for toggle controls action button
                fixedPanel(id = "cbox", class = "panel pael-default",
                           draggable = F, top = 75, left = "auto",
                           right = 50, bottom = "auto", width = 110, height = 30,
                           actionButton('plotBtn', 'Toggle Controls', "data-toggle"='collapse', "data-target"='#controls')),
                # begin collapsable control panel
                fixedPanel(id = "controls", class = "collapse",
                           draggable = T, top = 130, left = "auto", right = 35, bottom = 20,
                           width = 850, height = 900,
                           h4("Map tools"),
                           fluidRow(column(3,
                                           #selectInput("stusi", "Select Case Study", stusi,
                                                       #selected = stusi[1], width = "200px")
                                           uiOutput("stusi")
                                           ),
                                    column(3,
                                           sliderInput("pcex", label = "Point size",
                                                       min = 1, max = 10, value = 5)),
                                    column(4,
                                           fileInput("geojson", "Upload custom geometry (.geojson)",
                                              accept = ".geojson"))
                           ),
                           h4("Time series tools"),
                           fluidRow(column(3,
                                           dateInput('date',
                                                     label = 'Date of event',
                                                     value = Sys.Date())),
                                    column(4,
                                           selectInput('add.trend', 'Add trendline',
                                                       choices = c('Connect MP' = 'ctrend',
                                                                   'Linear trend' = 'ltrend',
                                                                   '2nd order polynomial trend' = 'ptrend'), 
                                                       selected = 'ctrend')),
                                    column(2,
                                           actionButton('sub.offset', label = 'Subtr. offset',
                                                        style = "margin-top: 25px;"))#,
                                    #column(3,
                                     #      actionButton('add.ts', label = 'Add point to TS-Selection',
                                      #                  style = "margin-top: 25px;"))
                                    
                                    ),
                           plotOutput("psts", height = 600, width = 800),
                           fluidRow(verbatimTextOutput("Click_text")) # end control panel inputs/outputs
                ) # end collapsable control panel
        ),# end visualizer map tab item
        # start manual tab item
        tabItem(tabName = "manual",
                includeMarkdown("www/manual.md")
        ), # end manual tab item
        # start manual tab item
        tabItem(tabName = "cite",
                includeMarkdown("www/cite.md")
        ), # end manual tab item
        tabItem(tabName = "blplot",
                fluidPage(
                  
                  # title
                  titlePanel("Baseline Plot"),
                  
                  # panel layout with input and output defs
                  sidebarLayout(
                    
                    # sidebar panel for Baseline Plot inputs
                    sidebarPanel(
                      
                      selectInput(inputId = "bl.file",
                                  label = "Select table with baseline data",
                                  choices = bl.info,
                                  selected = bl.info[1]),
                      # select SBAS or PS plot
                      radioButtons(inputId =  "blopt", 
                                   label = "DInSAR approach:",
                                   choiceNames = list(
                                     "Persistent Scatterer PS", "Small BAseline Subset SBAS"),
                                   choiceValues = list(
                                     "ps.a", "sbas.a"),
                                   selected = "ps.a"),
                      
                      # static input slider for temporal baseline
                      # sliderInput(inputId = "bl.temp",
                      #             label = "Temporal Baseline Threshold",
                      #             min = 1,
                      #             max = 365,
                      #             value = 48,
                      #             step = 1),
                      # 
                      
                      # dynamic input slider for PS SBAS option
                      uiOutput("bl.temp"),
                      
                      
                      # input slider for spatial baseline
                      sliderInput(inputId = "bl.spat",
                                  label = "Perpendicular baseline threshold",
                                  min = 1,
                                  max = 200,
                                  value = 200,
                                  step = 1)  
                      
                    ),
                    
                    # main panel for Baseline Plot tab
                    mainPanel(
                      # output baseline plot
                      plotOutput(outputId = "bl.plot"),
                      fluidRow(verbatimTextOutput("bl.text"))
                    )
                  )
                )), # end baseline plot tab
        tabItem(tabName = "settings",
                includeMarkdown("www/settings.md"),
                fluidRow(
                column(2, actionButton("load.def.settings",
                             label = "Load default settings"#,
                             #icon = "archive"
                             )),
                column(4, verbatimTextOutput("text.settings")),
                ),
                includeMarkdown("www/settings_maxnpoints.md"),
                fluidRow(
                  column(2, numericInput(inputId = "select.max.n.points",
                                         label = "max.n.points",
                                         value = settings$max.n.points,
                                         min = 1,
                                         max = 500000,
                                         width = "200px")),
                  column(2, actionButton("set.max.n.points",
                                         label = "Enable for current session",
                                         style = "margin-top: 25px;"
                                         #icon = "cogs"
                                         )),
                  column(2, actionButton("save.max.n.points",
                                         label = "Save to user settings",
                                         style = "margin-top: 25px;"
                                         #icon = "user-cog"
                                         )),
                  column(3, verbatimTextOutput("text.max.n.points"),
                         style = "margin-top: 25px;")
                )
              ) # end setting tab
      ) # end body items
    ) # end dashboard body 
  ) # end dashboard page
)) # end UI