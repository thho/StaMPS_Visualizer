########################
##setup user interface##
##StaMPS-Visualizer   ##
########################

library(leaflet)
library(shinydashboard)
library(shinycssloaders)
library(lubridate)

# prepare UI
shinyUI(fluidPage(
  
  # load custom css stylesheet
  includeCSS("www/style.css"),
  
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
                       menuItem("Explanation", tabName = "expl", icon = icon("comment")),
                       menuItem("Baseline Plot", tabName = "blplot", icon = icon("project-diagram")),
                       menuItem("Manual", tabName = "lit", icon = icon("book-open")),
                       #menuItem("Literature", tabName = "lit", icon = icon("book")),
                       menuItem("Cite", tabName = "cite", icon = icon("graduation-cap"))
                     )# end dashboard Meu
    ),# end dashboard sidebar
    # begin dashboard body
    dashboardBody(
      tabItems(
        # begin visualizer map
        tabItem(tabName = "vismap",
                # map with PS time series tools
                leafletOutput("map") %>% withSpinner(color = "#2a84b5"),
                # begin control panel
                fixedPanel(id = "controls", class = "panel panel-default",
                           draggable = T, top = "auto", left = "auto", right = 47, bottom = 20,
                           width = 900, height = "800",
                           h2("Time series tools"),
                           fluidRow(column(3,
                                           selectInput("stusi", "Case Study", stusi,
                                                       selected = stusi[1], width = "200px")),
                                    column(4,
                                           dateInput('date',
                                                     label = 'Date of event',
                                                     value = Sys.Date())),
                                    column(4,
                                           selectInput('add.trend', 'Add Trendline',
                                                       choices = c('Connect MP' = 'ctrend',
                                                                   'Linear Trend' = 'ltrend',
                                                                   '2nd Order Polynomial Trend' = 'ptrend'), 
                                                       selected = 'ctrend'))
                           ),
                           fluidRow(column(3,
                                           selectInput('event.marker', label = 'Event Marker',
                                                       event, selected = event[1], width = "200px")),
                                    column(4,
                                           actionButton('sub.offset', label = 'Subtr. Offset'))
                                    ),
                           plotOutput("psts", height = 600, width = 800),
                           fluidRow(verbatimTextOutput("Click_text")) # enc control panel
                )
        )# end visualizer map
      ) # end body items
    ) # end dashboard body 
  ) # end dashboard page
)) # end UI