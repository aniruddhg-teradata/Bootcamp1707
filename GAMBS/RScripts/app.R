library(shiny)
require(shinydashboard)
library(dplyr)
library(igraph)
library(shinyBS)
library(leaflet)
library(shinyTime)
library(graphics)
library(scales)
library(RColorBrewer)
library (DT)
#library(RQuantLib)
#library(rPython)
library("colorspace")


load("./drugs.RData")
load("./locations.RData")


plot_map <- function(category){
  colors <- brewer.pal(n=11,name="RdYlGn")
  if(max(as.numeric(paste(category)),na.rm = T) < 1) cols <- (round((max(as.numeric(paste(category)),na.rm = T)-as.numeric(paste(category)))*200,0))+1
  else      cols <- (round((max(as.numeric(paste(category)),na.rm = T)-as.numeric(paste(category))),0))+1
  leaflet() %>% addTiles() %>% addCircleMarkers(lng = as.numeric(paste(loc$long)),lat = as.numeric(paste(loc$lat)),
                                                fill = TRUE,
                                                color = 0,
                                                fillColor = colors[cols],
                                                popup = loc$region,
                                                radius = as.numeric(paste(loc$population))/200000,
                                                fillOpacity = 0.8
                                                ) 
}



server <- function(input, output,session) {
  ##############################################################################
  # reactive functions
  ##############################################################################
  # list containing variables that can be read and modified by the dashboard
  
  output$plot_map_murder <- renderLeaflet({
    plot_map(loc$murder)
  })
  
  output$plot_map_drugs <- renderLeaflet({
    plot_map(loc$drugs)
  })
  
  output$plotdrugs <- renderPlot({
    p<- ggplot() +  geom_line(aes(x= drugsNORM_TOT$YEAR,y=drugsNORM_TOT$RATE,group = drugsNORM_TOT$REGION,color = drugsNORM_TOT$REGION,size=2)) + 
      xlab("Year") + ylab("Drug abuse")
    p
  })
  
  output$plotmurder <- renderPlot({
    p<- ggplot() +  geom_line(aes(x= murderReg_TOT$YEAR,y=murderReg_TOT$RATE,group = murderReg_TOT$REGION,color = murderReg_TOT$REGION,size=2)) + 
      xlab("Year") + ylab("Murder rate")
    p
  })
  
  output$choseregionDrugs <- renderUI({
    choseregionDrugs <- selectizeInput('drugregion',"Select a region... ",choices  = c("...",loc$region))
  })
  
  output$choseregionMurder <- renderUI({
    choseregionMurder <- selectizeInput('murderregion',"Select a region... ",choices  = c("...",loc$region))
  })
  
  
  
}


sidebar <- dashboardSidebar(
  sidebarMenu(
    # tabs with main functionalities
    menuItem('Introduction', tabName = 'intro', icon = icon('link'),selected = T),
    menuItem('Murder', tabName = 'murder', icon = icon('link')),
    menuItem('Drug Abuse', tabName = 'drugs', icon = icon('link'))
  )
)


# dashboard body
body <- dashboardBody(
  # tabItems correspond to entries in the sidebar
  tabItems(
  tabItem(tabName = 'murder',
            mainPanel(
              fluidRow(width=12,height = 12,
                (HTML("<h2>Murder\n<p>")),
                leafletOutput('plot_map_murder',height = 700,width = 1200 ),
                HTML("<br/> <h4>"),
                #plotOutput("plotmurder",height = 500,width = 1200 ),
                column(width = 12,
                       box(uiOutput('choseregionMurder'),
                           actionButton('give_murder',"Calculate Prediction")
                       ))
              )
            )
  ),
  tabItem(tabName = 'drugs',
          mainPanel(
            fluidRow(
              (HTML("<h2>Drug Abuse\n<p>")),
              leafletOutput('plot_map_drugs',height = 700,width = 1200 ),
              HTML("<br/><h4> "),
              #plotOutput("plotdrugs",height = 500,width = 1200 )
              column(width = 12,
                     box(uiOutput('choseregionDrugs'),
                         actionButton('give_drugs',"Calculate Prediction")
                              ))
            )
          )
  ),
 tabItem(tabName = "intro",
         titlePanel("Introduction"),
          mainPanel(
            fluidRow(
              
                htmlOutput('introText')
            )
          )
          )
 )
)



ui <- dashboardPage(dashboardHeader(title = 'InSights'),
                    skin = 'yellow',
                    sidebar = sidebar, 
                    body = body)

shinyApp(ui = ui, server = server )
