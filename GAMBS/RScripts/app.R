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
library(plotly)
#library(RQuantLib)
#library(rPython)
library("colorspace")


load("./drugs.RData")
load("./locations.RData")
load("./pred_drug.RData")
load("./pred_murder.RData")

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
  
  
  observeEvent(input$give_murder,{
  output$plotmurder <- renderPlot({
    print(input$murderregion)
    p<- ggplot() +  geom_line(aes(x= murder_regions$Year,y=murder_regions[[paste(input$murderregion)]]),size=1.2,color = "black") + xlab("Year") + ylab("Murder rate")   +  geom_line(aes(x= pred_murder$year,y=pred_murder[[paste(input$murderregion)]],size=2),color = "orange") 
    p
  })
  })
  
  observeEvent(input$give_drugs,{
    output$plotdrugs <- renderPlot({
      print(input$drugregion)
      p1<- ggplot() +  geom_line(aes(x= drugsNORM$YEAR,y=drugsNORM[[paste(input$drugregion)]]),size=1.2,color = "black") + xlab("Year") + ylab("Drug Abuse")   +  geom_line(aes(x= pred_drug$year,y=pred_drug[[paste(input$drugregion)]],size=2),color = "orange") 
      p1
    })
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
                box(uiOutput('choseregionMurder'),
                    actionButton('give_murder',"Calculate Prediction"),width = 3
                       ),
                plotOutput("plotmurder",height = 500,width = 800 )
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
              box(uiOutput('choseregionDrugs'),
                  actionButton('give_drugs',"Calculate Prediction"), width=3
                              ),
              plotOutput("plotdrugs",height = 500,width = 800 )
              
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
