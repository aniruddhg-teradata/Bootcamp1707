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
library(ggplot2)
library(plyr)
#library(RQuantLib)
#library(rPython)
library("colorspace")


load("./drugs.RData")
load("./murder.RData")
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
                                                popup = paste(loc$region,"<br/> Poverty rate:",round(loc$poverty,1)),
                                                radius = as.numeric(paste(loc$population))/200000,
                                                fillOpacity = 0.8
  ) 
}

drugsNORM <- split.data.frame(drugsNORM_TOT,drugsNORM_TOT$REGION)
ind <- sapply(seq(1:length(drugNORM)), function(x) cbind(drugNORM[[x]]$RATE))
ind <- cbind(drugNORM[[1]]$YEAR,ind)
colnames(ind) <- c("YEAR",names(drugsNORM))
drugsNORM <- ind %>% as.data.frame
rm(ind)

murder_regions <- split.data.frame(murderReg_TOT,murderReg_TOT$REGION)
ind <- sapply(seq(1:length(murder_regions)), function(x) cbind(murder_regions[[x]]$RATE))
ind <- cbind(murder_regions[[1]]$YEAR,ind)
colnames(ind) <- c("Year",names(murder_regions))
murder_regions <- ind %>% as.data.frame
rm(ind)

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
                       HTML("<h2>Murder\n</h2><br/>
                            <h4><b>Circle size:</b> Population in the area, <b>Circle color:</b> Average Murder rate (1960-2014)"),
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
                HTML("<h2>Drug Abuse\n</h2><br/><h4><b>Circle size:</b> Population in the area, <b>Circle color:</b> Average Drug abuse rate (2004-2011)"),
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
                fluidRow(
                  box(width=12,
                      h2("Problem Statement", style="color:orange"),
                      h3("Build a analytic solution that predicts murder and crime rates for several metropolitan statistical areas (MSA) in the US.")
                  ),
                  box(width=12,
                      h2("Analytic Approach", style="color:orange"),
                      h3("Stochastic propagation processes have been employed to predict the crime and drug abuse rate in the different MSAs for the next 5 years.
                         The processes was trained by the historical data provided by the Federal Bureau of Investigation (FBI) and the Drug Abuse Warning Network (DAWN).")
                      ),
                  box(width=12,
                      h2("Business Benefit",style="color:orange"),
                      h3("Violent crime is of immense cost throughout the United States and was estimated $179 billion in government expenditures on police protection, judicial and legal activities, 
                         and corrections (US Department of Justice). Further, the economic cost of drug abuse in the United States was estimated at $180.9 billion in 2002 (Office of National Drug Control Policy).
                         
                         With the knowledge of future commited violent crime and the abuse of drugs these cost can be reduced dramatically.")
                      )
                      )
                      )
                  )
                  )
            )
            )

# tb_images <- system.file("extdata", "tb_images", package = "TBdesign")
# 
# div(style = "position: fixed; bottom: 35px; left: 35px;",
#     img(src = 'tb_images/tb_logo.png', width = 197)
# )


ui <- dashboardPage(dashboardHeader(title = 'InSights'),
                    #TBdesign::td_colors$primary["orange"],
                    skin = 'yellow',
                    sidebar = sidebar, 
                    body = body)

shinyApp(ui = ui, server = server )
