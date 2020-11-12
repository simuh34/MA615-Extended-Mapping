library(tidyverse)
library(hurricaneexposuredata)
library(maps)
library(tmap)
library(sp)
library(sf)
library(viridis)
library(magrittr)
library(ggplot2)
library(ggpubr)
library(shiny)
library(DT)
library(shinydashboard)
library(rsconnect)
## data from Fema, select data which incident type being "Hurricane" and happened between 2009 to 2018
##df <- read.csv("C:/Users/32631/Desktop/A BU/A615/Fema/PublicAssistanceFundedProjectsDetails.csv", header = TRUE)
##df <- subset(df, df$incidentType == "Hurricane")
#summary(df$projectAmount)
#summary(df$federalShareObligated)
## delete data which is negative
##df <- subset(df, df$projectAmount > 0 & df$federalShareObligated > 0)
df <- read.csv("D:\\R project\\615\\Project-FEMA\\Shiny-FEMA\\df.csv", header = TRUE)
df$year <- as.numeric(substr(df$declarationDate, 1,4))
df <- subset(df, df$year>=2009 & df$year <= 2018)
df <- df %>% mutate(ID=str_c(state,county,sep = ","))
df$ID <- tolower(df$ID)
dy <- df # store the data frame for yearly figure



# unique(df$state)
counties_C <- c("alabama", "texas", "virgin islands of the U.S.", "north carolina", "massachusetts", "puerto rico", "new york", "virginia", "new hampshire", "maryland", "delaware", "west virginia", "louisiana", "florida", "new jersey", "vermont", "connecticut", "pennsylvania", "rhode island", "maine", "district of columbia", "mississippi", "ohio", "georgia", "south carolina", "american samoa", "hawaii") 
state_CB <- map_data("state", counties_C)
counties_CB<- map_data("county", counties_C)
de <- df


## the estimated total cost of Public Assistance grant project from 2009 to 2018
df_c <- df %>% 
  group_by(ID) %>% 
  summarize(projectAmount = sum(projectAmount))
df_c <- df_c %>% 
  mutate(`Estimated Total Cost (USD)` = cut(df_c$projectAmount, 
                                            breaks=c(0,100000,1000000,10000000,100000000,1000000000,10000000000,20000000000),
                                            include.lowest = TRUE))
ttMap <- st_as_sf(map("county",plot=F,fill=T))
df_c <- left_join(ttMap,df_c,by="ID")
df_c <- filter(df_c[, -2])


dy_2009 <- subset(dy, dy$year==2009)
dy_2009 <- dy_2009 %>% 
  group_by(ID) %>% 
  summarize(projectAmount = sum(projectAmount))
summary(dy_2009$projectAmount)
dy_2009 <- dy_2009 %>% 
  mutate(`Estimated Total Cost (USD)` = cut(dy_2009$projectAmount, 
                                            breaks=c(0,1000000,1500000,2000000,2500000,3000000,3500000),
                                            include.lowest = TRUE))
tMap <- st_as_sf(map("county",counties_C ,plot=F,fill=T))
dy_2009 <- left_join(tMap,dy_2009,by="ID")
dy_2009 <- filter(dy_2009[, -2])

dy_2010 <- subset(dy, dy$year==2010)
dy_2010 <- dy_2010 %>% 
  group_by(ID) %>% 
  summarize(projectAmount = sum(projectAmount))
summary(dy_2010$projectAmount)
dy_2010 <- dy_2010 %>% 
  mutate(`Estimated Total Cost (USD)` = cut(dy_2010$projectAmount, 
                                            breaks=c(0,100000,500000,1000000,5000000,10000000,15000000),
                                            include.lowest = TRUE))
tMap <- st_as_sf(map("county",counties_C ,plot=F,fill=T))
dy_2010 <- left_join(tMap,dy_2010,by="ID")
dy_2010 <- filter(dy_2010[, -2])




dy_2011 <- subset(dy, dy$year==2011)
dy_2011 <- dy_2011 %>% 
  group_by(ID) %>% 
  summarize(projectAmount = sum(projectAmount))
summary(dy_2011$projectAmount)
dy_2011 <- dy_2011 %>% 
  mutate(`Estimated Total Cost (USD)` = cut(dy_2011$projectAmount, 
                                            breaks=c(0,500000,1000000,5000000,10000000,50000000,100000000,500000000),
                                            include.lowest = TRUE))
tMap <- st_as_sf(map("county",counties_C ,plot=F,fill=T))
dy_2011 <- left_join(tMap,dy_2011,by="ID")
dy_2011 <- filter(dy_2011[, -2])

## the estimated total cost of Public Assistance grant project for 2012
dy_2012<- subset(dy, dy$year==2012)
dy_2012 <- dy_2012 %>% 
  group_by(ID) %>% 
  summarize(projectAmount = sum(projectAmount))
summary(dy_2012$projectAmount)
dy_2012 <- dy_2012 %>% 
  mutate(`Estimated Total Cost (USD)` = cut(dy_2012$projectAmount, 
                                            breaks=c(0,500000,1000000,5000000,10000000,50000000,100000000,500000000,1000000000,5000000000,10000000000),
                                            include.lowest = TRUE))
tMap <- st_as_sf(map("county",counties_C ,plot=F,fill=T))
dy_2012 <- left_join(tMap,dy_2012,by="ID")
dy_2012 <- filter(dy_2012[, -2])


dy_2013<- subset(dy, dy$year==2013)
dy_2013 <- dy_2013 %>% 
  group_by(ID) %>% 
  summarize(projectAmount = sum(projectAmount))
summary(dy_2013$projectAmount)
dy_2013 <- dy_2013 %>% 
  mutate(`Estimated Total Cost (USD)` = cut(dy_2013$projectAmount, 
                                            breaks=c(0,50000,100000,500000,1000000,5000000,10000000,50000000),
                                            include.lowest = TRUE))
tMap <- st_as_sf(map("county",counties_C ,plot=F,fill=T))
dy_2013 <- left_join(tMap,dy_2013,by="ID")
dy_2013 <- filter(dy_2013[, -2])

dy_2016<- subset(dy, dy$year==2016)
dy_2016 <- dy_2016 %>% 
  group_by(ID) %>% 
  summarize(projectAmount = sum(projectAmount))
summary(dy_2016$projectAmount)
dy_2016 <- dy_2016 %>% 
  mutate(`Estimated Total Cost (USD)` = cut(dy_2016$projectAmount, 
                                            breaks=c(0,50000,100000,500000,1000000,5000000,10000000,50000000,100000000,500000000),
                                            include.lowest = TRUE))
tMap <- st_as_sf(map("county",counties_C ,plot=F,fill=T))
dy_2016 <- left_join(tMap,dy_2016,by="ID")
dy_2016 <- filter(dy_2016[, -2])

dy_2017<- subset(dy, dy$year==2017)
dy_2017 <- dy_2017 %>% 
  group_by(ID) %>% 
  summarize(projectAmount = sum(projectAmount))
summary(dy_2017$projectAmount)
dy_2017 <- dy_2017 %>% 
  mutate(`Estimated Total Cost (USD)` = cut(dy_2017$projectAmount, 
                                            breaks=c(0,50000,100000,500000,1000000,5000000,10000000,50000000,100000000,500000000,1000000000,5000000000,10000000000,50000000000),
                                            include.lowest = TRUE))
tMap <- st_as_sf(map("county",counties_C ,plot=F,fill=T))
dy_2017 <- left_join(tMap,dy_2017,by="ID")
dy_2017 <- filter(dy_2017[, -2])

dy_2018<- subset(dy, dy$year==2018)
dy_2018 <- dy_2018 %>% 
  group_by(ID) %>% 
  summarize(projectAmount = sum(projectAmount))
summary(dy_2018$projectAmount)
dy_2018 <- dy_2018 %>% 
  mutate(`Estimated Total Cost (USD)` = cut(dy_2018$projectAmount, 
                                            breaks=c(0,50000,100000,500000,1000000,5000000,10000000,50000000,100000000,500000000,1000000000),
                                            include.lowest = TRUE))
tMap <- st_as_sf(map("county",counties_C ,plot=F,fill=T))
dy_2018 <- left_join(tMap,dy_2018,by="ID")
dy_2018 <- filter(dy_2018[, -2])

df_d <- df %>% 
  group_by(ID) %>% 
  summarize(federalShareObligated = sum(federalShareObligated))
df_d <- df_d %>% 
  mutate(`Public Assistance Grant Funding (USD)` = cut(df_d$federalShareObligated, 
                                                       breaks=c(0,100000,1000000,10000000,100000000,1000000000,10000000000,20000000000),
                                                       include.lowest = TRUE))
ttMap <- st_as_sf(map("county",plot=F,fill=T))
df_d <- left_join(ttMap,df_d,by="ID")
df_d <- filter(df_d[, -2])



dp_2009 <- subset(dy, dy$year==2009)
dp_2009 <- dp_2009 %>% 
  group_by(ID) %>% 
  summarize(federalShareObligated = sum(federalShareObligated))
summary(dp_2009$federalShareObligated)
dp_2009 <- dp_2009 %>% 
  mutate(`Public Assistance Grant Funding (USD)` = cut(dp_2009$federalShareObligated, 
                                                       breaks=c(0,500000,1000000,1500000,2000000,2500000),
                                                       include.lowest = TRUE))
tMap <- st_as_sf(map("county",counties_C ,plot=F,fill=T))
dp_2009 <- left_join(tMap,dp_2009,by="ID")
dp_2009 %<>% select(-federalShareObligated)

dp_2010 <- subset(dy, dy$year==2010)
dp_2010 <- dp_2010 %>% 
  group_by(ID) %>% 
  summarize(federalShareObligated = sum(federalShareObligated))
summary(dp_2010$federalShareObligated)
dp_2010 <- dp_2010 %>% 
  mutate(`Public Assistance Grant Funding (USD)` = cut(dp_2010$federalShareObligated, 
                                                       breaks=c(0,5000,10000,50000,100000,500000,1000000,5000000,10000000),
                                                       include.lowest = TRUE))
dp_2010 <- left_join(tMap,dp_2010,by="ID")
dp_2010 %<>% select(-federalShareObligated)

dp_2011 <- subset(dy, dy$year==2011)
dp_2011 <- dp_2011 %>% 
  group_by(ID) %>% 
  summarize(federalShareObligated = sum(federalShareObligated))
summary(dp_2011$federalShareObligated)
dp_2011 <- dp_2011 %>% 
  mutate(`Public Assistance Grant Funding (USD)` = cut(dp_2011$federalShareObligated, 
                                                       breaks=c(0,5000,10000,50000,100000,500000,1000000,5000000,10000000,50000000,100000000,500000000),
                                                       include.lowest = TRUE))
dp_2011 <- left_join(tMap,dp_2011,by="ID")
dp_2011 %<>% select(-federalShareObligated)

dp_2012 <- subset(dy, dy$year==2012)
dp_2012 <- dp_2012 %>% 
  group_by(ID) %>% 
  summarize(federalShareObligated = sum(federalShareObligated))
summary(dp_2012$federalShareObligated)
dp_2012 <- dp_2012 %>% 
  mutate(`Public Assistance Grant Funding (USD)` = cut(dp_2012$federalShareObligated, 
                                                       breaks=c(0,5000,10000,50000,100000,500000,1000000,5000000,10000000,50000000,100000000,500000000,1000000000,5000000000,10000000000),
                                                       include.lowest = TRUE))
dp_2012 <- left_join(tMap,dp_2012,by="ID")
dp_2012 %<>% select(-federalShareObligated)

dp_2013 <- subset(dy, dy$year==2013)
dp_2013 <- dp_2013 %>% 
  group_by(ID) %>% 
  summarize(federalShareObligated = sum(federalShareObligated))
summary(dp_2013$federalShareObligated)
dp_2013 <- dp_2013 %>% 
  mutate(`Public Assistance Grant Funding (USD)` = cut(dp_2013$federalShareObligated, 
                                                       breaks=c(0,5000,10000,50000,100000,500000,1000000,5000000,10000000,50000000),
                                                       include.lowest = TRUE))
dp_2013 <- left_join(tMap,dp_2013,by="ID")
dp_2013 %<>% select(-federalShareObligated)

dp_2016 <- subset(dy, dy$year==2016)
dp_2016 <- dp_2016 %>% 
  group_by(ID) %>% 
  summarize(federalShareObligated = sum(federalShareObligated))
summary(dp_2016$federalShareObligated)
dp_2016 <- dp_2016 %>% 
  mutate(`Public Assistance Grant Funding (USD)` = cut(dp_2016$federalShareObligated, 
                                                       breaks=c(0,5000,10000,50000,100000,500000,1000000,5000000,10000000,50000000,100000000,500000000),
                                                       include.lowest = TRUE))
dp_2016 <- left_join(tMap,dp_2016,by="ID")
dp_2016 %<>% select(-federalShareObligated)

dp_2017 <- subset(dy, dy$year==2017)
dp_2017 <- dp_2017 %>% 
  group_by(ID) %>% 
  summarize(federalShareObligated = sum(federalShareObligated))
summary(dp_2017$federalShareObligated)
dp_2017 <- dp_2017 %>% 
  mutate(`Public Assistance Grant Funding (USD)` = cut(dp_2017$federalShareObligated, 
                                                       breaks=c(0,10000,50000,100000,500000,1000000,5000000,10000000,50000000,100000000,500000000,1000000000,10000000000,50000000000),
                                                       include.lowest = TRUE))
dp_2017 <- left_join(tMap,dp_2017,by="ID")
dp_2017 %<>% select(-federalShareObligated)

dp_2018 <- subset(dy, dy$year==2018)
dp_2018 <- dp_2018 %>% 
  group_by(ID) %>% 
  summarize(federalShareObligated = sum(federalShareObligated))
summary(dp_2018$federalShareObligated)
dp_2018 <- dp_2018 %>% 
  mutate(`Public Assistance Grant Funding (USD)` = cut(dp_2018$federalShareObligated, 
                                                       breaks=c(0,10000,50000,100000,500000,1000000,5000000,10000000,50000000,100000000,500000000,1000000000),
                                                       include.lowest = TRUE))
dp_2018 <- left_join(tMap,dp_2018,by="ID")
dp_2018 %<>% select(-federalShareObligated)




dt_2009 <- subset(dy, dy$year==2009)
dt_2009 <- dt_2009 %>% 
  group_by(ID) %>% 
  summarize(projectAmount = sum(projectAmount))

dt_2010 <- subset(dy, dy$year==2010)
dt_2010 <- dt_2010 %>% 
  group_by(ID) %>% 
  summarize(projectAmount = sum(projectAmount))

dt_2011 <- subset(dy, dy$year==2011)
dt_2011 <- dt_2011 %>% 
  group_by(ID) %>% 
  summarize(projectAmount = sum(projectAmount))

dt_2012 <- subset(dy, dy$year==2012)
dt_2012 <- dt_2012 %>% 
  group_by(ID) %>% 
  summarize(projectAmount = sum(projectAmount))

dt_2013 <- subset(dy, dy$year==2013)
dt_2013 <- dt_2013 %>% 
  group_by(ID) %>% 
  summarize(projectAmount = sum(projectAmount))

dt_2016 <- subset(dy, dy$year==2016)
dt_2016 <- dt_2016 %>% 
  group_by(ID) %>% 
  summarize(projectAmount = sum(projectAmount))

dt_2017 <- subset(dy, dy$year==2017)
dt_2017 <- dt_2017 %>% 
  group_by(ID) %>% 
  summarize(projectAmount = sum(projectAmount))

dt_2018 <- subset(dy, dy$year==2018)
dt_2018 <- dt_2018 %>% 
  group_by(ID) %>% 
  summarize(projectAmount = sum(projectAmount))



dtt_2009 <- subset(dy, dy$year==2009)
dtt_2009 <- dtt_2009 %>% 
  group_by(ID) %>% 
  summarize(federalShareObligated = sum(federalShareObligated))

dtt_2010 <- subset(dy, dy$year==2010)
dtt_2010 <- dtt_2010 %>% 
  group_by(ID) %>% 
  summarize(federalShareObligated = sum(federalShareObligated))

dtt_2011 <- subset(dy, dy$year==2011)
dtt_2011 <- dtt_2011 %>% 
  group_by(ID) %>% 
  summarize(federalShareObligated = sum(federalShareObligated))

dtt_2012 <- subset(dy, dy$year==2012)
dtt_2012 <- dtt_2012 %>% 
  group_by(ID) %>% 
  summarize(federalShareObligated = sum(federalShareObligated))

dtt_2013 <- subset(dy, dy$year==2013)
dtt_2013 <- dtt_2013 %>% 
  group_by(ID) %>% 
  summarize(federalShareObligated = sum(federalShareObligated))

dtt_2016 <- subset(dy, dy$year==2016)
dtt_2016 <- dtt_2016 %>% 
  group_by(ID) %>% 
  summarize(federalShareObligated = sum(federalShareObligated))

dtt_2017 <- subset(dy, dy$year==2017)
dtt_2017 <- dtt_2017 %>% 
  group_by(ID) %>% 
  summarize(federalShareObligated = sum(federalShareObligated))

dtt_2018 <- subset(dy, dy$year==2018)
dtt_2018 <- dtt_2018 %>% 
  group_by(ID) %>% 
  summarize(federalShareObligated = sum(federalShareObligated))


###########################################







header <- dashboardHeader(title="Hurricane")
sidebar <- dashboardSidebar(
  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "sidebarmenu",
    menuItem("Data Display", tabName="db"),
    menuItem("EDA", tabName = "calculate"),
    menuItem("Project Amount", tabName = "search"),
    menuItem("Federal Share Obligated", tabName = "search1")
  )
)

body <- dashboardBody(
  
  tabItems(
    tabItem("db",NULL,
            fluidRow(
              DT::dataTableOutput("table1")
            )),
    tabItem("calculate",NULL,
            fluidRow(
              textOutput("text7"),tags$head(tags$style("#text7{color: green;
                                 font-size: 20px;
                            
                                 
                                 }"
              )),
              plotOutput("distPlot4"),
              textOutput("text3"),tags$head(tags$style("#text3{color: green;
                                 font-size: 20px;
                            
                                 
                                 }"
              )),
              plotOutput("distPlot6"),
              textOutput("text6"),tags$head(tags$style("#text6{color: green;
                                 font-size: 20px;
                            
                                 
                                 }"
              )))),
    tabItem("search",NULL,
            fluidRow(
              selectInput("year",label ="Choose the time of the hurricane:",
                          choices = sort(c(unique(as.character(df$year))))), 
              box(plotOutput("distPlot3"), width = 6), 
              box(textOutput("text1"),
                  tags$head(tags$style("#text1{color: green;
                                 font-size: 20px;
                            
                                 
                                 }"
                  )
                  ),
                  plotOutput("distPlot1"), width = 6),
              
              box(DT::dataTableOutput("table2"), width = 6)
              
              
              
            )),
    tabItem("search1",NULL,
            fluidRow(
              selectInput("year1",
                          label ="Choose the time of the hurricane:",
                          
                          choices = sort(c(unique(as.character(df$year))))),
              box(plotOutput("distPlot5"), width = 6),
              box(textOutput("text2"),
                  tags$head(tags$style("#text2{color: blue;
                                 font-size: 20px;
                            
                                 
                                 }"
                  )),
                  plotOutput("distPlot2"), width = 6),
              
              box(DT::dataTableOutput("table3"), width = 6)
              
            ))
  )
)



ui=dashboardPage(header, sidebar, body)	


server <- function(input, output){
  
  
  
  output$table1 <- DT::renderDataTable(DT::datatable({
    df
  }))
  
  
  dataset<- reactive({
    if (input$year== "2009"){
      dt_2009
    }else if(input$year== "2010"){
      dt_2010
    }else if(input$year== "2011"){
      dt_2011
    }else if(input$year=="2012"){
      dt_2012
    }else if(input$year== "2013"){
      dt_2013
    }else if(input$year== "2016"){
      dt_2016
    }else if(input$year== "2017"){
      dt_2017
    }else if(input$year== "2018"){
      dt_2018
    }
  })
  output$table2 <- renderDataTable({
    datatable(dataset())
  })
  plotinput <- reactive({
    switch(input$year,
           "2008" = tm_shape(dy_2009)+
             tm_polygons("Estimated Total Cost (USD)",  border.col = "grey",
                         lwd = 0.1, colorNA = NULL, style="cont",
                         title = "Estimated Total Cost (USD)",
                         palette = "Greens") + 
             tm_layout(main.title = 'Project_Amount 2009',main.title.position="center"),
           "2009" = tm_shape(dy_2010)+
             tm_polygons("Estimated Total Cost (USD)",  border.col = "grey",
                         lwd = 0.1, colorNA = NULL, style="cont",
                         title = "Estimated Total Cost (USD)",
                         palette = "Greens") + 
             tm_layout(main.title = 'Project_Amount 2010',main.title.position="center"),
           
           "2010" = tm_shape(dy_2010)+
             tm_polygons("Estimated Total Cost (USD)",  border.col = "grey",
                         lwd = 0.1, colorNA = NULL, style="cont",
                         title = "Estimated Total Cost (USD)",
                         palette = "Greens") + 
             tm_layout(main.title = 'Project_Amount 2010',main.title.position="center"),
           
           "2011" = tm_shape(dy_2011)+
             tm_polygons("Estimated Total Cost (USD)",  border.col = "grey",
                         lwd = 0.1, colorNA = NULL, style="cont",
                         title = "Estimated Total Cost (USD)",
                         palette = "Greens") + 
             tm_layout(main.title = 'Project_Amount 2011',main.title.position="center"),
           "2012" = tm_shape(dy_2012)+
             tm_polygons("Estimated Total Cost (USD)",  border.col = "grey",
                         lwd = 0.1, colorNA = NULL, style="cont",
                         title = "Estimated Total Cost (USD)",
                         palette = "Greens") + 
             tm_layout(main.title = 'Project_Amount 2012',main.title.position="center"),
           "2013" = tm_shape(dy_2013)+
             tm_polygons("Estimated Total Cost (USD)",  border.col = "grey",
                         lwd = 0.1, colorNA = NULL, style="cont",
                         title = "Estimated Total Cost (USD)",
                         palette = "Greens") + 
             tm_layout(main.title = 'Project_Amount 2013',main.title.position="center"),
           "2016" = 
             tm_shape(dy_2016)+
             tm_polygons("Estimated Total Cost (USD)",  border.col = "grey",
                         lwd = 0.1, colorNA = NULL, style="cont",
                         title = "Estimated Total Cost (USD)",
                         palette = "Greens") + 
             tm_layout(main.title = 'Project_Amount 2016',main.title.position="center"),
           
           "2017" = tm_shape(dy_2017)+
             tm_polygons("Estimated Total Cost (USD)",  border.col = "grey",
                         lwd = 0.1, colorNA = NULL, style="cont",
                         title = "Estimated Total Cost (USD)",
                         palette = "Greens") + 
             tm_layout(main.title = 'Project_Amount 2017',main.title.position="center"),
           
           
           "2018" = 
             tm_shape(dy_2018)+
             tm_polygons("Estimated Total Cost (USD)",  border.col = "grey",
                         lwd = 0.1, colorNA = NULL, style="cont",
                         title = "Estimated Total Cost (USD)",
                         palette = "Greens") + 
             tm_layout(main.title = 'Project_Amount 2018',main.title.position="center")
           
    ) })
  
  
  
  output$distPlot3 <- renderPlot({
    plotinput()
  })
  output$text3 <- renderText ({"This is the chart focus on the distribution of each state."})
  
  dataset1<- reactive({
    if (input$year1== "2009"){
      dtt_2009
    }else if(input$year1== "2010"){
      dtt_2010
    }else if(input$year1== "2011"){
      dtt_2011
    }else if(input$year1=="2012"){
      dtt_2012
    }else if(input$year1== "2013"){
      dtt_2013
    }else if(input$year1== "2016"){
      dtt_2016
    }else if(input$year1== "2017"){
      dtt_2017
    }else if(input$year1== "2018"){
      dtt_2018
    }
  })
  
  output$table3 <- renderDataTable({
    datatable(dataset1())
  })
  
  plotinput5 <- reactive({
    switch(input$year1,
           
           "2009" = tm_shape(dp_2009)+
             tm_polygons("Public Assistance Grant Funding (USD)",  border.col = "grey",
                         lwd = 0.1, colorNA = NULL, style="cont",
                         title = "Public Assistance Grant Funding (USD)",
                         palette = "Blues") + 
             tm_layout(main.title = 'Federal_Share_Obligated 2009',main.title.position="center"),
           
           "2010" = tm_shape(dp_2010)+
             tm_polygons("Public Assistance Grant Funding (USD)",  border.col = "grey",
                         lwd = 0.1, colorNA = NULL, style="cont",
                         title = "Public Assistance Grant Funding (USD)",
                         palette = "Blues") + 
             tm_layout(main.title = 'Federal_Share_Obligated 2010',main.title.position="center"),
           
           "2011" = tm_shape(dp_2011)+
             tm_polygons("Public Assistance Grant Funding (USD)",  border.col = "grey",
                         lwd = 0.1, colorNA = NULL, style="cont",
                         title = "Public Assistance Grant Funding (USD)",
                         palette = "Blues") + 
             tm_layout(main.title = 'Federal_Share_Obligated 2011',main.title.position="center"),
           "2012" = tm_shape(dp_2012)+
             tm_polygons("Public Assistance Grant Funding (USD)",  border.col = "grey",
                         lwd = 0.1, colorNA = NULL, style="cont",
                         title = "Public Assistance Grant Funding (USD)",
                         palette = "Blues") + 
             tm_layout(main.title = 'Federal_Share_Obligated 2012',main.title.position="center"),
           "2013" = tm_shape(dp_2013)+
             tm_polygons("Public Assistance Grant Funding (USD)",  border.col = "grey",
                         lwd = 0.1, colorNA = NULL, style="cont",
                         title = "Public Assistance Grant Funding (USD)",
                         palette = "Blues") + 
             tm_layout(main.title = 'Federal_Share_Obligated 2013',main.title.position="center"),
           "2016" = 
             tm_shape(dp_2016)+
             tm_polygons("Public Assistance Grant Funding (USD)",  border.col = "grey",
                         lwd = 0.1, colorNA = NULL, style="cont",
                         title = "Public Assistance Grant Funding (USD)",
                         palette = "Blues") + 
             tm_layout(main.title = 'Federal_Share_Obligated 2016',main.title.position="center"),
           
           "2017" = tm_shape(dp_2017)+
             tm_polygons("Public Assistance Grant Funding (USD)",  border.col = "grey",
                         lwd = 0.1, colorNA = NULL, style="cont",
                         title = "Public Assistance Grant Funding (USD)",
                         palette = "Blues") + 
             tm_layout(main.title = 'Federal_Share_Obligated 2017',main.title.position="center"),
           
           
           "2018" = 
             tm_shape(dp_2018)+
             tm_polygons("Public Assistance Grant Funding (USD)",  border.col = "grey",
                         lwd = 0.1, colorNA = NULL, style="cont",
                         title = "Public Assistance Grant Funding (USD)",
                         palette = "Blues") + 
             tm_layout(main.title = 'Federal_Share_Obligated 2018',main.title.position="center")
           
    ) })
  
  
  output$distPlot5 <- renderPlot({
    plotinput5()
  })
  
  
  
  output$distPlot1 <- renderPlot({
    tm_shape(df_c)+
      tm_polygons("Estimated Total Cost (USD)",  border.col = "grey",
                  lwd = 0.1, colorNA = NULL, style="cont",
                  title = "Estimated Total Cost (USD)",
                  palette = "Greens") + 
      tm_layout(main.title = 'Project_Amount 2009-2018',main.title.position="center")
  })
  output$text1 <- renderText ({"The following plot shows the total cost of public assistance programs in each state from 2009 to 2018."})
  
  
  
  
  output$distPlot2 <- renderPlot({
    tm_shape(df_d)+
      tm_polygons("Public Assistance Grant Funding (USD)",  border.col = "grey",
                  lwd = 0.1, colorNA = NULL, style="cont",
                  title = "Public Assistance Grant Funding (USD)",
                  palette = "Blues") + 
      tm_layout(main.title = 'Federal_Share_Obligated 2009-2018',main.title.position="center")
  })
  
  
  
  
  
  output$text2 <- renderText ({"  The following plot concludes the public assistance Grant funding (USD) of each state from 2009 to 2018."})
  
  
  output$distPlot4 <- renderPlot({
    dd <- df
    damagecategorycount <- dd %>% group_by(damageCategory) %>%     summarize(count=sum(damageCategory!= "0"))
    damagecategorycount <- damagecategorycount[order(damagecategorycount$count), ]
    damagecategorycount$percent_value = round(damagecategorycount$count/sum(damagecategorycount$count) * 100)
    damagecategorycount$labs <- paste0(damagecategorycount$damageCategory, " (", damagecategorycount$percent_value, "%)")
    ggdonutchart(damagecategorycount, "count",
                 label = "labs",
                 fill = "damageCategory",
                 lab.adjust = 0,
                 lab.font = c(2, "bold", "grey"),
                 color = "white",
                 palette = "Blues" ) + 
      coord_polar(theta = "y", start = 0, clip = "off")})
  
  output$distPlot6 <- renderPlot({
    ds <- df
    ## calculate the frequency distribution of damage in different states
    statecount <- ds %>% group_by(state) %>% summarize(count=sum(state!= "0"))
    statecount <- statecount[order(statecount$count), ]
    # Georgia 2692
    # Vermont	3218			
    # North Carolina	6162			
    # Texas	6914			
    # Puerto Rico	7704			
    # New Jersey	9576			
    # Florida	10979			
    # New York	13115	
    ## calculate the sum of frequency of rest of the states
    topstate <- statecount[20:27,]
    names(topstate) <- c("state","count")
    restcount <- sum(statecount$count)-sum(topstate$count)
    reststate <- data.frame("Rest States",restcount)
    names(reststate)<-c("state","count")
    newstate <- rbind(topstate, reststate)
    newstate$percent_value = round(newstate$count/sum(newstate$count) * 100)
    newstate$labs <- paste0(newstate$state, " (", newstate$percent_value, "%)")
    ggdonutchart(newstate, "count",
                 label = "labs",
                 fill = "state",
                 lab.adjust = 0,
                 lab.font = c(4, "bold", "gray"),
                 color = "white",
                 palette = "Greens" )
  })
  output$text6 <- renderText({"This is a chart that calculate the frequency distribution of damage in different states."})
  
  output$text7 <- renderText({"This is the frequency  distribution of different damage category."})
  
  
  
  
  
}
shinyApp(ui = ui, server = server)

