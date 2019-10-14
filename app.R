#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(rgeos)
library(maptools)
library(ggmap)
library(broom)
library(leaflet)
library(lubridate)
library(tidyr)
library(mapview)
library(plotly)

# data: https://www.kaggle.com/cgurkan/airplane-crash-data-since-1908/download

setwd("C:\\Users\\Nigel\\Google Drive\\Classes\\data-vis-math-2270\\assignment3\\HistoryOfAviationCrashes")
#setwd("C:\\Users\\kychi\\google-drive\\Classes\\data-vis-math-2270\\assignment3\\HistoryOfAviationCrashes")

if(FALSE) {
    # register_google(key = "[your key]")
    # Process Raw Data
    data.raw <- read_csv("crashes.csv") %>%
        mutate(Date=as.Date(Date, format = "%m/%d/%Y")) %>% 
        drop_na(Location) %>%
        mutate(year = year(Date), 
               month = month(Date), 
               day = day(Date)) 
    
    # Geocode the Location column
    data <- mutate_geocode(data.raw, Location)
    
    #write_csv(data[is.na(data$lon),], "crashes_na.csv")
    #write_csv(data, "crashes_processed.csv")
} else {
    data <- read_csv("crashes_processed.csv")
}

# Registration for Special Historic Events
hist.civ.vector <- c("N736PA/PH-BUF","JA8119","HZ-AIH/UN-76435", "TC-JAV", "VT-EFO")
hist.mil.vector <- c("15-22","7T-WIP","NAF911","5A-DIA","60-0297")

nine.eleven.vector <- c("N334AA", "N612UA", "N644AA", "N591UA")
hist.civ.vector <- append(hist.civ.vector, nine.eleven.vector)

d.day.vector <- c("42-93095","42-100905","42-101035")
hist.mil.vector <- append(hist.mil.vector, d.day.vector)

# Init Colors
mil.spec.color <- "#D63E2A"
mil.color <- "darkred"
map.spec.mil.color <- "red"
map.mil.color <- "darkred"

civ.spec.color <- "#38A9DC"
civ.color <- "#0067A3"
map.spec.civ.color <- "blue"
map.civ.color <- "darkblue"

spec.color <- "black"
map.spec.color <- "black"

icon.default.color <- "black"
icon.spec.color <- "#FFFFFF"

# Remove repeated ground fatality measurement for 9/11
data[data$Registration %in% c("N334AA"),"Ground"] <- 0

data <- data %>%
    drop_na(lat) %>% 
    mutate(Fatalities = as.numeric(Fatalities), Ground = as.numeric(Ground)) %>%
    replace_na(list(Fatalities = 0, Ground = 0)) %>%
    mutate(icon=ifelse(grepl("military", tolower(Operator)), "fighter-jet", "plane"),
           color= ifelse(grepl("military", tolower(Operator)), 
                               map.mil.color, 
                               map.civ.color),
           icon.color= icon.default.color,
           Civillian=ifelse(grepl("military", tolower(Operator)),FALSE, TRUE),
           Military=grepl("military", tolower(Operator)),
           HistoricCiv=FALSE,
           HistoricMil=FALSE,
           CivillianF=ifelse(grepl("military", tolower(Operator)), 0, Fatalities),
           MilitaryF=ifelse(grepl("military", tolower(Operator)), Fatalities, 0),
           HistoricCivF=0,
           HistoricMilF=0,
           CivillianFG=ifelse(grepl("military", tolower(Operator)), 0, Fatalities+Ground),
           MilitaryFG=ifelse(grepl("military", tolower(Operator)), Fatalities+Ground, 0),
           HistoricCivFG=0,
           HistoricMilFG=0,
           DatedSummary=sprintf("%s: %s Passenger and Crew Fatalitiles: %s, Ground Fatalities: %d", Date, Summary, ifelse(Fatalities>0,as.character(Fatalities),"Not Available"), Ground))

data[data$Registration %in% hist.civ.vector,"color"] <- map.spec.civ.color
data[data$Registration %in% hist.civ.vector,"icon.color"] <- icon.spec.color

data[data$Registration %in% hist.mil.vector,"color"] <- map.spec.mil.color
data[data$Registration %in% hist.mil.vector,"icon.color"] <- icon.spec.color

# Move stats for historic events into histric columns
data[data$Registration %in% hist.civ.vector,"HistoricCiv"] <- TRUE
data[(data$Registration %in% hist.civ.vector) & (data$Civillian==TRUE),"HistoricCivF"] <- data[(data$Registration %in% hist.civ.vector) & (data$Civillian==TRUE),"CivillianF"]
data[(data$Registration %in% hist.civ.vector) & (data$Civillian==TRUE),"CivillianF"] <- 0
data[(data$Registration %in% hist.civ.vector) & (data$Civillian==TRUE),"HistoricCivFG"] <- data[(data$Registration %in% hist.civ.vector) & (data$Civillian==TRUE),"CivillianFG"]
data[(data$Registration %in% hist.civ.vector) & (data$Civillian==TRUE),"CivillianFG"] <- 0

data[data$Registration %in% hist.mil.vector,"HistoricMil"] <- TRUE
data[(data$Registration %in% hist.mil.vector) & (data$Military==TRUE),"HistoricMilF"] <- data[(data$Registration %in% hist.mil.vector) & (data$Military==TRUE),"MilitaryF"]
data[(data$Registration %in% hist.mil.vector) & (data$Military==TRUE),"MilitaryF"] <- 0
data[(data$Registration %in% hist.mil.vector) & (data$Military==TRUE),"HistoricMilFG"] <- data[(data$Registration %in% hist.mil.vector) & (data$Military==TRUE),"MilitaryFG"]
data[(data$Registration %in% hist.mil.vector) & (data$Military==TRUE),"MilitaryFG"] <- 0

#Slightly separate the 9/11 planes
data[data$Registration %in% c("N334AA"),"lon"] <- -74.001


data.ts <- data %>%
    group_by(year) %>%
    summarise(Military=sum(Military),
              Civillian=sum(Civillian),
              Historic.Military=sum(HistoricMil),
              Historic.Civilian=sum(HistoricCiv))

data.ts2 <- data %>%
    group_by(year) %>%
    summarise(Military=sum(MilitaryF),
              Civillian=sum(CivillianF),
              Historic.Military=sum(HistoricMilF),
              Historic.Civilian=sum(HistoricCivF))

data.ts3 <- data %>%
    group_by(year) %>%
    summarise(Military=sum(MilitaryFG),
              Civillian=sum(CivillianFG),
              Historic.Military=sum(HistoricMilFG),
              Historic.Civilian=sum(HistoricCivFG))

##########################################################################################
# add.markers function
##########################################################################################

add.markers <- function(p, y.max) {
    event.lines <- list()
    
    p <- add.marker(p, 1914, y.max, "World War I Begins", mil.spec.color)
    event.lines <- append(event.lines, list(make.line(1914,y.max,1918,y.max,mil.spec.color)))
    p <- add.marker(p, 1918, y.max, "World War I Ends", mil.spec.color)
    
    p <- add.marker(p, 1939, y.max, "World War II Begins", mil.spec.color)
    event.lines <- append(event.lines, list(make.line(1939,y.max,1945,y.max,mil.spec.color)))
    p <- add.marker(p, 1944, y.max, "Normanday Landings (D-day)", mil.spec.color)
    p <- add.marker(p, 1945, y.max, "World War II Ends", mil.spec.color)
    
    p <- add.marker(p, 1950, y.max, "Korean War Begins", mil.spec.color)
    event.lines <- append(event.lines, list(make.line(1950,y.max,1953,y.max,mil.spec.color)))
    p <- add.marker(p, 1953, y.max, "Korean War Ends", mil.spec.color)
    
    p <- add.marker(p, 1955, y.max, "Vietnam War Begins", mil.spec.color)
    event.lines <- append(event.lines, list(make.line(1955,y.max,1975,y.max,mil.spec.color)))
    p <- add.marker(p, 1975, y.max, "Vietnam War Ends", mil.spec.color)
    
    p <- add.marker(p, 1979, y.max, "Soviet-Afghan War Begins", mil.spec.color)
    event.lines <- append(event.lines, list(make.line(1979,y.max,1989,y.max,mil.spec.color)))
    p <- add.marker(p, 1989, y.max, "Soviet-Afghan War Ends", mil.spec.color)
    
    p <- add.marker(p, 1990, y.max, "Persian Gulf War Begins", mil.spec.color)
    event.lines <- append(event.lines, list(make.line(1990,y.max,1991,y.max,mil.spec.color)))
    p <- add.marker(p, 1991, y.max, "Persian Gulf War Ends", mil.spec.color)
    
    p <- add.marker(p, 2001, y.max*1.05, "War in Afghanistan Begins", mil.spec.color)
    event.lines <- append(event.lines, list(make.line(2001,y.max*1.05, 2019,y.max*1.05,mil.spec.color)))
    
    p <- add.marker(p, 2003, y.max, "\n1.Iraq War Begins\n2.Iran Ilyushin Il-76 Crash", mil.spec.color)
    event.lines <- append(event.lines, list(make.line(2003,y.max,2011,y.max,mil.spec.color)))
    p <- add.marker(p, 2011, y.max, "Iraq War Ends", mil.spec.color)
    
    # Military Event Markers
    p <- add.marker(p, 1968, y.max, "Kham Duc C-130 Shootdown", mil.spec.color)
    p <- add.marker(p, 1992, y.max, "\n1.Nigerian Air Force C-130 Crash\n2.Libyan Air Force/Airlines Collision", mil.spec.color)
    p <- add.marker(p, 2018, y.max, "Algerian Air Force Ilyushin Il-76 Crash", mil.spec.color)
    
    # Civillian Event Markers
    p <- add.marker(p, 1974, y.max, "Turkish Airlines Flight 981", civ.spec.color)
    p <- add.marker(p, 1977, y.max, "Tenerife Airport Disaster", civ.spec.color)
    p <- add.marker(p, 1985, y.max, "\n1.Japan Airlines Flight 123\n2.Air India Flight 182", civ.spec.color)
    p <- add.marker(p, 1996, y.max, "Charkhi Dadri Mid-Air Collision", civ.spec.color)
    p <- add.marker(p, 2001, y.max, "9/11 Terrorist Attacks", civ.spec.color)
    
    return(list(p, event.lines))
}

##########################################################################################
# make.line function
##########################################################################################

make.line <- function(x0, y0, x1, y1, clr) {
    temp.line <- list(
        type = "line",
        line = list(color = clr),
        xref = "x",
        yref = "y",
        x0 = x0,
        y0 = y0,
        x1 = x1,
        y1 = y1
    )
    
    return(temp.line)
}

##########################################################################################
# add.marker function
##########################################################################################

add.marker <- function(p, x, y, msg, clr) {
    p <- add_trace(
        p,
        x = x, y = y,
        text = sprintf("%d: %s", x, msg),
        hoverinfo = 'text',
        type = 'scatter',
        mode = "markers",
        marker = list(color=clr),
        showlegend = F
    )
    return(p)
}

##########################################################################################
# server definition
##########################################################################################

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    border-radius: 25px;
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))

title <- tags$div(
    tag.map.title, HTML("History of Aviation Crashes")
) 

# Define server logic
server <- function(input, output) {
    show_info <- 0
    # Base Map
    output$map <- renderLeaflet({
        leaflet(data) %>% 
            addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
            setView(lat = 39.2904, lng = -76.6122, zoom = 7) %>%
            addControl(title, position = "topleft", className="map-title")
    })
    
    # Plot Option
    plot.option <- reactive({input$plot.option})
    
    # Plot Data
    plot.data.choice <- reactive({input$plot.data})
    
    # Filter data by year
    data.reactive <- reactive({
      if(is.na(input$year) || input$year<min(data$year)) {
        y <- min(data$year)
      } else if(input$year>max(data$year)) {
        y <- max(data$year)
      } else {
        y <- input$year
      }
        
      data %>%
          filter(year == y)
    })
    
    # year
    year.reactive <- reactive({
        if(is.na(input$year) || input$year<min(data$year)) {
          min(data$year)
        } else if(input$year>max(data$year)) {
          max(data$year)
        } else {
          input$year
        }
    })
    
    # Crash count Time Series Plot
    output$ts <- renderPlotly({ 
        if(plot.option()==2) {
            title.string <- "Aviation Passenger and Crew Fatalities Over Time"
            plot.data <- data.ts2
        } else if(plot.option()==3) {
            title.string <- "Aviation Passenger, Crew and Ground Fatalities Over Time"
            plot.data <- data.ts3
        } else {
            title.string <- "Aviation Crashes Over Time"
            plot.data <- data.ts
        }
        
        if(plot.data.choice()==2) {
            p <- plot_ly(data=plot.data,
                         x=~year, y=~Military,
                         name="Military", type = 'bar', marker = list(color = mil.color)) %>%
                add_trace(p, data=plot.data, y = ~Historic.Military, 
                          name="Historic Military", marker = list(color =mil.spec.color))
            y.max <- max(plot.data$Military+plot.data$Historic.Military)
        } else if(plot.data.choice()==3) {
            p <- plot_ly(data=plot.data,
                         x=~year, y=~Civillian,
                         name="Civilian", type = 'bar', marker = list(color = civ.color)) %>%
                add_trace(p, data=plot.data, y = ~Historic.Civilian, 
                          name="Historic Civilian", marker = list(color =civ.spec.color))
            y.max <- max(plot.data$Civillian+plot.data$Historic.Civilian)
        } else {
            p <- plot_ly(data=plot.data, 
                         x=~year, y=~Military, 
                         name="Military", type = 'bar', marker = list(color = mil.color))  %>%
                add_trace(p, data=plot.data, y = ~Historic.Military, 
                          name="Historic Military", marker = list(color =mil.spec.color)) %>%
                add_trace(p, data=plot.data, y = ~Civillian, 
                          name="Civilian", marker = list(color =civ.color)) %>%
                add_trace(p, data=plot.data, y = ~Historic.Civilian, 
                          name="Historic Civilian", marker = list(color =civ.spec.color))
            y.max <- max(plot.data$Military+plot.data$Civillian+plot.data$Historic.Military+plot.data$Historic.Civilian)
        }
        
        year.line <- make.line(year.reactive(), 0, year.reactive(), y.max*1.09, "black")
        
        result <- add.markers(p, y.max+y.max*0.1)
        p <- result[[1]]
        p.shapes <- append(result[[2]], list(year.line))
        
        p <- layout(p, 
                    title = title.string,
                    barmode = 'stack',
                    xaxis = list(title="Year"),
                    yaxis = list(title="Count"),
                    plot_bgcolor='transparent',
                    paper_bgcolor='transparent',
                    shapes = p.shapes)
        
        p
    })
    
    # Place filtered markers
    observe({
        if(plot.data.choice()==2) {
            m.data <- data.reactive() %>% 
                filter(Military==TRUE)
        } else if(plot.data.choice()==3) {
            m.data <- data.reactive() %>% 
                filter(Civillian==TRUE)
        } else {
            m.data <- data.reactive()
        }
        
        if(nrow(m.data)>0) {
            leafletProxy("map", data = m.data) %>%
                clearMarkers() %>%
                addAwesomeMarkers(lng=~lon,
                                  lat=~lat,
                                  popup=~DatedSummary,
                                  label=~Operator,
                                  icon=makeAwesomeIcon(icon=~icon,
                                                       library="fa",
                                                       markerColor=~color,
                                                       iconColor=~icon.color))
        } else {
            leafletProxy("map") %>%
                clearMarkers()
        }
    })
    
    # Infor Panel Button
    show_info_flag <- reactive({input$midpanel})
    output[["info_panel"]] <- renderUI({
        if(show_info_flag()==3){
            absolutePanel(top = 0, left = 0, right = 0, bottom = 0, width =920, height=475, id = "info_panel", 
                          style="border-radius: 25px;
                         padding: 8px; 
                         border-bottom: 2px solid #CCC;
                         background: rgba(255,255,255,0.75);",
                          h3("Data Inclusions"),
                          p("1. All civil and commercial aviation accidents of scheduled and non-scheduled passenger airliners worldwide, which resulted in a fatality (including all U.S. Part 121 and Part 135 fatal accidents)"),
                          p("2. All cargo, positioning, ferry and test flight fatal accidents."),
                          p("3. All military transport accidents with 10 or more fatalities."),
                          p("4. All commercial and military helicopter accidents with greater than 10 fatalities."),
                          p("5. All civil and military airship accidents involving fatalities."),
                          p("6. Aviation accidents involving the death of famous people."),
                          p("7. Aviation accidents or incidents of noteworthy interest."),
                          h3("Sources"),
                          p("1. Database index. (2019). Retrieved 4 October 2019, from http://www.planecrashinfo.com/database.htm"),
                          p("2. G, C. (2019). Airplane Crash Data Since 1908. Retrieved 4 October 2019, from https://www.kaggle.com/cgurkan/airplane-crash-data-since-1908"),
                          p("3. Google Geocoding Service. (2019). Retrieved 4 October 2019, from https://developers.google.com/maps/documentation/javascript/geocoding")
            )
        } else {
            return()
        }
    })
    
    # Intro Panel Button
    output[["intro_panel"]] <- renderUI({
      if(show_info_flag()==2){
        absolutePanel(top = 0, left = 0, right = 0, bottom = 0, width =920, height=525, id = "intro_panel", 
                      style="border-radius: 25px;
                         padding: 8px; 
                         border-bottom: 2px solid #CCC;
                         background: rgba(255,255,255,0.75);",
                    h3("Introduction"),
                    p("It wasn't until as recently as the past 20 years that a clear downward trend in number 
                    of fatal crashes began to emerge. Be it mechanical malfunctions, weather, war or human error, 
                    the plane crashes of history helped to shape aviation safety as we know it today. 
                    As the saying goes in aviation, regulations are written in blood. 
                    It is important that we remember the path that we have taken to get to where we are today."),
                    p("In 2018, commercial airlines transported a total of 4.3 billion passengers across the globe 
                    according to the International Civil Aviation Organization (2018), with only 11 fatal crashes. 
                    More passengers than ever are travelling by air and yet, the last time the number civilian of 
                    fatal crashes was as low was in 1926. It should come as no surprise then that the Washington 
                    Post (2015) reported flying as the safest method of travel per unit distance travelled. "),
                    h3("Timeline Information"),
                    p("The black line on the plot shows the year that you are currently viewing. 
                          Set the year to one that interests you and click on the map markers for more information.
                          Historical Military and Civilian events hightlighted in the plot (red and blue dots) 
                            below typically show the crashes with the most fatalities in each category. 
                            Major military conflicts are highlighted (red dots with lines).
                            However, individual events that are part of it are not.
                            This visualization was designed on a 1920x1080 resolution screen.
                            "),
                    h3("Sources"),
                    p("1. The World of Air Transport in 2018. (2018). Retrieved 13 October 2019, from 
                      https://www.icao.int/annual-report-2018/Pages/the-world-of-air-transport-in-2018.aspx"),
                    p("2. Ingraham, C. (2015). The safest - and deadliest - ways to travel. Retrieved 13 October 2019, 
                      from https://www.washingtonpost.com/news/wonk/wp/2015/05/14/the-safest-and-deadliest-ways-to-travel/"))
      } else {
        return()
      }
    })
}

##########################################################################################
# UI definition
##########################################################################################

ui <- bootstrapPage(
    tags$style(type = "text/css", "html, 
               body {width:100%;height:100%;}
               #info_panel {margin: 5% auto auto auto;}
               #intro_panel {margin: 5% auto auto auto;}"),
    leafletOutput("map", width = "100%", height = "100%"),
    # Time Series
    absolutePanel(bottom = 15, left = 10, width ="98%", height = 300,
                  style="border-radius: 25px;
                         padding: 8px; 
                         border-bottom: 2px solid #CCC;
                         background: rgba(255,255,255,0.75);
                         text-align: center;",
                  plotlyOutput("ts",height="100%")
    ),
    # Plot Options
    absolutePanel(bottom = 320, left = 10, width =285, height=115,
                  style="border-radius: 25px;
                         padding: 8px; 
                         border-bottom: 2px solid #CCC;
                         background: rgba(255,255,255,0.75);",
                  radioButtons("plot.option",
                               label = "Select Plot:",
                               choices = list("Crashes" = 1,
                                              "Passenger and Crew Fatalities" = 2,
                                              "Passenger, Crew and Ground Fatalities" = 3),
                               selected = 3)),
    # Plot Data
    absolutePanel(bottom = 440, left = 10, width =100, height=115,
                  style="border-radius: 25px;
                         padding: 8px; 
                         border-bottom: 2px solid #CCC;
                         background: rgba(255,255,255,0.75);",
                  radioButtons("plot.data",
                               label = "Select Data:",
                               choices = list("Both" = 1,"Military" = 2, "Civilian"=3),
                               selected = 1)),
    # Plot Year
    absolutePanel(bottom = 440, left = 120, width =100, height=85,
                  style="border-radius: 25px;
                         padding: 8px; 
                         border-bottom: 2px solid #CCC;
                         background: rgba(255,255,255,0.75);",
                  numericInput("year", 
                               label = "Input Year:", 
                               value = 2001,
                               min = min(data$year),
                               max = max(data$year),
                               step = 1)),
    # Show Hide Button
    absolutePanel(right = 25, bottom = 320, width = 200, height = 110,
                  style="border-radius: 25px;
                         padding: 8px; 
                         border-bottom: 2px solid #CCC;
                         background: rgba(255,255,255,0.75);",
                  radioButtons("midpanel",
                               label = "Show Information Panel:",
                               choices = list("None" = 1,
                                              "Introduction" = 2,
                                              "Data Sources" = 3),
                               selected = 2)),
    # Information Panel
    uiOutput("info_panel"),
    # Intro Panel
    uiOutput("intro_panel")
)

# Run the application 
shinyApp(ui = ui, server = server)