library(RColorBrewer)
library(scales)
library(lattice)
library(viridis)
library(ggplot2)
library(maps)
library(dplyr)
library(tidyr)
library(car)
library(geojsonio)
library(rmapshaper)
library(rgdal)
library(tidyverse)
library(spdplyr)
library(sf)
library(socviz)
library(maps)
library(viridis)
library(googleCharts)
library(mapproj)
data<-read.csv("data/clean_data.csv")
summary_by_province = data %>% group_by(Province,.drop = FALSE) %>% summarise(Total = sum(Value))
summary_by_zone = data %>%  group_by(Zone,.drop = FALSE) %>% summarise(Total = sum(Value)) 
regionalsummary = summary_by_zone %>% mutate(Region = car::recode(Zone, "c('K','L','M','N') = 'SE Ontario';
                                                                  c('G','E','B','H') = 'East Quebec & Maritimes';
                                                                  c('J','A','C') = 'NW Quebec, Labrador & Islands';
                                                                  c('P','R','S') = 'W Ontario & Praries';
                                                                  c('T','V') = 'BC & Alberta';
                                                                  c('Y','X') = 'Territories'
                                                                  "))
data<-data %>% mutate(Region = car::recode(Zone, "c('K','L','M','N') = 'SE Ontario';
                                                                  c('G','E','B','H') = 'East Quebec & Maritimes';
                                                                  c('J','A','C') = 'NW Quebec, Labrador & Islands';
                                                                  c('P','R','S') = 'W Ontario & Praries';
                                                                  c('T','V') = 'BC & Alberta';
                                                                  c('Y','X') = 'Territories'
                                                                  "))
data1 <- read.csv("data/summary_city.csv")
data1$Province <- as.factor(data1$Province)


#### DRAW GRAPHS
theme_map <- function(base_size=9, base_family="") {
  require(grid)
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0, "lines"),
          plot.background=element_blank(),
          legend.position = 'bottom',
          legend.title = element_text(color = 'black', size=18, face = 'bold'),
          legend.key.size = unit(1, "cm")
    )
}

theme_set(theme_map())
canada_cd <- st_read("data/canada_cd_sim.geojson", quiet = TRUE)
canada_cd <- st_transform(canada_cd, crs = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

canada_cd_2 <- st_read("data/canada_cd2_sim.geojson", quiet = TRUE)

canada_cd_2 <- st_transform(canada_cd_2, crs = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

canada_cd_zone = canada_cd_2 %>% mutate(Zone = substr(as.character(CFSAUID),1,1))

canada_cd_zone = merge(canada_cd_zone, regionalsummary, by='Zone')

### FILTER ON ONE OF 'SE Ontario', 'NW Quebec, Labrador & Islands',
# 'W Ontario & Praries', 'BC & Alberta'


Pro<- summary_by_province$Province
Pro<- as.character(Pro)
Pro<-c("Overall",Pro)

Zone_var<-c('SE Ontario','East Quebec & Maritimes','NW Quebec, Labrador & Islands',
            'W Ontario & Praries','BC & Alberta','Territories')
ui<-navbarPage("GoC TBS Funding", id="nav",
               
               tabPanel("Funding by Province",
                        div(class="outer",
                            
                            # If not using custom CSS, set height of leafletOutput to a number instead of percent
                            plotOutput("Overall",width = "90%", height = "600px"),
                            
                            # Shiny versions prior to 0.11 should use class = "modal" instead.
                            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                          draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                          width = 350, height = "auto",
                                          
                                          h2("Funding explorer"),
                                          
                                          selectInput("Province", "Province", Pro),
                                          sliderInput("Year", "Year", min=2013, max=2019, value=2019, step=1),
                                          plotOutput("Pie", height = 350)
                            )
                        )
               ),
               tabPanel("Funding by Geographic Zone",
                        div(class="outer",
                            
                            # If not using custom CSS, set height of leafletOutput to a number instead of percent
                            plotOutput("Zone",width = "100%", height = "600px"),
                            
                            # Shiny versions prior to 0.11 should use class = "modal" instead.
                            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                          draggable = TRUE, top = 60, left = "auto", right = 40, bottom = "auto",
                                          width = 350, height = "auto",
                                          
                                          h2("Funding explorer"),
                                          selectInput("Zone", "Zone", Zone_var),
                                          plotOutput("ZonePie", height = 350)
                                          
                            )
                        )
               ),
               tabPanel("Funding by City",
                        fluidPage(
                          googleChartsInit(),
                          # Use the Google webfont "Source Sans Pro"
                          tags$link(
                            href=paste0("http://fonts.googleapis.com/css?",
                                        "family=Source+Sans+Pro:300,600,300italic"),
                            rel="stylesheet", type="text/css"),
                          tags$style(type="text/css",
                                     "body {font-family: 'Source Sans Pro'}"
                          ),
                          
                          h2("Funding by City"),
                          plotOutput("Canada_map",width = "100%", height = "800px"),
                          h2(),
                          googleBubbleChart("chart",
                                            width="100%", height = "475px",
                                            options = list(
                                              fontName = "Source Sans Pro",
                                              fontSize = 13,
                                              # Set axis labels and ranges
                                              hAxis = list(
                                                title = "longitude",
                                                viewWindow = list(
                                                  min = -125,
                                                  max = -61
                                                )
                                              ),
                                              vAxis = list(
                                                title = "latitude",
                                                viewWindow = list(
                                                  min = 40,
                                                  max = 55
                                                )
                                              ),
                                              # The default padding is a little too spaced out
                                              chartArea = list(
                                                top = 50, left = 75,
                                                height = "75%", width = "75%"
                                              ),
                                              # Allow pan/zoom
                                              explorer = list(),
                                              # Set bubble visual props
                                              bubble = list(
                                                opacity = 0.4, stroke = "none",
                                                # Hide bubble label
                                                textStyle = list(
                                                  color = "none"
                                                )
                                              ),
                                              # Set fonts
                                              titleTextStyle = list(
                                                fontSize = 16
                                              ),
                                              tooltip = list(
                                                textStyle = list(
                                                  fontSize = 12
                                                )
                                              )
                                            )
                          ),
                          fluidRow(
                            shiny::column(4, offset = 4,
                                          sliderInput("Year", "Year",
                                                      min = 2009, max = 2019,
                                                      value = 2009, animate = TRUE, step = 1))))),
               tabPanel("Data Explorer",
                        fluidRow(
                          shiny::column(3,
                                 selectInput("province", "Province", c("All Provinces"="",Pro[-1]), multiple=TRUE)
                          )
                        ),
                        fluidRow(
                          shiny::column(2,
                                 numericInput("minValue", "Min value", min=0, max=100000,step=100, value=1000)
                          ),
                          shiny::column(2,
                                 numericInput("maxValue", "Max value", min=0, step=10,max=1817270000, value=18170000)
                          )
                        ),
                        hr(),
                        DT::dataTableOutput("datatable")
               )
               
)

server<-function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$Overall <- renderPlot({
  datayear = data %>% filter(StartYear == input$Year)
  summary_by_province<-datayear%>% group_by(Province,.drop = FALSE) %>% summarise(Total = sum(Value))
  canada_cd_province = merge(x=canada_cd, y=summary_by_province,by.x = 'PRENAME', by.y = 'Province')
  p_canada <- ggplot(data = canada_cd_province, 
                     mapping = aes(fill = Total/1000000))+ 
    geom_sf(color = "grey60", size = 0.01) + 
    scale_fill_gradient(low = 'lightblue', high = 'navy') + 
    theme_map()  + labs(fill = 'Total G&C, Millions') + 
    theme(panel.grid.major = element_line(color = "white"),
          legend.key = element_rect(color = "gray40", size = 0.1))
    
  return(p_canada)
  })
  output$Pie <- renderPlot({
    if (input$Province == "Overall"){
      datayear = data %>% filter(StartYear == input$Year)
      summary_category_overall = datayear %>% group_by(Type, .drop=F) %>% 
        summarize(Total = sum(Value)) %>% mutate(Proportion = Total / sum(Total))  %>% arrange(desc(Proportion))
      sig_portion = summary_category_overall %>% filter(Proportion >= 0.01)
      return(pie(x = abs(summary_category_overall$Proportion), labels = c(paste(sig_portion$Type, percent(sig_portion$Proportion), sep=' '),
                                                              rep('', nrow(summary_category_overall)-nrow(sig_portion))),
          col = viridis(length(summary_category_overall$Type)),main="Breakdown by Funding Type" ))}
    else{
      datayear = data %>% filter(StartYear == input$Year)
      data_province = datayear %>% filter(Province == input$Province)
      summary_category = data_province %>% group_by(Type, .drop=F) %>%  
      summarize(Total = sum(Value)) %>% mutate(Proportion = Total / sum(Total)) %>% arrange(desc(Proportion))
      sig_portion = summary_category %>% filter(Proportion >= 0.01)
                   
      return(pie(x = abs(summary_category$Proportion), labels = c(paste(sig_portion$Type, percent(sig_portion$Proportion), sep=' '),
                                                                   rep('', nrow(summary_category)-nrow(sig_portion))),
                       col = viridis(length(summary_category$Type)),main="Breakdown by Funding Type"))
      }
  })
    
    
  output$datatable <- DT::renderDataTable({
    df <- data %>%
      filter(
        Value >= input$minValue,
        Value <= input$maxValue,
        is.null(input$province) | Province %in% input$province)
    action <- DT::dataTableAjax(session, df, outputId = "datatable")
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
  output$Zone<-renderPlot(
    {
      canada_cd_region = canada_cd_zone %>% filter(Region == input$Zone)
      ggplot(data = canada_cd_region, 
            mapping = aes(fill = Total/1000000), show.legend=F)+ 
        geom_sf(color = "grey60", size = 0.01) + 
        scale_fill_gradient(low = 'lightblue', high = 'navy') + 
        theme_map() + labs(fill = 'Total G&C, Millions') + 
        theme(panel.grid.major = element_line(color = "white"),
              legend.key = element_rect(color = "gray40", size = 0.1))}
    
    )
  output$ZonePie<-renderPlot({
  datawithregion = data %>% mutate(Region = car::recode(Zone, "c('K','L','M','N') = 'SE Ontario';
                                                                  c('G','E','B','H') = 'East Quebec & Maritimes';
                                                                  c('J','A','C') = 'NW Quebec, Labrador & Islands';
                                                                  c('P','R','S') = 'W Ontario & Praries';
                                                                  c('T','V') = 'BC & Alberta';
                                                                  c('Y','X') = 'Territories'
                                                                  "))
  data_region = datawithregion %>% filter(Region == input$Zone)
  summary_category_zone = data_region %>% group_by(Type, .drop=F) %>% summarize(Total = sum(Value)) %>%
  mutate(Proportion = Total / sum(Total)) %>% arrange(desc(Proportion))
  sig_portion = summary_category_zone %>% filter(Proportion >= 0.01)
  return(pie(x = summary_category_zone$Proportion, labels = c(paste(sig_portion$Type, percent(sig_portion$Proportion), sep=' '),
                                                       rep('', nrow(summary_category_zone)-nrow(sig_portion))),
      col = viridis(length(summary_category_zone$Type)),main="Breakdown by Funding Type"))
  })
  defaultColors <- rainbow(9)
  series <- structure(
    lapply(defaultColors, function(color) { list(color=color) }),
    names = levels(data1$Province)
  )
  yearData <- reactive({
    # Filter to the desired year, and put the columns
    # in the order that Google's Bubble Chart expects
    # them (name, x, y, color, size). Also sort by region
    # so that Google Charts orders and colors the regions
    # consistently.
    df <- data1 %>%
      filter(Year == input$Year) %>%
      select(City, long,lat, Province, Total
      ) %>%
      arrange(Province)})
  output$chart <- reactive({
    # Return the data and options
    list(
      data = googleDataTable(yearData()),
      options = list(
        title = sprintf(
          "Funding to Top 28 Cities by Population, Canada, %s",
          input$Year),
        series = series
      )
    )
  })
  output$Canada_map<-renderPlot({
    Canada_map<-recordPlot()
    maps::map("world","Canada")
    maps::map.cities(canada.cities,minpop=100000,label = F,lwd=5)
    return(Canada_map)})
}
shinyApp(ui = ui, server = server)
