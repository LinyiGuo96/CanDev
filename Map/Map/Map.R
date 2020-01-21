install.packages("sf")
install.packages("rgdal")
install.packages("geojsonio")
install.packages("spdplyr")
install.packages("rmapshaper")

#Then we load the packages, along with others we’ve already installed (as part of the seminar).

library(geojsonio)
library(rmapshaper)
library(rgdal)
library(tidyverse)
library(spdplyr)
library(sf)
library(socviz)
library(maps)

#We make a function, theme_map(), that will be our ggplot theme. It consists mostly in turning off pieces of the plot (like axis text and so on) that we don’t need.



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
          )
}

theme_set(theme_map())


canada_cd <- st_read("canada_cd2_sim.geojson", quiet = TRUE)

canada_cd <- st_transform(canada_cd, crs = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

canada_cd_zone = canada_cd %>% mutate(Zone = substr(as.character(CFSAUID),1,1))

canada_cd_zone = merge(canada_cd_zone, regionalsummary, by='Zone')

canada_cd_zone_K = canada_cd_zone %>% filter(Region == 'SE Ontario')


p <- ggplot(data = canada_cd_zone_K, 
            mapping = aes(fill = Total), show.legend=F)
p_out <- p + geom_sf(color = "gray60", 
                     size = 0.001) + 
  scale_fill_gradient(low = 'lightblue', high = 'navy') + 
  theme_map() + 
  theme(panel.grid.major = element_line(color = "white"),
        legend.key = element_rect(color = "gray40", size = 0.1))


p_out

