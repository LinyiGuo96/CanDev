##### Data Cleaning

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

## choose where to get csv

setwd(choose.dir())

## reading data in current directory

raw = read.csv('grants.csv')

## filter only Canada

raw_canada = raw %>% filter(recipient_country == 'CA', recipient_type != '')

## Select relevant columns

raw_columns = raw_canada %>% select(agreement_start_date, agreement_end_date,
                                    recipient_province, recipient_city,
                                    recipient_postal_code, owner_org_title, agreement_value, recipient_type
)

## Rename relevant columns

rename = raw_columns %>% rename(Start = agreement_start_date, End = agreement_end_date, 
                                Province = recipient_province, City = recipient_city,
                                Postal = recipient_postal_code, Department = owner_org_title,
                                Value = agreement_value, Type = recipient_type)

## Clean province codes

province_clean = rename %>% mutate(Province = car::recode(Province, "c('QC', 'QB', 'Quebec') = 'Quebec';
                                                     c('ON', 'Ontario', 'ON ') = 'Ontario';
                                                     c('BC', 'British Columbia') = 'British Columbia';
                                                     c('MB', 'MA') = 'Manitoba';
                                                     c('NL', 'NF') = 'Newfoundland and Labrador';
                                                     c('SK','SK ') = 'Saskatchewan';
                                                     c('AB', 'AB ', 'Alberta') = 'Alberta';
                                                     'NS' = 'Nova Scotia';
                                                     'PE' = 'Prince Edward Island';
                                                     'NB' = 'New Brunswick';
                                                     'NT' = 'Northwest Territories';
                                                     'YT' = 'Yukon';
                                                     'NU' = 'Nunavut'
                                                     "))

## Rename recipient type names via dictionary

Type_dic = province_clean %>% mutate(Type = car::recode(Type, "'A' = 'Aboriginal';
                                                        'F' = 'For-Profit';
                                                        'G' = 'Government';
                                                        'I' = 'International';
                                                        'N' = 'Not-for-Profit/Charities';
                                                        'O' = 'Other';
                                                        'P' = 'Individual';
                                                        'S' = 'Academia'
                                                        "))

## Remove missing provinces

province_filter = Type_dic %>% filter(Province != '')

## Creating new columns 

date_years = province_filter %>% mutate(StartYear = substring(Start,1,4), EndYear = substring(End,1,4))

date_duration = date_years %>% mutate(Length = as.Date(End) - as.Date(Start))

postal_zone = date_duration %>% mutate(Zone = substring(Postal,1,1))

#### Cleaning new columns

## Remove non-alphabetic postal zones

clean_zones = postal_zone %>% filter(grepl('^[A-Za-z]+$', Zone))

## Coerce all zones to uppercase

cleaner_zones = clean_zones %>% mutate(Zone = toupper(Zone))

### naming final data

data = cleaner_zones

##### FILTER

## enter one of 'For-Profit', 'Not-for-Profit/Charities', 'Individual',
## 'Government', 'Academia', 'Aboriginal', 'Other', 'International'

Year=2018

datayear = data %>% filter(StartYear == Year)

datayear = datayear %>% mutate(Zone = factor(Zone, levels = c('K','L','M','N','G','E','B','J','A','C'
                                                              ,'P','R','S','T','V','Y','X','W','H')))

##### Summarize Data

summary_by_province = datayear %>% group_by(Province,.drop = FALSE) %>% summarise(Total = sum(Value))

summary_by_zone = datayear %>%  group_by(Zone, .drop = FALSE) %>% summarise(Total = sum(Value)) 

regionalsummary = summary_by_zone %>% mutate(Region = car::recode(Zone, "c('K','L','M','N') = 'SE Ontario';
                                                                  c('G','E','B') = 'East Quebec & Maritimes';
                                                                  c('J','A','C') = 'NW Quebec, Labrador & Islands';
                                                                  c('P','R','S') = 'W Ontario & Praries';
                                                                  c('T','V') = 'BC & Alberta';
                                                                  c('Y','X') = 'Territories'
                                                                  "))

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
          legend.key.size = unit(1, "cm"),
    )
}

theme_set(theme_map())

##### BY PROVINCE

canada_cd <- st_read("canada_cd_sim.geojson", quiet = TRUE)

canada_cd <- st_transform(canada_cd, crs = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

canada_cd_province = merge(x=canada_cd, y=summary_by_province, by.x = 'PRENAME', by.y = 'Province')

p_canada <- ggplot(data = canada_cd_province, 
                   mapping = aes(fill = Total/1000000))+ 
  geom_sf(color = "grey60", size = 0.01) + 
  scale_fill_gradient(low = 'lightblue', high = 'navy') + 
  theme_map()  + labs(fill = 'Total G&C, Millions') + 
  theme(panel.grid.major = element_line(color = "white"),
  legend.key = element_rect(color = "gray40", size = 0.1))
p_canada


##### BY ZONE


canada_cd_2 <- st_read("canada_cd2_sim.geojson", quiet = TRUE)

canada_cd_2 <- st_transform(canada_cd_2, crs = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

canada_cd_zone = canada_cd_2 %>% mutate(Zone = substr(as.character(CFSAUID),1,1))

canada_cd_zone = merge(canada_cd_zone, regionalsummary, by='Zone')

### FILTER ON ONE OF 'SE Ontario', 'NW Quebec, Labrador & Islands',
# 'W Ontario & Praries', 'BC & Alberta'

zone = 'BC & Alberta'

canada_cd_region = canada_cd_zone %>% filter(Region == zone)


p_region = ggplot(data = canada_cd_region, 
                  mapping = aes(fill = Total/1000000), show.legend=F)+ 
                  geom_sf(color = "grey60", size = 0.01) + 
                  scale_fill_gradient(low = 'lightblue', high = 'navy') + 
                  theme_map() + labs(fill = 'Total G&C, Millions') + 
                  theme(panel.grid.major = element_line(color = "white"),
                  legend.key = element_rect(color = "gray40", size = 0.1))

p_region

#### PIE CHARTS 

## Zones

datawithregion = datayear %>% mutate(Region = car::recode(Zone, "c('K','L','M','N') = 'SE Ontario';
                                                                  c('G','E','B') = 'East Quebec & Maritimes';
                                                                  c('J','A','C') = 'NW Quebec, Labrador & Islands';
                                                                  c('P','R','S') = 'W Ontario & Praries';
                                                                  c('T','V') = 'BC & Alberta';
                                                                  c('Y','X') = 'Territories'
                                                                  "))
data_region = datawithregion %>% filter(Region == zone)

summary_category_zone = data_region %>% group_by(Type, .drop=F) %>% summarize(Total = sum(Value)) %>%
 mutate(Proportion = Total / sum(Total)) %>% arrange(desc(Proportion))


sig_portion = summary_category_zone %>% filter(Proportion >= 0.01)

pie(x = summary_category_zone$Proportion, labels = c(paste(sig_portion$Type, percent(sig_portion$Proportion), sep=' '),
                                                     rep('', nrow(summary_category_zone)-nrow(sig_portion))),
    col = viridis(length(summary_category_zone$Type)))


## Province

pro = 'Ontario'

data_province = datayear %>% filter(Province == pro)

summary_category = data_province %>% group_by(Type, .drop=F) %>% 
  summarize(Total = sum(abs(Value))) %>% mutate(Proportion = Total / sum(Total)) %>% arrange(desc(Proportion))


sig_portion = summary_category %>% filter(Proportion >= 0.01)

pie(x = summary_category$Proportion, labels = c(paste(sig_portion$Type, percent(sig_portion$Proportion), sep=' '),
                                                rep('', nrow(summary_category)-nrow(sig_portion))),
    col = viridis(length(summary_category$Type)) )

#### Pie Chart Total

summary_category_overall = datayear %>% group_by(Type, .drop=F) %>% 
  summarize(Total = sum(Value)) %>% mutate(Proportion = Total / sum(Total))  %>% arrange(desc(Proportion))

sig_portion = summary_category_overall %>% filter(Proportion >= 0.01)


pie(x = summary_category_overall$Proportion, labels = c(paste(sig_portion$Type, percent(sig_portion$Proportion), sep=' '),
                                                        rep('', nrow(summary_category_overall)-nrow(sig_portion))),
    col = viridis(length(summary_category_overall$Type)) )

