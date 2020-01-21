library(dplyr)
library(tidyr)
library(car)

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

summary_by_province = data %>%  group_by(Province) %>% summarise(Total = sum(Value)) 

summary_by_zone = data %>%  group_by(Zone) %>% summarise(Total = sum(Value)) 

regionalsummary = summary_by_zone %>% mutate(Region = car::recode(Zone, "c('K','L','M','N') = 'SE Ontario';
                                                                  c('G','E','B') = 'East Quebec & Maritimes';
                                                                  c('J','A','C') = 'NW Quebec, Labrador & Islands';
                                                                  c('P','R','S') = 'W Ontario & Praries';
                                                                  c('T','V') = 'BC & Alberta';
                                                                  c('Y','X') = 'Territories'
                                                                  "))