
rm(list=ls())
library(tidyverse)
library(lubridate)

##### read in FE data

fe<-read_csv("./data/fe_9_16_19.csv")

fe<-fe %>% 
  filter(`Date (Year)`!=2100)

##renaming ugly variables 

fe<-fe %>% 
  rename(ID = `Unique ID`, name = `Subject's name`, age =`Subject's age`,
         race = `Subject's race`, agency = "Agency(ies) involved in death",
         gender = `Subject's gender`, state = `Location of death (state)`)

### convert date to date

fe<-fe %>% 
  mutate(date = mdy(`Date of injury resulting in death (month/day/year)`))

#### group by race/ethn

fe %>% 
  group_by(`race`) %>% 
  group_by(`Location of death (city)`) %>%
  summarise(count = n())

#### look at time trends

# fe_ts<-fe %>%
#   filter(year(date)<=2019) %>%
#   group_by(year(date), `Subject's race`) %>%
#   summarise(deaths = n())
# 
# ggplot(fe_ts,
#        aes(x = `year(date)`,
#            y = deaths,
#            color = `Subject's race`)) +
#   geom_line()

#### explore the Police dept variable
# ? 

fe_pd <- fe %>%
  group_by(agency) %>%
  summarise(count = n()) 

hist <- ggplot(fe_pd, aes(x= count)) + geom_bar() + scale_x_log10()

#### some items have multiple PDs
temp<- str_split(fe$agency, ",")
  
### read in the state data
#sd<- read_csv("./data/nhgis0001_csv/nhgis0001_tract.csv")
## loaded in block-level to work with

sd_block <- read_csv("./data/nhgis0003_csv/nhgis0003_block.csv")

## sd_block <-sd %>% mutate(stname = tolower(STATE))
### RESHAPE THE TRACT-LEVEL INTO STATE-LEVEL DATA
##sd<-sd %>% group_by(STATE) %>% summarise(AHZAE001 = sum(AHZAE001))

###### TO MATCH LET'S MAKE A FIPS INDEX TABLE
#### USE THIS TO START

library(maps)
data(state.fips)


state.fips<-state.fips %>% 
  select(fips, abb) %>% 
  distinct()

fe_st_yr<-fe %>% 
  group_by(state, `Date (Year)`) %>% 
  summarise(deaths = n())

####################################################
#### 1. MAKE A STATE - YEAR SUMMARY TABLE FOR FE (DONE!)
#### 2. Make a per capita death rate table using the pop data
#### 3. visualize it somehow

# ggplot(fe_st_yr,
#        aes(x = `Date (Year)`,
#            y = deaths)) + 
#   geom_line() + 
#   facet_wrap(~state)

##renaming column in state fips to join to fe data? 

#colnames(state.fips)[colnames(state.fips)=="abb"] <- "state"
### same thing
state.fips<-state.fips %>% 
  rename(state=abb)

### note that you need to drop some stuff and add HI, AK
## what am I dropping? 

state.fips <- state.fips %>% 
  add_row(fips = 2, state = "AK", .before = 2) %>% 
  add_row(fips = 15, state = "HI", .before = 12)

# joining ? 

fe_fips <- left_join(fe_st_yr, state.fips, by = "state")

## BROKE HERE? 

sd_block <- sd_block %>% 
  rename(fips=STATEA) %>% 
  mutate(fips=as.numeric(fips))


fe_sd <- left_join(fe_fips, sd_block) ## STUPID. broke the dataset. dimensions are all off. 

fe_sd <- fe_sd %>%
  rename(year=`Date (Year)`, pop_tot=AHY2E001) %>%
  select(state, year, deaths, fips, pop_tot) %>% 
  mutate(deaths_pc = deaths/pop_tot*1e5) %>% 
  filter(year>2010)

fe_sd %>% 
  ggplot(aes(x=year, y=deaths_pc))+
  geom_line()+
  facet_wrap(~state)

####################################################

#### make a 5-year ACS table for state population from NHGSInh

#### and attach it to FE: table B03002. Hispanic or Latino Origin by Race

### make sure state column names are the same; state abbreviations vs full state name? 

fe_state <-fe%>%mutate(`Location of death (state)` = case_when(
  `Location of death (state)` == "Alabama" ~ "AL",
  `Location of death (state)` == "Alaska" ~ "AK",
  `Location of death (state)` == "Arizona" ~ "AZ"))


### use a left_join on a summarise by state table 

### convert date to date

fe<-fe %>% 
  mutate(date = mdy(`Date of injury resulting in death (month/day/year)`))

### feel free to rename, or not


