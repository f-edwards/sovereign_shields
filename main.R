rm(list=ls())
library(tidyverse)
library(lme4)
select<-dplyr::select
theme_set(theme_bw())

### rds produced in read.r
aianh_dat<-readRDS("./data/aianh_dat.rds")
state_dat<-readRDS("./data/state_dat.rds")
state_dat<-state_dat %>% 
  mutate(pc_income_ineq = pc_income_amind/pc_income_white) %>% 
  select(state, statea, stusps, gisjoin, 
         total_population, total_population_amind,
         pc_income_ineq,
         pov_ratio) %>% 
  mutate(statea=as.numeric(statea)) %>% 
  distinct()
afcars<-read_csv("./data/afcars_aian_st.csv")

west_of_MS<-
  c("AK", "AZ", "AR", 
    "CA", "CO", "HI",
    "ID", "IA", "KS",
    "LA", "MN", "MO",
    "MT", "NE", "NV",
    "NM", "ND", "OK",
    "OR", "SD", "TX",
    "UT", "WA", "WY")

state_dat$west_MS<-state_dat$stusps%in%west_of_MS

state_dat$pl280<-case_when(
  state_dat$stusps %in% c("CA", "MN", "NE", "OR", 
                          "WI", "AK") ~ "Mandatory",
  state_dat$stusps %in% c("NV", "FL", "ID", "IA", 
                          "WA", "SD", "MT", "ND", 
                          "AZ", "UT") ~ "Optional"
)

state_dat$pl280<-ifelse(is.na(state_dat$pl280), "Non-PL280", state_dat$pl280)

state_dat$AnyTribalLand<-!(is.na(state_dat$pov_ratio))

fe_dat<-read_csv("./data/fe_imputed_08_18.csv") %>% 
  select(.imp, id, year, FIPS_county, loc_state, race, fe_cause_of_death) %>% 
  filter(fe_cause_of_death!="suicide") 

fe_st<-fe_dat %>% 
  group_by(.imp, year, loc_state, race) %>% 
  summarise(fe_deaths = n()) %>% 
  rename(stusps = loc_state) 

### complete zeroes
expands<-expand_grid(stusps = unique(fe_dat$loc_state), .imp=1:5, year=2008:2018, 
                       race = unique(fe_dat$race))

fe_st<-expands %>% 
  left_join(fe_st) %>% 
  mutate(fe_deaths = ifelse(is.na(fe_deaths), 0 , fe_deaths)) %>% 
  filter(race=="amind") %>% 
  left_join(state_dat) %>% 
  left_join(afcars %>% 
              rename(statea=state)) %>% 
  filter(year<2018) ## not a full year in FE anyway, for AFCARS 2017 is latest

afcars_st<-afcars %>%
  rename(statea = state) %>% 
  left_join(state_dat) %>% 
  filter(!(statea)%in%c(2, 15)) # remove AK, HI. Add into state_dat later
  


