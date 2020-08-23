rm(list=ls())
library(tidyverse)

library(maps)
source("main.R")
state_map<-map_data("state")
fe_st<-fe_st %>% 
  mutate(region=tolower(state))

####################################
############ MAPS
####################################
######################## PL 280

plot_dat<-state_dat %>% 
  mutate(region = tolower(state)) %>% 
  left_join(state_map)

ggplot(plot_dat,
       aes(x=long, y = lat, fill = pl280,
           group = group)) + 
  geom_polygon(color = "black") + 
  theme_void() +
  labs(fill = "") +
  theme(legend.position = "bottom") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  ggsave("./vis/map_pl280.png")

################### FATAL ENCOUNTERS
## Police killings of American Indians and Alaska Natives 
## per year per 100,000 population, 2008 - 2018

fe_yrs_n<-length(unique(fe_st$year))
fe_st_plot<-fe_st %>% 
  ungroup() %>% 
  group_by(region) %>% 
  summarise(value = mean(fe_deaths)/max(pop_aian_adult)* 1e5) %>%  # convert to deaths/100k/yr
  mutate(var="Deaths caused by police") 

afcars_st_plot<-fe_st %>% 
  ungroup() %>% 
  group_by(region) %>% 
  summarise(value = mean(fc_caseload/pop_aian_child) * 100) %>% 
  mutate(var = "Foster care caseload")

plot_dat<-fe_st_plot %>% 
  bind_rows(afcars_st_plot) %>% 
  left_join(state_map)

ggplot(plot_dat %>% 
         filter(var=="Deaths caused by police"),
       aes(x=long, y = lat, fill = value,
           group = group)) + 
  geom_polygon(color = "black") + 
  theme_void() +
  labs(fill = "") +
  theme(legend.position = "bottom") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_distiller(palette = "Spectral") +
  ggsave("./vis/map_fe_st.png")

## American Indian children in foster care as percent of child population, 
## 2004-2017 annual average

ggplot(plot_dat %>% 
         filter(var=="Foster care caseload"),
       aes(x=long, y = lat, fill = value,
           group = group)) + 
  geom_polygon(color = "black") + 
  theme_void() +
  labs(fill = "") +
  theme(legend.position = "bottom") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_distiller(palette = "Spectral")  +
  ggsave("./vis/map_fc_cl.pdf", width = 8, height = 5)

## Police killings and foster care caseloads by state, in mean-centered standard deviation units


ggplot(plot_dat %>% 
         group_by(var) %>% 
         mutate(value = scale(value)),
       aes(x=long, y = lat, fill = value,
           group = group)) + 
  geom_polygon(color = "black") + 
  theme_void() +
  labs(fill = "") +
  theme(legend.position = "bottom") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_distiller(palette = "Spectral") + 
  facet_wrap(~var) +
  ggsave("./vis/map_fe_fc.pdf", width = 8, height = 5)

## Percent of AIAN children in state foster care in AIAN foster homes, 2004 - 2017 average

afcars_st_plot<-fe_st %>% 
  ungroup() %>% 
  group_by(region) %>% 
  summarise("Caseload" = mean(fc_caseload/pop_aian_child, na.rm=TRUE) * 100,
            "Entries" = mean(fc_entered/pop_aian_child, na.rm=TRUE) *100,
            "Institutionalized" = mean(fc_inst/fc_caseload, na.rm=TRUE)*100,
            "In AIAN foster home" = mean(fc_aian_home, na.rm=TRUE)*100) %>% 
  pivot_longer(cols=c(Caseload, Entries, Institutionalized, `In AIAN foster home`), 
               names_to = "var",
               values_to = "value") %>% 
  group_by(var) %>% 
  left_join(state_map)

ggplot(afcars_st_plot %>% 
         filter(var=="In AIAN foster home"),
       aes(x=long, y = lat, fill = value,
           group = group)) + 
  geom_polygon(color = "black") + 
  theme_void() +
  labs(fill = "") +
  theme(legend.position = "bottom") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_distiller(palette = "Spectral") +
  ggsave("./vis/map_fc_aianhome.pdf", width = 8, height = 5)

## Proportion of tribal governments with independent police, with arrest authority over tribal members off of tribal lands, with arrest authority over non-Indians on tribal lands, and with detention facilities, 2002

police_plot<-fe_st %>% 
  ungroup() %>% 
  group_by(region) %>% 
  summarise("Tribal police" = mean(police_tribal),
            "Arrest members off tribal land" = mean(police_tribal_off_res),
            "Arrest non-Indians on tribal land" = mean(police_tribal_non_ind),
            "Detention" = mean(jail)) %>% 
  pivot_longer(cols=c("Tribal police":"Detention"), 
               names_to = "var",
               values_to = "value") %>% 
  group_by(var) %>% 
  left_join(state_map) %>% 
  ungroup() %>% 
  mutate(var = factor(var,
                      levels = c("Tribal police", 
                                 "Arrest members off tribal land",
                                 "Arrest non-Indians on tribal land", 
                                 "Detention")))

ggplot(police_plot,
       aes(x=long, y = lat, fill = value,
           group = group)) + 
  geom_polygon(color = "black") + 
  theme_void() +
  labs(fill = "") +
  theme(legend.position = "bottom") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_distiller(palette = "Spectral") + 
  facet_wrap(~var) + 
  ggsave("./vis/map_inst_police.pdf", width = 8, height = 5)


## Proportion of tribal governments with independent courts, 
## courts handling misdemeanors, and family courts, 2002


courts_plot<-fe_st %>% 
  ungroup() %>% 
  group_by(region) %>% 
  summarise("Courts" = mean(courts),
            "Courts, misdemeanor" = mean(courts_misd),
            "Courts, civil" = mean(courts_civil),
            "Courts, family" = mean(courts_family)) %>% 
  pivot_longer(cols=c("Courts":"Courts, family"), 
               names_to = "var",
               values_to = "value") %>% 
  group_by(var) %>% 
  left_join(state_map) %>% 
  ungroup() 

ggplot(courts_plot,
       aes(x=long, y = lat, fill = value,
           group = group)) + 
  geom_polygon(color = "black") + 
  theme_void() +
  labs(fill = "") +
  theme(legend.position = "bottom") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_distiller(palette = "Spectral") + 
  facet_wrap(~var)+ 
  ggsave("./vis/map_inst_courts.pdf", width = 8, height = 5)

## ## ## ## ## ## ## ## ## 
## Models
## ## ## ## ## ## ## ## ## 

### for now, use state period average
### and quasipoission glm
fe_st_avg<-fe_st %>% 
  filter(.imp==1) %>% 
  group_by(stusps) %>% 
  summarise(fe_deaths = sum(fe_deaths),
            pc_income_ineq = mean(pc_income_ineq),
            fc_entered = ceiling(mean(fc_entered)),
            fc_istpr = ceiling(mean(fc_istpr)),
            fc_caseload = ceiling(mean(fc_caseload)),
            fc_aian_home = mean(fc_aian_home),
            pop_aian_child = ceiling(mean(pop_aian_child)),
            pop_aian_adult = ceiling(mean(pop_aian_adult)))

fe_st_fixed<-fe_st %>% 
  filter(.imp==1) %>% 
  select(stusps, pl280, west_MS,
         police_tribal, police_tribal_off_res,
         police_tribal_non_ind, jail, courts, 
         courts_misd, courts_civil, courts_family,
         pop_pct_aian_aianh, pov_ratio,
         police_sov, court_sov) %>% 
  distinct()

fe_st_model<-fe_st_avg %>% 
  left_join(fe_st_fixed) 

m_fc<-glmer(fc_caseload ~ 
              pl280 + 
              police_sov + 
              court_sov + 
              pop_pct_aian_aianh + 
              pov_ratio +
              (1|stusps) + 
              (1|year),
            data = fe_st %>% 
              filter(.imp==1),
            family = "poisson",
            offset =log(pop_aian_child))

m_tpr<-glmer(fc_istpr ~ 
               pl280 + 
               police_sov + 
               court_sov + 
               pop_pct_aian_aianh + 
               pov_ratio +
               (1|stusps) + 
               (1|year),
             data = fe_st %>% 
               filter(.imp==1),
             family = "poisson",
             offset =log(pop_aian_child))

m_aian_home<-lmer(fc_aian_home ~
                   pl280 + 
                   police_sov + 
                   court_sov + 
                   pop_pct_aian_aianh + 
                   pov_ratio +
                   (1|stusps) +
                  (1|year),
                 data = fe_st %>% 
                   filter(.imp==1))

m_fe<-glmer(fe_deaths ~ 
            pl280 + 
            police_sov + 
            court_sov + 
            pop_pct_aian_aianh + 
            pov_ratio +
            (1|stusps) + 
            (1|year),
          data = fe_st %>% 
            filter(.imp==1),
          family = "poisson",
          offset =log(pop_aian_adult))

### simulation data
stusps<-"XX"
year<-2020
pop_aian_adult<-1e5
pop_aian_child<-1e5
pl280<-c("Mandatory", "Optional", "Non-PL280")
police_tribal<-seq(1,0)
police_tribal_off_res<-c(1,0)
police_tribal_non_ind<-seq(0,1,0.01)
pop_pct_aian_aianh<-0.2
jail<-c(1,0)
courts<-c(1,0)
courts_misd<-c(1,0)
courts_civil<-c(1,0)
courts_family<-seq(0,1,0.01)
pov_ratio<-1.8
police_sov<-0:4
court_sov<-0:4

newdata<-expand_grid(stusps, year,
                     pov_ratio,
                     police_sov, court_sov,
                     pop_aian_adult, pop_aian_child,
                     pop_pct_aian_aianh,
                     pl280)


newdata$fe_yhat<-predict(m_fe, newdata, type = "response", 
                         allow.new.levels = T) * 1e5
newdata$fc_cl_yhat<-predict(m_fc, newdata, type = "response", allow.new.levels = T) * 100


## predict fe along police, police_tribal_off_res

ggplot(newdata %>% 
         filter(court_sov == 2),
       aes(x=police_sov, y = fe_yhat/10, #deaths/100k/yr
           color = pl280)) +
  geom_point() +
  geom_line() + 
  ylab("AIAN deaths per 100,000 per year") +
  xlab("Breadth of tribal police jurisdiction") + 
  labs(color = "PL-280") +
  theme_minimal()

ggplot(newdata %>% 
         filter(police_sov == 0),
       aes(x=court_sov, y = fe_yhat/10, #deaths/100k/yr
           color = pl280)) +
  geom_point() +
  geom_line() + 
  ylab("AIAN deaths per 100,000 per year") +
  xlab("Breadth of tribal courts") + 
  labs(color = "PL-280") +
  theme_minimal()

plot_dat<-bind_rows(newdata %>% 
  filter(court_sov == 2) %>% 
    select(-court_sov) %>% 
    rename(sov = police_sov) %>% 
    mutate(scen = "Police"),
  newdata %>% 
    filter(police_sov == 2) %>% 
    select(-police_sov) %>% 
    rename(sov = court_sov) %>% 
    mutate(scen = "Courts")) %>% 
  distinct()

ggplot(plot_dat,
       aes(x=sov, y = fc_cl_yhat, #deaths/100k/yr
           color = pl280)) +
  geom_point() +
  geom_line() + 
  ylab("Percent of AIAN children in foster care") +
  xlab("Breadth of tribal jurisdiction") + 
  theme_minimal() +
  theme(legend.position = "bottom") + 
  labs(color = "PL 280 Status") +
  facet_wrap(~scen) + 
  ggsave("./vis/fc_cl_regplots.pdf", width = 8, height = 5)


ggplot(plot_dat,
       aes(x=sov, y = fe_yhat, #deaths/100k/yr
           color = pl280)) +
  geom_point() +
  geom_line() + 
  ylab("American Indians killed by police (per 100,000)") +
  xlab("Breadth of tribal jurisdiction") + 
  theme_minimal() +
  theme(legend.position = "bottom") +   
  labs(color = "PL 280 Status") +
  facet_wrap(~scen) + 
  ggsave("./vis/fe_regplots.pdf", width = 8, height = 5)
