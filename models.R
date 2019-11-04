rm(list=ls())
library(tidyverse)
library(lme4)
select<-dplyr::select
theme_set(theme_bw())

### rds produced in read.r
aianh_dat<-readRDS("./data/aianh_dat.rds")
state_dat<-readRDS("./data/state_dat.rds")
afcars_dat<-read_csv("./data/afcars_aian.csv")

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

state_dat$AnyTribalLand<-!(is.na(state_dat$aianh_pov_rt))

#### q0: do tribal lands protect aminds? 
#### - is risk higher on tribal than non-tribal lands?
#### this needs to diff out pop and killings from state total
#### for later: urban rates diff from other groups?

#### q1: are aminds where quality of life on tribal lands > 
#### qol in state less likely to be killed?

#### q2: presence of tribal police force?

### state-level: aian risk as function of ineq
### aian risk as function of avg sovereignty / disadv

###LATER _ MANUALLY CODE RACE ON ALL CASES ON OR WITHIN X MILES OF AIANH

############################################################
### descriptives
############################################################
### state: killings per cap
### natl: aian deaths on / not on aianh
### aianh: by law enf agency type
### by aian / not aian on aianh - look at missing

#### MAKE MAP OF THIS DATA! 3x2 FACET

### simple correlations
### hs/unemp/pov tightly correlated
### negative on income/pov as expected

# ggplot(state_dat, 
#        aes(x = pov_ratio,
#            y = st_income_ineq)) + 
#   geom_point() + 
#   geom_smooth(method = "lm") + 
#   ggsave("vis/bivar_pov_income.png")
# 
# ggplot(state_dat, 
#        aes(x = pov_ratio,
#            y = unemp_ratio)) + 
#   geom_point() + 
#   geom_smooth(method = "lm") + 
#   ggsave("vis/bivar_pov_unemp.png")
# 
# ggplot(state_dat, 
#        aes(x = pov_ratio,
#            y = hs_ratio)) + 
#   geom_point() + 
#   geom_smooth(method = "lm") + 
#   ggsave("vis/bivar_pov_hs.png")
# 
# ggplot(state_dat, 
#        aes(x = pov_ratio,
#            y = sqrt(fe_rt_aian))) + 
#   geom_point() + 
#   geom_smooth(method = "lm") + 
#   ggsave("vis/bivar_pov_fe.png")
# 
# ggplot(state_dat, 
#        aes(x = st_income_ineq,
#            y = sqrt(fe_rt_aian))) + 
#   geom_point() + 
#   geom_smooth(method = "lm") + 
#   ggsave("vis/bivar_inc_fe.png")
# 
# ggplot(state_dat, 
#        aes(x = aianh_pct_tribal_police,
#            y = sqrt(fe_rt_aian))) + 
#   geom_point() + 
#   geom_smooth(method = "lm") + 
#   ggsave("vis/bivar_tribal_police_fe.png")
# 
# ggplot(state_dat, 
#        aes(x = aianh_pct_tribal_police,
#            y = aianh_pct_tribal_jail)) + 
#   geom_point() + 
#   geom_smooth(method = "lm") + 
#   ggsave("vis/bivar_tribal_police_jail.png")
# 
# ggplot(state_dat, 
#        aes(x = aianh_pct_tribal_police,
#            y = aianh_pct_tribal_courts)) + 
#   geom_point() + 
#   geom_smooth(method = "lm") + 
#   ggsave("vis/bivar_tribal_police_courts.png")




####################################################
#### MODELS
####################################################

############ STATE LEVEL ############ 
library(lme4)
state_dat$obs_n<-1:nrow(state_dat)

s0<-glmer(fe_aian ~ 
            log(st_income_ineq) +  
            pl280 + (1|year) + (1|stusps),
          data = state_dat,
          offset = log(total_population_amind),
          family = "poisson")

s1<-glmer(fe_aian ~ 
            log(st_income_ineq) +  
            west_MS + pl280 + 
            (1|year) + (1|stusps),
          data = state_dat,
          offset = log(total_population_amind),
          family = "poisson")
### make scenarios for low-high income ineq, west/east of MS: no pl280
### west of MS, pl280 all scens. 4 sequences of st_income
fake_dat<-data.frame(st_income_ineq = rep(
  seq(from = 0.5, to = 1, length.out = 1000), 4),
  pl280 = c(rep("Non-PL280", 2000),
            rep("Optional", 1000),
            rep("Mandatory", 1000)),
  west_MS = c(rep(FALSE, 1000),
              rep(TRUE, 3000)),
  year = rep(2018, 4000),
  stusps = rep("MN", 4000),
  total_population_amind = rep(1e5, 4000)
)

fake_dat$yhat<-predict(s1, newdata = fake_dat,
                       re.form = ~0)

ggplot(fake_dat,
       aes(x = st_income_ineq,
           y = exp(yhat) * 1e5,
           color = pl280,
           lty = !west_MS)) + 
  geom_line() + 
  labs(y = "Deaths per 100,000",
       color = "Public Law 280 State",
       lty = "West of Mississippi River",
       x = "Native / White income ratio") + 
  ggsave("./vis/preds_state.png")

ggplot(fake_dat,
       aes(x = st_income_ineq,
           y = exp(yhat) * 1e5,
           color = pl280,
           lty = west_MS)) + 
  geom_line() + 
  labs(y = "Deaths per 100,000",
       color = "Public Law 280 State",
       lty = "West of Mississippi River",
       x = "Native / White income ratio") + 
  ggsave("./vis/preds_state.pdf")

############ AIANH LEVEL ############ 
### disadvantage


h4a<-glm(fe_aian ~ 
                scale(pov_rt) + scale(unemp_rt) + scale(hs_rt) + 
                scale(log(pc_income_amind)) + 
              offset(log(total_population_amind)) ,
              data = aianh_dat,
              family = "quasipoisson")

# h4a_ml<-glmer(fe_aian ~ 
#              scale(pov_rt) + scale(unemp_rt) + scale(hs_rt) + 
#              scale(log(pc_income_amind)) + 
#              (1|state),
#            offset = log(total_population_amind) ,
#            data = aianh_dat,
#            family = "poisson")

### sovereignty (legal institutions)

h5a_0<-glm(fe_aian ~ 
             police_tribal + 
             police_tribal_staterecog + 
             jail_tribal + juv_tribal + 
             courts + 
             pl280 + 
             offset(log(total_population_amind + 1)) ,
           data = aianh_dat,
           family = "quasipoisson")

h5a_1<-update(h5a_0, 
              .~. + 
                scale(pov_rt) + scale(unemp_rt) + scale(hs_rt) + 
                scale(log(pc_income_amind)))

############ SIMULATION ############ 
############ SOVEREIGNTY / PL280
mean_dat<-aianh_dat%>%
  summarise_all(mean, na.rm = TRUE)%>%
  mutate(total_population_amind = 1e5)

fake_dat_sov<-aianh_dat[1:6,]
fake_dat_sov[1:6, ]<-mean_dat[1,] 

TF<-c(TRUE, FALSE)
PL280<-c("Mandatory", "Optional", "Non-PL280")
rep_dat<-expand.grid(TF, PL280)
rep_TF<-function(x)x=rep_dat$Var1
fake_dat_sov<-fake_dat_sov%>%
  mutate_at(vars(police_tribal, police_tribal_staterecog,
                       jail_tribal, juv_tribal, 
                       courts), rep_TF)%>%
  mutate(pl280 = rep_dat$Var2)%>%
  select(police_tribal, police_tribal_staterecog,
         jail_tribal, juv_tribal, 
         courts, pov_rt, unemp_rt, hs_rt, pc_income_amind,
         pl280,
         total_population_amind)

yhat<-predict(h5a_1, newdata = fake_dat_sov, 
        se.fit = TRUE)

fake_dat_sov<-fake_dat_sov%>%
  mutate(pred = exp(yhat$fit), 
         pred_upr = exp(yhat$fit + yhat$se.fit),
         pred_lwr = exp(yhat$fit - yhat$se.fit))

ggplot(fake_dat_sov,
       aes(x = police_tribal,
           y = pred,
           ymin = pred_lwr,
           ymax = pred_upr)) + 
  geom_errorbar() + 
  facet_wrap(~pl280) + 
  xlab("Tribal legal institutions present") + 
  ylab("Deaths per 100,000") + 
  # labs(title = "Predicted American Indians killed by police by PL280 status",
  #      subtitle = "Mean prediction +/- 1 SE") + 
  ggsave("vis/preds_pl280.png")

############ Structural disadvantage
### same six scens as above + 
### min-max disadvantage
### vary from min:max by (max-min)/1000
### for each of pov_rt, unemp_rt, hs_rt, pc_income (range from max:min)
len<-1e3
make_seq<-function(x){
  out<-seq(from = 
             quantile(x, 0.1, na.rm = TRUE),
           to = quantile(x, 0.9, na.rm=TRUE), 
           length.out = len)
  return(out)}

make_seq_desc<-function(x){
  out<-seq(from = 
             quantile(x, 0.9, na.rm = TRUE),
           to = quantile(x, 0.1, na.rm=TRUE), 
           length.out = len)
  return(out)}

### stack scenarios
### loop over each row in fake_dat_sov, make len df, bind
scen_out<-list()
for(i in 1:nrow(fake_dat_sov)){
  fake_dat_sov<-as.data.frame(fake_dat_sov)
  scen_temp<-data.frame(police_tribal = 
                          rep(fake_dat_sov[i,"police_tribal"], len),
                        police_tribal_staterecog = 
                          rep(fake_dat_sov[i,"police_tribal_staterecog"], len),
                        jail_tribal = 
                          rep(fake_dat_sov[i,"jail_tribal"], len),
                        juv_tribal = 
                          rep(fake_dat_sov[i,"juv_tribal"], len),
                        courts = 
                          rep(fake_dat_sov[i,"courts"], len),
                        pl280 = 
                          rep(fake_dat_sov[i, "pl280"]),
                        pov_rt = make_seq(aianh_dat$pov_rt),
                        unemp_rt = make_seq(aianh_dat$unemp_rt),
                        hs_rt = make_seq(aianh_dat$hs_rt),
                        pc_income_amind = make_seq_desc(aianh_dat$pc_income_amind),
                        total_population_amind = rep(1e5, len),
                        index = seq(from = 0.1, to = 0.9, length.out = len)
                        )
  scen_out[[i]]<-scen_temp
}

scen_out<-bind_rows(scen_out)

yhat<-predict(h5a_1, newdata = scen_out,
              se.fit = TRUE)

scen_out<-scen_out%>%
  mutate(pred = exp(yhat$fit)/19, 
         pred_upr = exp(yhat$fit + yhat$se.fit)/19,
         pred_lwr = exp(yhat$fit - yhat$se.fit)/19)

ggplot(scen_out%>%
         rename(`Tribal legal institutions` = courts), 
       aes(x = index, 
           y = pred,
           ymax = pred_upr,
           ymin = pred_lwr,
           color = `Tribal legal institutions`,
           fill = `Tribal legal institutions`)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.2, color = NA) + 
  facet_wrap(~pl280) +
  xlab("Structural disadvantage (quantile)") + 
  ylab("Deaths per 100,000") + 
  theme(legend.position = "bottom") + 
  labs(fill = "Tribal legal institutions present",
       color = "Tribal legal institutions present") + 
  ggsave("vis/preds_disadv.png")

ggplot(scen_out%>%
         rename(`Tribal legal institutions` = courts), 
       aes(x = index, 
           y = pred,
           ymax = pred_upr,
           ymin = pred_lwr,
           color = `Tribal legal institutions`,
           fill = `Tribal legal institutions`)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.2, color = NA) + 
  facet_wrap(~pl280) +
  xlab("Structural disadvantage (quantile)") + 
  ylab("Deaths per 100,000") + 
  theme(legend.position = "bottom") + 
  labs(fill = "Tribal legal institutions present",
       color = "Tribal legal institutions present") + 
  ggsave("vis/preds_disadv.pdf")
  
