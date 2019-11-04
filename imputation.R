#### Risk of being killed by police in the U.S. by age, race/ethnicity, and sex
#### Frank Edwards, Hedwig Lee, Michael Esposito
#### impute missing race/ethnicity data from fatal encounters
### based on surname and county population
library(tidyverse)
library(mice)
library(xtable)

source("imputation_pre_process.R")

### drop cases prior to 2008
### based on EDA with NVSS, 2008-18 
### higher quality than 00-07 (possible censorship)
fe_imp_dat_join<-fe_imp_dat_join%>%
  filter(as.numeric(as.character(year))>2007)

fe_imp_dat_join<-fe_imp_dat_join %>% 
  select(-age, -sex)

# ### make table of missing values by variable
# tab.out<-xtable(fe_imp_dat_join%>%
#   summarise_all(funs(signif(sum(is.na(.))/n() * 100)),3)%>%
#   select(age, sex, race, fe_cause_of_death)%>%
#   rename(Age = age, Sex = sex, Race = race, `Cause of death` = fe_cause_of_death),
#   caption = "Focal variables missing values in Fatal Encounters, percent of cases 2008 - 2018",
#   label = "tab:pct_var",
#   digits = 3)
# 
# print(tab.out, include.rownames = FALSE,
#       file = "./vis/pct_var.tex")

####################################################
#### impute!
####################################################
fe_imp_setup<-mice(fe_imp_dat_join,
             maxit = 0,
             m=1,
             seed = 1)
### set predictor matrix, disable id as predictor
preds<-fe_imp_setup$predictorMatrix
preds[1,]<-0
preds[,1]<-0
preds[,2]<-0
preds[,3]<-0

### default methods are good here

fe_imp<-mice(fe_imp_dat_join,
             maxit =10,
             m=5,
             seed = 1,
             predictorMatrix = preds)

saveRDS(fe_imp, "imputations.rds")

####################################################
## Diagnose!
####################################################
# pdf("./vis/imp_trace.pdf")
# plot(fe_imp)
# dev.off()
# pdf("./vis/imp_density_race.pdf")
# densityplot(fe_imp, ~race)
# dev.off()
# pdf("./vis/imp_density_age.pdf")
# densityplot(fe_imp, ~age)
# dev.off()


### latinos see a dip in pct, amind and asian slight dip, 
### black incarease, white increase
### do more diagnostics with mice() vignettes

### fit probability of missing conditional on surname posterior probability - maybe 
### hispanic surnames getting id'd more often in original data?
### maybe Black has higher hit rate in original based on photos?

### method from https://stefvanbuuren.name/fimd/sec-diagnostics.html
### via https://doi.org/10.1002/sim.6926

#### OUTPUT IMPUTED

### read in imputed data and format for merge
dat<-mice::complete(read_rds("imputations.rds"),
                    action = "long", 
                    include = FALSE)

### create age/sex/race/year/causeofdeath data

write_csv(dat, "./data/fe_imputed_08_18.csv")