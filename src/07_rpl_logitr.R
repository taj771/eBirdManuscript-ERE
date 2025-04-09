################################################################################
# Title: model 7                                                               #
# Date: 9/29/2021                                                              #
# Description: RP model estimation                                            #
################################################################################

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)
library(tidyverse)
library(logitr)
library(mlogit)

#------------------------------------------------------------------------------#
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                                   #
#------------------------------------------------------------------------------#
database <- read.csv("./data/processed/ApolloData_nalt500.csv") 

#------------------------------------------------------------------------------#
# Divide TC by 10 to for convergence
#------------------------------------------------------------------------------#
database <- database %>%
  mutate_at(vars(starts_with("tc_")), ~./ 10 ) %>%
  mutate_at(vars(starts_with("sr_")), ~ log(. + 1 )) %>%
  #  mutate_at(vars(starts_with("sr_")), ~./ 10 ) %>%
  arrange(choice_id)  %>%
  arrange(observer_id)

# only keep variables using in estimation
dat = database %>%
  select(observer_id, choice_id, choice, starts_with("tc_"),
  starts_with("sr_"), starts_with("avail_")) %>%
  sample_n(2000) %>%
  pivot_longer(-c(observer_id:choice), names_to = c("type", "site"),
               names_sep = "_",
               values_to = "value") %>%
  pivot_wider(#-c(observer_id, choice_id, site, choice),
               names_from = "type",
               values_from = "value") %>%
  filter(avail == 1) %>%
  mutate(observer_id = as.numeric(as.factor(observer_id)),
         site = as.numeric(site),
         choice = ifelse(choice == site, 1, 0)) %>%
  group_by(observer_id, site) %>%
  mutate(choice_id2 = seq(n())) %>%
  arrange(observer_id, choice_id2, site) %>%
  select(-avail) %>%
  select(id = observer_id, obs_id = choice_id, site, choice, tc, sr) %>%
  ungroup(.)

gc()

summary(database)

mnl_pref <- logitr(
  data    = dat,
  panelID = "id",
  startVals = c(-.23, 2.7),
  outcome = 'choice',
  numCores = 6,
  robust = T,
  obsID   = 'obs_id',
  options = list(print_level = 1),
  pars    = c("tc", "sr"))

summary(mnl_pref)

dat %>%
  group_by(id, obs_id) %>%
  summarise(choice = sum(choice)) %>%
  filter(choice != 1)

mxl_pref <- logitr(
  data    = dat,
  panelID = "id",
  startVals = c(-.2, 3, 3),
  outcome = 'choice',
  scaleInputs = FALSE,
  numCores = 10,
#  robust = T,
# numMultiStarts = 10,
  obsID   = 'obs_id',
  options = list(print_level = 1),
  pars    = c("tc", "sr"),
  randPars = c(sr = "n"))

summary(mxl_pref)

dat2 <- dfidx(dat, 
             # id.var = "id",
            #  chid.var = "obs_id")

              idx = c("obs_id", "site"))
#   chid.var = "choice_id2",
#idx = c("observer_id", "choice_id"))#,
#alt.var = "site",
#id.var = "observer_id")

mnl1 <- mlogit(choice ~ tc + sr | 0, dat2, 
               start = c(-.2, .2),
               print.level = 2 )
summary(mnl1)

rpl1 <- mlogit(choice ~ tc + sr | 0, dat2, 
                   start = c(-.2, .2, .2),
                   rpar=c(sr = 'n'), 
                   print.level = 2,
                   panel =T,
               R = 50, 
               halton = NA   )
summary(rpl1)
