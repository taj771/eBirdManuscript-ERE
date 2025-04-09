# Clean memory 
rm(list = ls())

library(tidyverse)
library(rlist)
library(mded)
#library(turner)
library(MASS)
# load library
library(hrbrthemes)
library(viridis)

# set seed - as simulating and need to get the same results
set.seed(1234)

# number of simulations

num_sims = 1000

# read estimats from individual models
estimate <- read.csv("./data/estimate_indi.csv") %>%
  dplyr::select(b_tc, b_sr)

# number of trips - to weight the individual models
num_trips = read_csv("./data/num_trips.csv")

weight <- num_trips %>%
  mutate(weight = V1/mean(V1))%>%
  dplyr::select(id, weight)

mu = split(as.matrix(estimate),seq(nrow(estimate)))   

# read var-cov 
cov <- read.csv("./data/cov_indi.csv")

var = cov %>% 
  dplyr::select(b_tc, b_sr, id) %>%
       group_split(id, .keep = F)  %>%
  map(as.matrix)

sim_indiv <- map2(mu, var, ~mvrnorm(mu = .x, Sigma = .y, n= num_sims))

weight_list = as.list(weight$weight)

wtp_indiv <- map2(sim_indiv, weight_list, function(x, y){
  out = x[, 2] / -x[, 1] * y
  return(out)
})

wtp_indiv = bind_rows(wtp_indiv) %>%
  rowMeans(.)

################################################################################################
# MNL model estimations 
# simulate coefficient from MNL model (model 1)
# read model output
model <- readRDS("./output/mnl_base_model.rds")

# simulation based on MNL model estimations
wtp_mnl = mvrnorm(n = num_sims,
                 mu = model[["estimate"]],
                 Sigma = model[["robvarcov"]]) %>%
  as.data.frame(.) %>%
  mutate(wtp = b_sr/-b_tc)

wtp_mnl = wtp_mnl$wtp
  

#############################################################################################################
# Summary

df_wtp = tibble(model = "indiv",
                sim_id = 1:num_sims,
                wtp = wtp_indiv) 

df_wtp = tibble(wtp = wtp_mnl,
         sim_id = 1:num_sims,
         model = "mnl") %>%
  bind_rows(df_wtp)

summary(wtp_indiv)
summary(wtp_mnl)

# Poe test
# H0  wtp_MNL = wtp_indi 
# H1  wtp_MNL > wtp_indi 
mded(wtp_indiv,wtp_mnl, detail = T, independent = T)

########################################################################################################
# Create distribution graph



cols <- c("gray10", "gray70")


p <- ggplot(df_wtp, aes(x = wtp, fill = model)) +
  geom_density(adjust=10,alpha = 0.4) + 
  scale_x_continuous(limits = c(-10, 10))+
  scale_fill_manual(values = cols,
                    name = "",
                    breaks = c("indiv","mnl"),
                    labels = c("Individual Models", "RUM model"))+
  theme_bw()+
  labs(x = "MWTP", y = "Density")

p 
