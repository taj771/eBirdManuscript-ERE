################################################################################
# Title: model 5                                                               #
# Date: 9/29/2021                                                              #
# Description: MNL model estimation                                            #
################################################################################

#------------------------------------------------------------------------------#
#Clear memory                                                                  #
#------------------------------------------------------------------------------#
rm(list = ls())

#------------------------------------------------------------------------------#
# load packages                                                                #
#------------------------------------------------------------------------------#
library(apollo)
library(tidyverse)
library(car)
library(furrr)
library(progressr)
library(ggExtra)
#------------------------------------------------------------------------------#
#Initialize code                                                               #
#------------------------------------------------------------------------------#
apollo_initialise( )

#------------------------------------------------------------------------------#
# load the .Rdata                                                              #
#set file nalt to 5 change as it needed
#------------------------------------------------------------------------------#
database = read_csv("data/processed/ApolloData_nalt500_MNL_mc.csv")
database = database[!(database$observer_id=="obsr272870"),] # higher standard error



database = database %>%
  select(observer_id, choice_id, choice, starts_with("tc_"),
         starts_with("sr_"), #starts_with("es_"), 
         starts_with("avail_"))  

database2 = database %>%
  select(observer_id, choice_id, starts_with("tc_")) %>%
  pivot_longer(-c(observer_id, choice_id), values_to = "tc") %>%
  mutate(site = parse_number(name)) %>%
  select(-name)

database2 = database %>%
  select(observer_id, choice_id, starts_with("avail_")) %>%
  pivot_longer(-c(observer_id, choice_id), values_to = "avail") %>%
  mutate(site = parse_number(name)) %>%
  select(-name) %>%
  left_join(database2)

database3 = database2 %>%
  mutate(tc2 = if_else(avail > 0 & is.na(tc),1, 0)) %>%
  filter(tc2>0)

remove_id = database3$choice_id

database = database %>%
  filter(!(choice_id %in% remove_id))

rm(database2, database3)
gc()

df_indiv = database %>%
  group_by(observer_id) %>%
  tally() %>%
  arrange(-n) %>%
  filter(n>=15)

keep_id = df_indiv$observer_id


database = database %>%
  filter(observer_id %in% keep_id)

database <- database %>%
  mutate_at(vars(starts_with("tc_")), ~./ 10 ) %>%
  mutate_at(vars(starts_with("sr_")), ~./ 10 ) %>%
  arrange(observer_id, choice_id)

database = as.data.frame(database)

database_list <- split(database, database$observer_id)

num_trips = map_dfr(database_list, nrow) %>%
  t(.) %>%
  as.data.frame(.) %>%
  rownames_to_column(var = "observer_id") %>%
  rename(num_trips = V1)

names(database_list) <- rep("database",length(database_list))


#database = database %>%
#  filter(observer_id == "obsr24588")
#1 obsr283574   3240
#2 obsr24588    1364
#3 obsr96302    1250
#4 obsr267011   1209
#5 obsr326091   1199
#6 obsr117062   1154
#7 obsr723443   1084
#8 obsr272870   1050
#9 obsr382575    937
#10 obsr267531    896

#  database = x
#------------------------------------------------------------------------------#
#Set core controls                                                             #
#------------------------------------------------------------------------------#
apollo_control = list(
  outputDirectory = "output",
  modelName ="mnl_1" ,
  modelDescr ="MNL model with ebird data " ,
  indivID = "observer_id",
  #  nCores = 6,
  workInLogs = T)

################################################################################
#Iterative coding for alternatives                                             #
################################################################################
J = database %>%
  select(starts_with("tc_")) %>%
  ncol(.)

################################################################################
#loop-1 : creating ascs for sites (Define model parameters)                    #
################################################################################
# No ascs for these models yet
#asc = list ()
#for ( j in 1:J ) asc[[ paste0("alt", j ) ]] = paste0("asc_", j)

################################################################################
#loop-2 : Set all of \ asc = 0                                                 #
# remove or include b_sr2 term based on the model (species richnes inclusion or#
################################################################################
apollo_beta = c(b_tc = -.2,
                b_sr = .2)#,
#   b_sr2 = 0,
#   b_es = .2)#,
#    b_2nd_sr = 0,
#    b_3rd_sr = 0,
#    b_4th_sr = 0,
#    b_pr_nat = 0,
#    b_pr_prov = 0)


################################################################################
#Home is a fixed parameter                                                     #
################################################################################
apollo_fixed = c()

################################################################################
#Apollo_probabilities function 
################################################################################

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate") {
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  V= list( ) 
  avail = list()
  
  # set up utilities for hotspots
  for ( j in 1:apollo_inputs$J ) {
    #   park = get(paste0("park_",j))
    
    V[[ paste0('choice', j ) ]] = b_tc * get(paste0('tc_', j)) + 
      b_sr * get(paste0("sr_", j))# + 
    #     b_sr2 * (get(paste0("sr_", j)))^2 +
    #     b_es * get(paste0("es_", j)) #+
    #    b_2nd_sr * get(paste0("sr_", j))*(season == "spring") + 
    #    b_3rd_sr * get(paste0("sr_", j))*(season == "summer") + 
    #    b_4th_sr * get(paste0("sr_", j))*(season == "fall") +
    #   b_pr_nat * (park == "nat")  +
    #    b_pr_prov * (park == "prov")
    
    avail[[paste0("choice", j)]]=get(paste0("avail_", j))
  }
  
  
  ###mnl_settings
  mnl_settings = list(
    alternatives = setNames (1:apollo_inputs$J,  names(V)) ,
    avail = avail ,
    choiceVar = choice ,
    V = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual (recognizing the repeated choice nature of our data)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}




apollo_inputs=apollo::apollo_validateInputs()
apollo_inputs$J = J # need to retain J (number of alternatives) for use inside apollo_probabilities

rm(database)

EstimateApollo = function(database){
  p()
  database = as.data.frame(database)
  apollo_inputs$database=as.data.frame(database)
  
  
  model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
  return(model)
}

plan(cluster, workers = 4)
with_progress({
  p <- progressor(steps = length(database_list))
  
  out = future_map(database_list, EstimateApollo)
})

closeAllConnections()

wtp = map_dfr(out, function(x){
  #out1 = out[[1]]
  if(x[["message"]] == "successful convergence ")
    tt = deltaMethod(x, "(b_sr )/ -b_tc")
  else
    tt = tibble(Estimate = 0,
                SE = 0,
                `2.5 %` = 0,
                `97.5 %` = 0) })


low_cutoff = -10
high_cutoff = 15
wtp2 = bind_cols(num_trips, wtp) %>%
  filter(num_trips > 14,
         low_cutoff < Estimate, 
         Estimate < high_cutoff, 
         !is.nan(SE),
         !is.infinite(SE)) %>%
  arrange(Estimate) %>%
  mutate(zstat = abs(Estimate/SE),
         rank = 1:nrow(.),
         Estimate = round(Estimate,2)) 

p = wtp2 %>%
  mutate(`2.5 %` = ifelse(`2.5 %` < low_cutoff, low_cutoff, `2.5 %`),
         `97.5 %` = ifelse(`97.5 %`>high_cutoff, high_cutoff, `97.5 %`)) %>%
  ggplot(aes(y = Estimate, x = rank)) +
  geom_point(alpha = 0.5) +
  # geom_point(aes(size = num_trips)) +
  geom_hline(yintercept=0) +
  #  geom_hline(yintercept=mean(wtp2$Estimate)) +
  geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`), width = 0.1) +
  ylab("WTP for one additional species") +
  xlab("Individual birders") +
  #  coord_cartesian(ylim = c(-15,25)) +
  theme_bw()

p =ggMarginal(p, type = "histogram", margins = c("y"), binwidth = .5)
p
ggsave("output/inidividual15_wtp.png", plot = p, height = 10, width = 8)


#Note: Inidividual birders are arranged on the horizontal axis in increasing order of their estimated 
#willingness-to-pay (WTP) for an additional bird species. 
#For each birder, a dot plots the point estimate of the WTP and a vertical bar represents the 95% confidence
#interval. Results are from the baseline model with only travel costs and expected species.

mean(wtp2$Estimate)