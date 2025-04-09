# Few outputs take throughout the code:
# (1). select the individual birders that have used in individual choice models
#      this is important as we use birders who made at least 30 trips but in MNL
#      we used all birders
# (2). estimate the individual models and extract the coefficient and var-cov matrix to simulation purpose 
#      of POE test

#Clear memory                                                                  
rm(list = ls())

# load packages                                                                

library(apollo)
library(tidyverse)
library(car)
library(furrr)
library(progressr)
library(ggExtra)
library(rlist)
library(mded)

#Initialize code                                                               

apollo_initialise( )

#------------------------------------------------------------------------------#

#load data
database = read_csv("data/processed/ApolloData_nalt500_MNL_mc_halfWR.csv")
#database = database[!(database$observer_id=="obsr272870"),] # higher standard error

# filter the data to extract individual who took 30 trips
database = database %>%
  dplyr::select(observer_id, choice_id, choice, starts_with("tc_"),
         starts_with("sr_"), 
         starts_with("avail_")) 

database2 = database %>%
  dplyr::select(observer_id, choice_id, starts_with("tc_")) %>%
  pivot_longer(-c(observer_id, choice_id), values_to = "tc") %>%
  mutate(site = parse_number(name)) %>%
  dplyr::select(-name)
database2 = database %>%
  dplyr::select(observer_id, choice_id, starts_with("avail_")) %>%
  pivot_longer(-c(observer_id, choice_id), values_to = "avail") %>%
  mutate(site = parse_number(name)) %>%
  dplyr::select(-name) %>%
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
  filter(n>=30)
# keep individuals who made trips > 30 
keep_id = df_indiv$observer_id
database = database %>%
  filter(observer_id %in% keep_id)

# preparing database for apollo
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

#save data base for use the same individual for MNL model

write.csv(database, "./data/processed/POE.csv")
write.csv(df_indiv, "./data/processed/indi_trips.csv")

#Set core controls  

apollo_control = list(
  outputDirectory = "output",
  modelName ="mnl_1" ,
  modelDescr ="MNL model with ebird data " ,
  indivID = "observer_id",
  #  nCores = 6,
  workInLogs = T)


#Iterative coding for alternatives                                             
J = database %>%
  dplyr::select(starts_with("tc_")) %>%
  ncol(.)

#loop-1 : creating ascs for sites (Define model parameters)                    

# No ascs for these models yet
#asc = list ()
#for ( j in 1:J ) asc[[ paste0("alt", j ) ]] = paste0("asc_", j)


#loop-2 : Set all of \ asc = 0                                                 
# remove or include b_sr2 term based on the model (species richnes inclusion or

apollo_beta = c(b_tc = -.2,
                b_sr = .2)

apollo_fixed = c()

#Apollo_probabilities function 

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
      b_sr * get(paste0("sr_", j))
    
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

# extract coefficients in each individual model
estimate <- do.call("rbind", lapply(out, "[[","estimate"))%>%
  as.data.frame() %>%
  mutate(id = 1:length(database_list))
#estimate <- cbind(rownames(estimate),estimate)
#colnames(estimate)[1] = "id"

# write csv file with coefficients as it needed for POE test simulations
write.csv(estimate, "./data/estimate_indi.csv")

# write csv file with number of trips as it needed for POE test simulations
num_trips <- do.call("rbind", lapply(out, "[[","nObs"))%>%
  as.data.frame() %>%
  mutate(id = 1:length(database_list))

write.csv(num_trips, "./data/num_trips.csv")


vcov_estimate <- do.call("rbind", lapply(out, "[[","varcov"))%>%
  as.data.frame() %>%
  mutate(id = rep(1:length(database_list), each = 2))

# write csv file with coefficients as it needed for POE test simulations
write.csv(vcov_estimate, "./data/cov_indi.csv")


