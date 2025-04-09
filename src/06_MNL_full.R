################################################################################
# Title: model 5                                                               #
# Date: 9/29/2021                                                              #
# Description: MNL model estimation - basic model                              #
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
#------------------------------------------------------------------------------#
#Initialize code                                                               #
#------------------------------------------------------------------------------#
apollo_initialise( )

#------------------------------------------------------------------------------#
#Set core controls                                                             #
#------------------------------------------------------------------------------#
apollo_control = list(
  outputDirectory = "output",
  modelName ="mnl_base" ,
  modelDescr ="MNL model with ebird data " ,
  indivID = "observer_id",
  nCores = 6,
  calculateLLC = F,
  workInLogs = T)


#------------------------------------------------------------------------------#
# load the .Rdata                                                              #
#set file nalt to 5 change as it needed
#------------------------------------------------------------------------------#
database = read_csv("./data/processed/ApolloData_nalt500.csv")

################################################################################
# Divide TC by 10 to for convergence                                           #
################################################################################
database <- database %>%
  mutate_at(vars(starts_with("tc_")), ~./ 10 ) %>%
  mutate_at(vars(starts_with("sr_")), ~./ 10 ) %>%
  arrange(choice_id)

################################################################################
# Need to set to data.frame for apollo                                         #
################################################################################
database <- as.data.frame(database)

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
apollo_beta = c(b_tc = 0,
                b_sr = 0,
                b_sr2 = 0,
                b_es = 0,
                b_2nd_sr = 0,
                b_3rd_sr = 0,
                b_4th_sr = 0,
                b_pr_nat = 0,
                b_pr_prov = 0)


################################################################################
#Home is a fixed parameter                                                     #
################################################################################
apollo_fixed = c()

################################################################################
apollo_beta=apollo_readBeta(apollo_beta,apollo_fixed,"mnl_base",overwriteFixed=FALSE)
################################################################################

################################################################################
#Validation and preparing inputs
################################################################################
apollo_inputs=apollo_validateInputs()
apollo_inputs$J = J # need to retain J (number of alternatives) for use inside apollo_probabilities

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
    park = get(paste0("park_",j))
    sr_j = get(paste0("sr_", j))
    V[[ paste0('choice', j ) ]] = b_tc * get(paste0('tc_', j)) + 
      b_sr * get(paste0("sr_", j)) + 
      b_sr2 * get(paste0("sr_", j))^2 +
      b_es * get(paste0("es_", j)) +
      b_2nd_sr * get(paste0("sr_", j)) *(season == "spring") + 
      b_3rd_sr * get(paste0("sr_", j)) *(season == "summer") + 
      b_4th_sr * get(paste0("sr_", j)) *(season == "fall") +
      b_pr_nat *(get(paste0("park_",j)) == "nat") +
      b_pr_prov *(get(paste0("park_",j)) == "prov")
    
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

model = apollo_estimate(apollo_beta, 
                        apollo_fixed, 
                        apollo_probabilities, 
                        apollo_inputs)

#-----------------------------------#
#Save the results 
#-----------------------------------#
apollo_modelOutput(model)

apollo_modelOutput(model, modelOutput_settings=list(printPVal=2) )

apollo_saveOutput(model, saveOutput_settings=list(printPVal=2) )


#-----------------------------------#
#TWTP & MWTP - species richenss
#-----------------------------------#

#TWTP by number of species
#replace appropriate parameter
#minimum = 1
#25th percentile = 39
#50th percentile = 72
#75th percentile = 112
#maximum = 257
# mean = 79.48
# average nat parks = 0.069
# average prove park = 0.1474
# average outside park = 0.7834

library(pacman)
p_load(tidyverse, car, apollo, haven, sjmisc)

deltaMethod(model, "((b_sr/10)*1 + 
                    (b_sr2/100)*1 +
                    (b_es)*2.22 +
                    (b_2nd_sr/10)*1 + 
                    (b_3rd_sr/10)*1 + 
                    (b_4th_sr/10)*1 +
                    (b_pr_nat)*0.069 +
                    (b_pr_prov)*0.1474)/ (-b_tc/10)") 

#TWTP by quarter
#replace appropriate quarter
#here we used mean of n_species = 79.48

deltaMethod(model, "((b_sr/10)*79.48 +
                     (b_sr2/100)*6317+
                     (b_es)*2.22 +
                     (b_4th_sr/10)*70.48 +
                    (b_pr_nat)*0.069 +
                    (b_pr_prov)*0.1474)/ (-b_tc/10)") 

#MWTP
# calculate MWTP
# replace appropriate quarter
deltaMethod(model, "(b_4th_sr/10)/ (-b_tc/10)") 

#apollo_saveOutput(model)
