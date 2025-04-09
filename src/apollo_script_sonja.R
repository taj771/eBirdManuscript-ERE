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
  nCores = 1,
  calculateLLC = F,
  workInLogs = T)


#------------------------------------------------------------------------------#                                    #
#set file nalt to 5 change as it needed
#------------------------------------------------------------------------------#
database = read_csv("sample_100.csv")

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
apollo_beta = c(b_tc = 0,
                b_sr = 0)

apollo_fixed = c()

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

model = apollo_estimate(apollo_beta, 
                        apollo_fixed, 
                        apollo_probabilities, 
                        apollo_inputs)

#-----------------------------------#
#Save the results 
#-----------------------------------#
apollo_modelOutput(model)

apollo_modelOutput(model)

apollo_saveOutput(model)

