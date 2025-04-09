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

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  outputDirectory = "output",
  modelName ="rpl_log_sr" ,
  modelDescr ="MXL model with ebird data " ,
  indivID = "observer_id",
  mixing    = TRUE,
  workInLogs = T,
  calculateLLC = F,
  nCores = 6) 

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
  arrange(choice_id) %>%
  sample_n(20000) %>%
  arrange(observer_id)

# only keep variables using in estimation
database = database %>%
  select(observer_id, choice_id, choice, starts_with("tc_"),
  starts_with("sr_"), starts_with("avail_"))

#-----------------------------------#
# Need to set to data.frame for apollo
#-----------------------------------#
database <- as.data.frame(database)

#-----------------------------------#
#Iterative coding for alternatives
#-----------------------------------#
J = database %>%
  select(starts_with("tc_")) %>%
  ncol(.)

gc()
# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation

apollo_beta = c(b_tc = -0.23,
                b_sr = 3.86,
                sigma_sr = 2.26)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()

#apollo_beta=apollo_readBeta(apollo_beta,apollo_fixed,apollo_control$modelName,overwriteFixed=FALSE)

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 50,
  interUnifDraws = c(),
  interNormDraws = c("draws_sr"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["beta_sr"]] = b_sr + sigma_sr * draws_sr
  return(randcoeff)
}


# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

summary(database)

apollo_inputs = apollo_validateInputs()
apollo_inputs$J = J # need to retain J (number of alternatives) for use inside apollo_probabilities
rm(database)
gc()
# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate") {
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  V= list( ) 
  avail = list()  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant

  # set up utilities for hotspots
  for ( j in 1:apollo_inputs$J ) {
    V[[ paste0('choice', j ) ]] = b_tc * get(paste0('tc_', j)) + 
      beta_sr * get(paste0("sr_", j)) 
    avail[[paste0("choice", j)]]=get(paste0("avail_", j))
  }
  
  ### Define settings for MNL model component
  ###mnl_settings
  mnl_settings = list(
    alternatives = setNames (1:apollo_inputs$J,  names(V)) ,
    avail = avail ,
    choiceVar = choice ,
    V = V
  )
  
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

#apollo_speedTest(apollo_beta,
#                 apollo_fixed,
#                 apollo_probabilities,
#                 apollo_inputs)


model = apollo_estimate(apollo_beta, 
                        apollo_fixed, 
                        apollo_probabilities, 
                        apollo_inputs)

#-----------------------------------#
#Save the results 
#-----------------------------------#
apollo_modelOutput(model)

apollo_saveOutput(model)

