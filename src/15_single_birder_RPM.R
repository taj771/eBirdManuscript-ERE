### Clear memory
rm(list = ls())


database <- read.csv("./data/processed/ApolloData_nalt50.csv") 
#library(plyr)
library(dplyr)
library(tidyverse)
#y=count(database,"observer_id")
df <- subset(database,observer_id == ("obsr1003923","obsr1004857"))

write.csv(df,"./data/processed/ApolloData.csv")

### Load Apollo library
library(apollo)
library(tidyverse)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  outputDirectory = "output",
  modelName ="MXL" ,
  modelDescr ="MXL model with ebird data " ,
  indivID = "observer_id",
  mixing    = TRUE,
  workInLogs = T,
  nCores = 6) 

#------------------------------------------------------------------------------#
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                                   #
#------------------------------------------------------------------------------#
database <- read.csv("./data/processed/ApolloData.csv") 

#------------------------------------------------------------------------------#
# Divide TC by 10 to for convergence
#------------------------------------------------------------------------------#
database <- database %>%
  mutate_at(vars(starts_with("tc_")), ~./ 10 ) %>%
  mutate_at(vars(starts_with("sr_")), ~./ 10 ) %>%
  arrange(choice_id)

# only keep variables using
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
J = length(unique(database$choice))



# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation

apollo_beta = c(b_tc = -0.01,
                mu_sr = 0.01,
                #b_sr2 = -0.004933,
                #b_spe_ri = 0.01,
                #b_pr_nat = 0.01,
                #b_pr_prov = 0.01,
                #b_pr_oth = 0.01,
                #b_2nd_sr = 0.038308,
                #b_3rd_sr = 0.02034,
                #b_4th_sr = 0.08980,
                #sigma_sr = 0.001,
                sigma_sr = 0.001)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()


# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 50,
  interUnifDraws = c(),
  interNormDraws = c("draws_sr"),#"draws_sr2","draws_spe_ri","draws_end",
  #  "draws_2nd_sr", "draws_3rd_sr", "draws_4th_sr"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_sr"]] = mu_sr + sigma_sr * draws_sr
  #randcoeff[["b_sr2"]] = mu_sr2 + sigma_sr2 * draws_sr2
  #randcoeff[["b_spe_ri"]] = mu_spe_ri + sigma_spe_ri * draws_spe_ri
  #randcoeff[["b_2nd_sr"]] = mu_2nd_sr + sigma_2nd_sr * draws_2nd_sr
  #randcoeff[["b_3rd_sr"]] = mu_3rd_sr + sigma_3rd_sr * draws_3rd_sr
  #randcoeff[["b_4th_sr"]] = mu_4th_sr + sigma_4th_sr * draws_4th_sr
  return(randcoeff)
}


# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate") {
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  J = 51 # need this if parallel (nCores > 1 in apollo_controls)
  
  V= list( ) 
  avail = list()  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  
  # set up utilities for hotspots
  for ( j in 1:J ) {
    V[[ paste0('choice', j ) ]] = b_tc * get(paste0('tc_', j)) + 
      b_sr * get(paste0("sr_", j)) 
    #b_sr2 * get(paste0("sr2_", j))+  
    #b_spe_ri * get(paste0("es_", j)) +
    #b_pr_nat * get(paste0("nat_pr_",j))  +
    #b_pr_prov * get(paste0("prov_pr_",j)) +
    #b_pr_oth * get(paste0("otr_pr_",j))  
    #b_2nd_sr * get(paste0("sr_", j))*(month == 2) +
    #b_3rd_sr * get(paste0("sr_", j))*(month == 3) + 
    #b_4th_sr * get(paste0("sr_", j))*(month == 4) 
    avail[[paste0("choice", j)]]=get(paste0("avail_", j))
  }
  
  ### Define settings for MNL model component
  ###mnl_settings
  mnl_settings = list(
    alternatives = setNames (1:J,  names(V)) ,
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

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

#-----------------------------------#
#Save the results 
#-----------------------------------#
apollo_modelOutput(model)


