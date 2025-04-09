#########################################################################################
# Here we estimate MNL model with same individuals that estimate individual choice models
# Few things done here
# (1). Estimate MNL model with same individals in individual choice models and extract 
#      the coefficient and sd to simulate during POE test
# (2). MNL model estimated with a given weighted based on number of trips as it necessary to 
#      compare it with the results of individual choice models.

# load data
# This is results from individual model code and ensure we using the same individuals 
database = read_csv("./data/processed/POE.csv")
# datafarem with individuals and their number of trips
# based on the number of trips I have weighted the MNL
df_indiv = read_csv("./data/processed/indi_trips.csv")

# define weight - based on the number of trips. i.e. If an individual made 30 trips then
# each trip made by that individual receive a weight o 1/30
weight <- df_indiv%>%
  mutate(WEIGHT = 1/n)%>%
  select(observer_id,WEIGHT)
# attach weights to database
database <- database%>%
  left_join(weight)

# appollo
#Initialize code                                                               
apollo_initialise( )

#Set core controls                                                             
apollo_control = list(
  outputDirectory = "output",
  modelName ="mnl_base" ,
  modelDescr ="MNL model with ebird data " ,
  indivID = "observer_id",
  #  weights = "WEIGHT",
  nCores = 6,
  calculateLLC = F,
  workInLogs = T)

# Divide TC by 10 to for convergence                                           
database <- database %>%
  select(-starts_with(c("park_", "es_"))) %>%
  arrange(choice_id)

# Need to set to data.frame for apollo                                         
database <- as.data.frame(database)

#Iterative coding for alternatives                                             
J = database %>%
  dplyr::select(starts_with("tc_")) %>%
  ncol(.)

#loop-1 : creating ascs for sites (Define model parameters)                    
# No ascs for these models yet
#asc = list ()
#for ( j in 1:J ) asc[[ paste0("alt", j ) ]] = paste0("asc_", j)

#loop-2 : Set all of \ asc = 0                                                 
# remove or include b_sr2 term based on the model (species richnes inclusion or#
apollo_beta = c(b_tc = 0,
                b_sr = 0)

#Home is a fixed parameter                                                     
apollo_fixed = c()

apollo_beta=apollo_readBeta(apollo_beta,apollo_fixed,apollo_control$modelName,overwriteFixed=FALSE)

#Validation and preparing inputs
apollo_inputs=apollo_validateInputs()
apollo_inputs$J = J # need to retain J (number of alternatives) for use inside apollo_probabilities

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
  
  ### weigh the observations
  #  P = apollo_weighting(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model = apollo_estimate(apollo_beta, 
                        apollo_fixed, 
                        apollo_probabilities, 
                        apollo_inputs)



#Save the results 
apollo_modelOutput(model)

apollo_modelOutput(model, modelOutput_settings=list(printPVal=2) )

apollo_saveOutput(model, saveOutput_settings=list(printPVal=2) )
