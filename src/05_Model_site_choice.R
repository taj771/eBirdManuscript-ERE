################################################################################
# Title: model 5                                                               #
# Date: 9/29/2021                                                              #
# Description: Model site choice                                               #
################################################################################

#-------------------------------------------------------------------------------
#Clear memory 
#-------------------------------------------------------------------------------
rm(list = ls())


# Load packages
library(tidyverse)
library(lubridate)
library(furrr)

#-------------------------------------------------------------------------------
# Select these variables

# Number of workers for parallel processing
n_workers = 6
# Select consideration set based on time or distance
# consider_time = 1 if time
# consider_time = 0 if distance
consider_time = 1

# low/high end of stright line travel cost distance km
travel_cutoff_lo = 5
travel_cutoff_hi = 120

travel_hours_lo = 5/60 # 5/60 is 5 minutes
travel_hours_hi = 2 #

# Choose number of non-chosen alternatives to sample
# start with n_alt =5 please change n_alt as needed.
n_alts = 200

#-------------------------------------------------------------------------------
# Function to sample non-chosen consideration_sets for each person
# Works at the individual choice level
# choices: dataframe with one row with observer_id, locality_id, year, month
# consideration_sets: dataframe with consideration_sets for each observer_id
# n_alts: number of non-chosen alternatives to include
# 
# returns out: dataframe with full consideration set (chosen and n_alt non-chosen alternatives)
SampleAlternatives = function(choices, consideration_sets, n_alts){
  
  trip_other = choices %>%
    select(-choice, -locality_id)
  
  choices = choices %>%
    select(observer_id, locality_id, choice)
  
  # Return dataframe for each id
  df_alts = consideration_sets %>%
    filter(observer_id == choices$observer_id & 
             locality_id != choices$locality_id) %>% # insure non-chosen
    mutate(choice = 0)
  
  # Only sample if consideration sets less than n_alts
  if(n_alts < nrow(df_alts))
    df_alts = slice_sample(df_alts, n = n_alts) 
  
  out = bind_rows(choices, df_alts) %>%
    left_join(trip_other, by = c("observer_id"))
  
  return(out)
}
#-------------------------------------------------------------------------------

# Import data

# Person-trips
df_pt <- read.csv("./data/processed/ab-ebdusers-person-trips1.csv")

# Travel costs
df_travel_costs <- read_csv("./data/processed/ab-ebd-travel-costs.csv")

# User postal codes
df_pc_sub <- read_csv("./data/processed/ab-ebdusers-pc-locations.csv")

# Euclidean distances
df_euc_dist <- read_csv("./data/processed/pc-hotspot-euclidean-distances.csv")

# All hotspot locations
df_hot_loc <- read_csv("./data/processed/ab-ebd-hotspot-locations.csv")

# number of species
df_spe <- read_csv("./data/processed/number_of_species.csv") # where is this from?

# number of birds at risk
df_risk <- read_csv("./data/processed/endangered_species.csv")

# type of parks
df_park <- read_csv("./data/processed/type_of_hotspt.csv")


#-------------------------------------------------------------------------------

# Travel costs
# Not unique so keep only distinct values
df_costs <- df_travel_costs %>% 
  distinct(locality_id, postal_code, .keep_all = T) %>%
  select(locality_id, postal_code, cost_total, hours)

# create postal code-id table
df_pc_id = df_pc_sub %>%
  select(-longitude, -latitude)

# Retrieve relevant person-trips (have postal codes, between 5 - 120 km)
df_pt_rel <- df_pt %>%
  left_join(df_pc_sub, by = "observer_id") %>%
  # Filter out trips where we don't have postal code info; might want these later though.
  filter(!is.na(postal_code)) %>%
  left_join(df_euc_dist, by = c("postal_code", "locality_id")) %>%
  filter(euc_distance_km <= travel_cutoff_hi & euc_distance_km >= travel_cutoff_lo) %>%
  select(observer_id, observation_date, locality_id, locality) # missing Kanaskis travel costs. 

# Create dataframe with chosen trips
df_trip <- df_pt_rel %>%
  mutate(month = as.character(month(observation_date, label = TRUE)),
         year = year(observation_date)) %>%
  group_by(observer_id, month, year) %>%
  mutate(choice = 1,
         choice_occasion = seq(n())) %>%
  ungroup(.) %>%
  mutate(month = factor(month, levels = month.abb),
         year = factor(year)) %>%
  # Make sure we have travel cost for trip
  left_join(df_pc_id, by = "observer_id") %>%
  left_join(df_costs, by = c("postal_code", "locality_id")) %>%
  left_join(df_spe, by = c("locality_id")) %>%
  left_join(df_risk, by = c("locality_id")) %>%
  left_join(df_park, by = c("locality_id"))%>%
  filter(!is.na(cost_total)) %>%
  filter(!is.na(num_species)) %>%
  filter(!is.na(num_species_endangered))%>%
  filter(!is.na(TYPE))%>%
  select(observer_id, locality_id, month, year, choice_occasion, choice)   

df_trip %>%
  group_by(year) %>%
  tally()

df_trip %>%
  group_by(month) %>%
  tally()

#----------------------------------------------
# Create dataframe for consideration sets
# Vector of hotspots - 1,227 total.
locality_id <- df_hot_loc %>% pull(locality_id)

# Vector of unique users - 287 total.
observer_id <- df_pt_rel %>% 
  distinct(observer_id) %>% 
  pull()

df_consideration_sets <- crossing(observer_id, locality_id) %>%
  left_join(df_pc_id, by = "observer_id") %>%
  left_join(df_euc_dist, by = c("postal_code", "locality_id")) %>%
  # Remove missing cost data
  left_join(df_costs, by = c("postal_code", "locality_id")) %>%
  filter(!is.na(cost_total))%>%
  left_join(df_spe, by = c("locality_id"))%>%
  filter(!is.na(num_species))%>%
  left_join(df_risk, by = c("locality_id"))%>%
  filter(!is.na(num_species_endangered))%>%
  left_join(df_park, by = c("locality_id"))%>%
  filter(!is.na(TYPE))

# Only keep combos we're interested 
if(consider_time == 1){ #  by time
  df_consideration_sets <- df_consideration_sets %>%
    filter(hours <= travel_hours_hi & hours >= travel_hours_lo) %>%
    select(observer_id, locality_id)
  
} else if (consider_time == 0){ # by distance
  df_consideration_sets <- df_consideration_sets %>%
    filter(euc_distance_km <= travel_cutoff_hi & euc_distance_km >= travel_cutoff_lo) %>%
    select(observer_id, locality_id)
}

#----------------------------------------------------
# Sample consideration sets
df_trip_list <- split(df_trip, seq(nrow(df_trip)))

plan(cluster, workers = n_workers)

df_modeling = future_map_dfr(df_trip_list, SampleAlternatives, 
                             consideration_sets = df_consideration_sets, 
                             n_alts = n_alts,
                             .options = furrr_options(seed = TRUE))
closeAllConnections()

#----------------------------------------------------
# Add back travel costs
df_modeling = df_modeling %>%  
  left_join(df_pc_id, by = "observer_id") %>%
  left_join(df_costs, by = c("postal_code", "locality_id")) %>%
  select(-postal_code, -hours)

# account for the inflation during the period of 2009 - 2020 while taking the
# base year as 2015

library(priceR)
df_modeling = df_modeling%>%
  mutate(adj_cost_total = adjust_for_inflation(cost_total,year, "CA", to_date = 2015))

# Add back number of species
df_modeling = df_modeling%>%
  left_join(df_spe, by = "locality_id")

# Add back number of birds at risk
df_modeling = df_modeling%>%
  left_join(df_risk, by = "locality_id")

# Add back the parks 
df_modeling = df_modeling%>%
  left_join(df_park, by = "locality_id")



# Should all be 1
df_modeling %>%
  group_by(observer_id, year, month, choice_occasion) %>%
  summarise(choice = sum(choice)) %>%
  group_by(choice) %>%
  tally()


# Create congestion variable
#df_congestion = df_modeling %>%
#  group_by(locality_id, year, month) %>%
#  summarise(total_trips = sum(choice))

#df_modeling_congest = df_modeling %>%
#  left_join(df_congestion, by = c("locality_id", "month", "year")) %>%
#  mutate(total_trips_others = total_trips - choice) %>% # exclude their own trip
#  select(observer_id, locality_id, year, month, choice_occasion, choice, cost_total, total_trips_others) 

#-------------------------------------------------------------------------------
# Prepare data for apollo

df_modeling = df_modeling %>%  
  arrange(observer_id, month, year, choice_occasion, locality_id) %>%
  group_by(observer_id, year, month, choice_occasion)  %>% 
  mutate(choice_id = cur_group_id(),
         alt = seq(n()),
         choice = choice * alt) %>%
  ungroup(.)

# Create travel cost in wide format
df_tc = df_modeling %>%
  pivot_wider(choice_id, names_from = "alt", 
              names_prefix = "tc_",
              values_from = "adj_cost_total")

# create number of species in wide format
df_sr = df_modeling%>%
  pivot_wider(choice_id, names_from = "alt",
              names_prefix = "sr_",
              values_from = "num_species")

# create number of birds at risk in wide format
df_ri = df_modeling%>%
  pivot_wider(choice_id, names_from = "alt",
              names_prefix = "es_",
              values_from = "num_species_endangered")


# create park observation in wide format
df_pr = df_modeling%>%
  pivot_wider(choice_id, names_from = "alt",
              names_prefix = "park_",
              values_from = "TYPE")

# Need availability because some people do not have n_alts alternatives in consideration sets

df_avail =  df_modeling %>%
  pivot_wider(choice_id, names_from = "alt", 
              names_prefix = "avail_",
              values_from = "cost_total") %>%
  mutate_at(vars(contains('avail_')), ~(ifelse(is.na(.), 0, 1)))

summary(df_avail)

# Decode month into quarter

df_modeling = df_modeling%>%
  mutate(season = case_when(month == "Dec" |
                            month == "Jan" |
                            month == "Feb" ~ "winter",
                           month == "Mar" |
                           month == "Apr" |
                           month == "May"~ "spring",
                           month == "Jun" |
                           month == "Jul" |
                           month == "Aug"~ "summer",
                           month == "Sep" |
                           month == "Oct" |
                           month == "Nov"~ "fall"))

# return chosen alternative
df_choice = df_modeling %>%
  filter(choice > 0) %>%
  select(observer_id, choice_id, choice,season) 

df_apollo = df_choice %>%
  left_join(df_tc, by = "choice_id") %>%
  left_join(df_sr, by = "choice_id")%>%
  left_join(df_ri, by = "choice_id")%>%
  left_join(df_pr, by = "choice_id")%>%
  left_join(df_avail, by = "choice_id")


write_csv(df_apollo, "data/processed/ApolloData_nalt200.csv")


3