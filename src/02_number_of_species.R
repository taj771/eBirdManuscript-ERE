################################################################################
# Title: Model 2                                                               #
# Date: 09/29/2021                                                             #
# Description: Get the number of unique species of a hot spot and number of    #
# species by month                                                             #
################################################################################
# Load packages
library(tidyverse)
library(janitor)
library(lubridate)
#------------------------------------------------------------------------------#
# Import data                                                                  #
df_ebd_ab_all = read_csv("./data/base/ebd_CA-AB_prv_relJan-2020_subset.csv")   #
#------------------------------------------------------------------------------#
# Tidying                                                                      #
# Subset observations:                                                         #
#------------------------------------------------------------------------------#
df_ebd_ab_sub <- df_ebd_ab_all %>%
  # Clean up column names
  clean_names(case = "snake") %>%
  # Remove observations pre-2009
  mutate(observation_date = ymd(observation_date)) %>%
  filter(observation_date >= "2009-01-01") %>%
  select(common_name, scientific_name, county, iba_code, bcr_code,
         locality:observation_date, observer_id, sampling_event_identifier,
         group_identifier, trip_comments)

#------------------------------------------------------------------------------#
# Retrieve number of checklists and unique species for each locality           #
#------------------------------------------------------------------------------#
df_loc_check <- df_ebd_ab_sub %>%
  mutate(month = month(observation_date)) %>%
  filter(locality_type == "H" | locality_type == "P") %>%
  group_by(locality, locality_id, month) %>%
  summarise(n_checklist = n_distinct(sampling_event_identifier),
            n_species = n_distinct(scientific_name))
#------------------------------------------------------------------------------#
# remove locality name                                                         #
#------------------------------------------------------------------------------#
df_loc_check = subset(df_loc_check,select = -c(locality, n_checklist))
df_loc_check <- df_loc_check%>%
  group_by(locality_id)
write_csv(df_loc_check, "data/processed/n_species_monthly.csv")
#------------------------------------------------------------------------------#
#number of species per hot spot                                                #
#------------------------------------------------------------------------------#
df_unique_species_loc = subset(df_ebd_ab_sub, select = c(scientific_name, locality_id, sampling_event_identifier))%>%
  group_by(locality_id)%>%
  summarise(n_checklist = n_distinct(sampling_event_identifier),
            n_species = n_distinct(scientific_name))
df_unique_species_loc = subset(df_unique_species_loc, select = -c(n_checklist))
write_csv(df_unique_species_loc, "data/processed/n_species_loc.csv")

  

