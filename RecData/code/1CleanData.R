#-----------------------------------------------------------------------------
# Filename:           CleanData.R
# Created:            04/03/2017
# Description: Clean survey data and organize rec activity information
#
# NOTES:
#
#-----------------------------------------------------------------------------
rm(list=ls(all=TRUE))
ls()

#------------------------------------#
# Load Packages
#------------------------------------#
library(tidyverse)
#------------------------------------#
# Import data
#------------------------------------#
df <- read_csv("raw/Raw2012Data.csv") 
		   
df_activities <- df %>%
	select(PUMFID, Q19_A_Hmdy:Q19_A_Fam, 
		   Q21_A_Hmdy:Q21_C_Fam, Q25_A_Hmdy:Q26b_G_Fam) %>%
	select(-contains("Codes"), -contains("Food"), -Q26a) %>%
	gather(type, value, Q19_A_Hmdy:Q26b_G_Fam) %>%
	separate(type, into = c("question", "activity", "type"), sep = "_") %>%
	unite(activity, c(question, activity), remove=TRUE) %>%
	mutate(activity = case_when(
						activity == "Q19_A" ~ "hiking",
						activity == "Q19_B" ~ "cycling",
						activity == "Q19_C" ~ "camping",
						activity == "Q19_D" ~ "water_nonmotor",
						activity == "Q19_E" ~ "ski_down",
						activity == "Q19_F" ~ "ski_cross",
						activity == "Q19_G" ~ "golf",
						activity == "Q21_A" ~ "photo",
						activity == "Q21_B" ~ "birding",
						activity == "Q21_C" ~ "garden",
						activity == "Q25_A" ~ "land_motor",
						activity == "Q25_B" ~ "water_motor",
						activity == "Q26b_A" ~ "hunt_waterfowl",
						activity == "Q26b_B" ~ "hunt_birds",
						activity == "Q26b_C" ~ "hunt_small",
						activity == "Q26b_D" ~ "hunt_large",
						activity == "Q26b_E" ~ "hunt_other",
						activity == "Q26b_F" ~ "trap",
						activity == "Q26b_G" ~ "fish",
						TRUE ~ "NA"),
		   type = case_when(type == "Hmdy" ~ "day_close",
					type == "Awydy" ~ "day_far",
					type == "Trans" ~ "cost_trans",
					type == "Equip" ~ "cost_equip",
					type == "Accom" ~ "cost_accom",
					type == "Fam" ~ "n_people",
					TRUE ~ "NA")) %>%
	mutate(value = as.numeric(value)) %>% # ensure 1 person per party
	ungroup(.) %>%
	rename(id = PUMFID)

# Export
write_csv(df_activities, "clean/CleanData.csv")
