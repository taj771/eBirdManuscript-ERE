#-----------------------------------------------------------------------------
# Filename:           CreateSocio.R
# Created:            14/04/2018
#
# NOTES: Create respondent charachteristic information used for imputation.
#
#-----------------------------------------------------------------------------
rm(list=ls(all=TRUE))
ls()

#------------------------------------#
# Load Packages
#------------------------------------#
library(tidyverse)
library(fastDummies)
library(mice)

#------------------------------------#
# Import data
#------------------------------------#
df <- read_csv("raw/Raw2012Data.csv") 

table(df$Q30_Notrips)
summary(df$Q30_Overnt)
table(df$Q30_Overnt_Codes)
table(df$Q30_Sameday)
table(df$Q30_Sameday_Codes)
table(df$Q33)

summary(df$Q33)
max_donations <- 12000
socio <- df  %>%
	transmute(province = Wt_Province,
			  id = PUMFID,
			  indig = as.numeric(case_when(Q51 == 1 ~ "1", 
			  				  Q51 == 2 ~ "0", 
			  				  TRUE ~ "NA")),
		   imigrant = as.numeric(case_when(Q52 == 1 ~ "1", 
		   					 Q52 == 2 ~ "0", 
		   					 TRUE ~ "NA")),
			  income = if_else(Q54 == 1, 25000, 
					if_else(Q54 == 2, (49999-25000)/2+25000, 
					if_else(Q54 == 3, (74999-50000)/2+50000, 
					if_else(Q54 == 4, (99999-75000)/2+75000, 
					if_else(Q54 == 5, 150000, NULL))))),
		   participate_hunt = as.numeric(case_when(Q26a == 1 ~ "1", 
		   							 Q26a == 2 ~ "0", 
		   							 TRUE ~ "NA")),
			  age = as.numeric(ifelse(AgeClass < 77, AgeClass, "NA")),
			  male = as.numeric(case_when(Q46 == 1 ~ "1", 
			  							Q46 == 2 ~ "0", 
			  							TRUE ~ "NA")),
		urban = ifelse(Census_Urban == 1, 1, 0),
			  education = as.numeric(ifelse(Q53 < 77, Q53, "NA")),
		web_sample = ifelse(Sam_From == "Web", 1, 0),
		cabin = as.numeric(case_when(Q33 == 1 ~ "1", 
					   Q33 == 2 ~ "0", 
					   TRUE ~ "NA")),
		num_overnt_trips = Q30_Overnt,
		donations = ifelse(Q35 < max_donations, Q35, max_donations),
		weight_address = Address_Analysis_Weight / mean(Address_Analysis_Weight, na.rm = TRUE),
		weight = Combined_Analysis_Weight/ mean(Combined_Analysis_Weight))

summary(socio)

write_csv(socio, "clean/Socio.csv")
