################################################################################
# Title: model 4                                                               #
# Date: 9/29/2021                                                              #
# Description: Coding bird species at risk and endangerd species               #
################################################################################
#Clear memory 
#------------------------------------------------------------------------------#
rm(list = ls())

library(tidyverse)

# Import data
# Hot locations
df_hot_loc <- read.csv("./data/processed/num_species_set.csv")
# ab_all data with scientific name
df_ebd_ab_all = read.csv("./data/base/ebd_CA-AB_prv_relJan-2020_subset.csv")

df_ebd_ab_all <- df_ebd_ab_all%>%
  select(scientific_name,locality_id)%>%
  mutate(endang_code = case_when(scientific_name == "Strix varia"~1,
                                 scientific_name == "Dendroica castanea"~2,
                                 scientific_name == "Dendroica virens"~3,
                                 scientific_name == "Certhia americana"~4,
                                 scientific_name == "Athene cunicularia"~5,
                                 scientific_name == "Cardellina canadensis"~6,
                                 scientific_name == "Dendroica tigrina"~7,
                                 scientific_name == "Calcarius ornatus"~8,
                                 scientific_name == "Buteo regalis"~9,
                                 scientific_name == "Centrocercus urophasianus"~10,
                                 scientific_name == "Histrionicus histrionicus"~11,
                                 scientific_name == "Lanius ludovicianus"~12,
                                 scientific_name == "Numenius americanus"~13,
                                 scientific_name == "Charadrius montanus"~14,
                                 scientific_name == "Glaucidium gnoma"~15,
                                 scientific_name == "Falco peregrinus"~16,
                                 scientific_name == "Charadrius melodus"~17,
                                 scientific_name == "Falco mexicanus"~18,
                                 scientific_name == "Oreoscoptes montanus"~19,
                                 scientific_name == "Asio flammeus"~20,
                                 scientific_name == "Anthus spragueii"~21,
                                 scientific_name == "Cygnus buccinator"~22,
                                 scientific_name == "Cathartes aura"~23,
                                 scientific_name == "Aechmophorus occidentalis"~24,
                                 scientific_name == "Melanitta fusca"~25,
                                 scientific_name == "Grus americana"~26,
                                 scientific_name == "Empidonax traillii"~27,
                                 TRUE ~ 0)) # hotspot without endangered species

#get the unique endangered species per hospot -may need to active if # of enddan
# is under consideration
df_ebd_ab_all <- df_ebd_ab_all%>%
  distinct(locality_id,endang_code)


#get the total number of unique species per hotspot
df_ebd_ab_all <- df_ebd_ab_all%>%
  group_by(locality_id) %>%
  summarise(num_species_endangered = sum(endang_code > 0)) # ensure that only endangered species count

write_csv(df_ebd_ab_all, "data/processed/endangered_species.csv")

#to get the number of bird species at risk within the consideration set
df <- read.csv("./data/processed/num_hotspot.csv" )%>%
  left_join(df_ebd_ab_all,by=c("locality_id"))
summary(df)


#above include species at risk / sensetive/ endangered so create a dummy only 
# with endangered species

df_ebd_ab_all = read.csv("./data/base/ebd_CA-AB_prv_relJan-2020_subset.csv")

df_ebd_ab_all <- df_ebd_ab_all%>%
  mutate(end_bin = case_when(scientific_name == "Strix varia"~0,
                             scientific_name == "Dendroica castanea"~ 0,
                             scientific_name == "Dendroica virens"~ 0,
                             scientific_name == "Certhia americana"~ 0,
                             scientific_name == "Athene cunicularia"~ 1,
                             scientific_name == "Cardellina canadensis"~ 0,
                             scientific_name == "Dendroica tigrina"~ 0,
                             scientific_name == "Calcarius ornatus"~ 0,
                             scientific_name == "Buteo regalis"~ 1,
                             scientific_name == "Centrocercus urophasianus"~ 1,
                             scientific_name == "Histrionicus histrionicus"~ 0,
                             scientific_name == "Lanius ludovicianus"~ 0,
                             scientific_name == "Numenius americanus"~ 0,
                             scientific_name == "Charadrius montanus"~1,
                             scientific_name == "Glaucidium gnoma"~0,
                             scientific_name == "Falco peregrinus"~1,
                             scientific_name == "Charadrius melodus"~1,
                             scientific_name == "Falco mexicanus"~0,
                             scientific_name == "Oreoscoptes montanus"~0,
                             scientific_name == "Asio flammeus"~0,
                             scientific_name == "Anthus spragueii"~0,
                             scientific_name == "Cygnus buccinator"~0,
                             scientific_name == "Cathartes aura"~0,
                             scientific_name == "Aechmophorus occidentalis"~1,
                             scientific_name == "Melanitta fusca"~0,
                             scientific_name == "Grus americana"~1,
                             scientific_name == "Empidonax traillii"~0))

df_ebd_ab_all <- df_ebd_ab_all%>%
  replace(is.na(.),0)

#get the unique endangered species per hospot -may need to active if # of enddan
# is under consideration
df_ebd_ab_all <- df_ebd_ab_all%>%
  distinct(locality_id,end_bin)

#write csv with endangers species bianry
write.csv(df_ebd_ab_all, "data/processed/endangered_species_binary.csv")

#to get the number of bird species at risk within the consideration set
df <- read.csv("./data/processed/num_hotspot.csv" )%>%
  left_join(df_ebd_ab_all,by=c("locality_id"))%>%
summary(df)
  


