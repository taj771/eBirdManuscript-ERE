
rm(list = ls())

library(tidyverse)
df <- read_csv("./data/base/ebd_CA-AB_prv_relJan-2020_subset.csv")
df_income <- read_csv("./data/base/ab-pc-income.csv")

df_pc <- read_csv("./data/processed/ab-ebdusers-pc-locations.csv")   
df_hotspot <- read_csv("./data/processed/ab-ebd-hotspot-locations.csv")    

#df_pt2 = df %>%
#  distinct(locality, locality_id, observer_id, observation_date)


#df_pt3 = df %>%
#  distinct(observer_id, observation_date)


df_id_all = df %>%
  distinct(observer_id)

df_hotspot = df %>%
  filter(!is.na(latitude), locality_type == "H") %>%
  distinct(locality_id, latitude, longitude) 

df_id_pc = df_id_all %>%
  left_join(df_pc) %>%
  filter(!is.na(postal_code))

df_check = anti_join(df_pc, df_id_pc)

df_postal_codes = df_pc %>%
  distinct(postal_code)

df_subset = df %>%
  filter(observer_id %in% df_id_pc$observer_id)


df_pc_locate = df_id_pc %>%
  distinct(postal_code, latitude, longitude)

df_hotspot_locate = df_hotspot %>%
  distinct(locality_id, latitude, longitude)


save(df_pc_locate, df_hotspot_locate, file = "locatedata.RData" )

df_euc_dist <- read_csv("./data/processed/pc-hotspot-euclidean-distances.csv")
mean(df_euc_dist$euc_distance_km)
median(df_euc_dist$euc_distance_km)
summary(df_euc_dist$euc_distance_km)


df <- read.csv("data/processed/df_modeling.csv" )


df <- df %>%  
  arrange(observer_id, year, month, choice_occasion, locality_id) %>%
  group_by(observer_id, year, month, choice_occasion)  %>% 
  mutate(choice_id = cur_group_id(),
         alt = seq(n()),
         choice = choice * alt) %>%
  ungroup(.) 

df <- df%>%
  select(observer_id,choice_id,postal_code,locality_id)

df <- df%>%
  distinct(choice_id, .keep_all = T)

df <- df%>%
  left_join(df_euc_dist)


df1 <- df%>%
  distinct(postal_code,locality_id, .keep_all = T)

mean(df1$euc_distance_km, na.rm = T)
median(df1$euc_distance_km, na.rm = T)
summary(df1$euc_distance_km)

