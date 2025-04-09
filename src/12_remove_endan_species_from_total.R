# number of species
df_spe <- read_csv("./data/processed/n_species_loc.csv")

# number of birds at risk
df_risk <- read_csv("./data/processed/endangered_species.csv")

df <- df_spe%>%
  left_join(df_risk, by =c("locality_id") )

df <- df %>%
  mutate(df,num_species = n_species - freq)%>%
  select(locality_id, num_species)

write_csv(df, "data/processed/number_of_species.csv")

df <- df %>%
  mutate(df, num_spe2 =num_species^2)%>%
  select(locality_id,num_spe2)

write_csv(df, "data/processed/number_of_species2.csv")