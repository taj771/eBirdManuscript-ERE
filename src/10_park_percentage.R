df <- read.csv("./type_of_hotspt.csv")

df_nat <- filter(df, TYPE == "1")
df_prov <- filter(df, TYPE == "2")
df_othr <- filter(df, TYPE == "3")
df_out <- filter(df, TYPE == "0")
155/1227*100
178/1227*100
17/1227*100
877/1227*100

library(plyr)
df_num_trips <- count(df_pt_rel, "locality_id")

df <- df %>%
  left_join(df_num_trips, by = "locality_id")
df <- df %>%
  drop_na()

sum(df$freq)

df_nat <- filter(df, TYPE == "1")
sum(df_nat$freq)
1153/32836*100
df_prov <- filter(df, TYPE == "2")
sum(df_prov$freq)
4930/32836*100
df_othr <- filter(df, TYPE == "3")
sum(df_othr$freq)
31/32836*100
df_out <- filter(df, TYPE == "0")
sum(df_out$freq)
26722/32836*100

