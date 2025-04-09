df_recBird <- read_csv("clean/CleanData.csv") 
library(tidyverse)
df_recBird <- df_recBird%>%
  filter(activity == "birding")
df_activities_day <- df_recBird %>%
  filter(type == "day_far")%>%
  drop_na()
  summary(df_activities_day)