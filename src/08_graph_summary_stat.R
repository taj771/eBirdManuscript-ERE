################################################################################
# Title: model 8
# Date: 9/29/2021
# Description: Graphs and summary statistics
################################################################################

# check unique hotspots within relevant trips(1,227)
df_hot_loc <- read.csv("./data/processed/ab-ebd-hotspot-locations.csv")
df_pt_rel <- read.csv("data/processed/df_pt_rel.csv")

df_num_hot <- df_pt_rel%>%
  group_by(locality_id)%>%
  tally()
write.csv(df_num_hot,"./data/processed/num_hotspot.csv" )
df_num_species <- read.csv("./data/processed/n_species_loc.csv")
summary(df_num_species)
df_num_species_set <- df_num_hot%>%
  left_join(df_num_species,by =c("locality_id"))
summary(df_num_species_set)

write.csv(df_num_species_set, "data/processed/num_species.csv")

#-----------------------------------------------------------------------------
# graps 
# Number of species - Histogram
df_hot_loc <- read.csv("./data/processed/n_species_loc.csv")
library(dplyr)
summary(df_hot_loc)
#number of species monts
df_hot_loc <- read.csv("./data/processed/number_of_species.csv")
# locations in choice set
df_modeling <- read.csv("./data/processed/df_modeling.csv")
library(dplyr)
df_hot_loc_set <- df_modeling%>%
  select(locality_id)
df_hot_loc_set = df_hot_loc_set%>%
  distinct(locality_id)
df_num_species <- df_hot_loc_set%>%
  left_join(df_hot_loc, by = c("locality_id"))
max(df_num_species$num_species)
min(df_num_species$num_species)
library(ggplot2)
library(hrbrthemes)
#distribution of number of speceis withn 878 hotspot
#if want distribution of total set please change "df_num_species_set to "df_num_species
df_num_species_set %>%
  ggplot( aes(x=n_species)) +
  geom_histogram( binwidth=3, color="#e9ecef") +
  #stat_bin(bins = 100)+
  theme(plot.title = element_text(size=15)) + theme_bw() +
  labs(x="Number of species in a given month", y = "Number of hotspot-month pairs")
#number of species monts - boxplot
df_hot_loc_month <- read.csv("./data/processed/n_species_monthly.csv")
#boxplot for hotspots within the consideration set, if want for full set change
#df_num_hot to df_hot_loc_set
df_num_species_month <- df_num_hot%>%
  left_join(df_hot_loc_month,by = c("locality_id"))
df <- df_num_species_month%>%
  distinct(locality_id)
boxplot(df_num_species_month$n_species ~ month, data = df_num_species_month,
        names = c("Jan","Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
        main = "Number of bird Species",
        ylab = "Number of birds species",
        xlab = "Month")

df_Jan <- df_num_species_month%>%
  select(n_species, month)%>%
  filter(month == 1)
mean(df_Jan$n_species)

df_Feb <- df_num_species_month%>%
  select(n_species, month)%>%
  filter(month == 2)
mean(df_Feb$n_species)

df_Mar <- df_num_species_month%>%
  select(n_species, month)%>%
  filter(month == 3)
mean(df_Mar$n_species)

df_Apr <- df_num_species_month%>%
  select(n_species, month)%>%
  filter(month == 4)
mean(df_Apr$n_species)

df_May <- df_num_species_month%>%
  select(n_species, month)%>%
  filter(month == 5)
mean(df_May$n_species)

df_Jun <- df_num_species_month%>%
  select(n_species, month)%>%
  filter(month == 6)
mean(df_Jun$n_species)

df_Jul <- df_num_species_month%>%
  select(n_species, month)%>%
  filter(month == 7)
mean(df_Jul$n_species)

df_Aug <- df_num_species_month%>%
  select(n_species, month)%>%
  filter(month == 8)
mean(df_Aug$n_species)

df_Sep <- df_num_species_month%>%
  select(n_species, month)%>%
  filter(month == 9)
mean(df_Sep$n_species)

df_Oct <- df_num_species_month%>%
  select(n_species, month)%>%
  filter(month == 10)
mean(df_Oct$n_species)

df_Nov <- df_num_species_month%>%
  select(n_species, month)%>%
  filter(month == 11)
mean(df_Nov$n_species)

df_Dec <- df_num_species_month%>%
  select(n_species, month)%>%
  filter(month == 12)
mean(df_Dec$n_species)


#graph n_trips and n_species

df <- read.csv("data/processed/num_species_set.csv")
df[,c(3,4)] <- log(df[,c(3,4)])
ggplot(data = df, aes(x = n_species, y = n)) + geom_point() + scale_x_log10() + scale_y_log10()
plot(df$n_species,df$n, main = "Number of trip vs number of species",
     xlab = "Number of species",ylab = "Number of trips")
abline(lm(n~n_species,data = df),col = "red")


#------------------------------------------------------------------------------
# graph of parameter estimations with different n_alt

df_trval_plot = read_csv("./data/processed/travel_cost_plot.csv")
df_species_rich_plot = read_csv("./data/processed/species_rich_plot.csv")
df_secies_risk = read.csv("./data/processed/birds_risk_plot.csv")
df_2nd_quar_plot = read_csv("./data/processed/2nd_quar_sr_pot.csv")
df_3rd_quar_plot = read_csv("./data/processed/3rd_quar_sr_plot.csv")
df_4th_quar_plot = read_csv("./data/processed/4th_quar_sr_plot.csv")


p <- ggplot(data=df_trval_plot, aes(x=n_alt, y=para, group = 1))  +   geom_point()+geom_line()
p + labs(x = "Number of alternative sites",y = "Estimated parameters value" )

p <- ggplot(data=df_species_rich_plot, aes(x=n_alt, y=para, group = 1))  +   geom_point()+geom_line()
p + labs(x = "Number of alternative sites",y = "Estimated parameters value" )

p <- ggplot(data=df_2nd_quar_plot, aes(x=n_alt, y=para, group = 1))  +   geom_point()+geom_line()
p + labs(x = "Number of alternative sites",y = "Estimated parameters value" )

p <- ggplot(data=df_3rd_quar_plot, aes(x=n_alt, y=para, group = 1))  +   geom_point()+geom_line()
p + labs(x = "Number of alternative sites",y = "Estimated parameters value" )

p <- ggplot(data=df_4th_quar_plot, aes(x=n_alt, y=para, group = 1))  +   geom_point()+geom_line()
p + labs(x = "Number of alternative sites",y = "Estimated parameters value" )

# summary statistics - table
# travel cost
df_modeling <- read.csv("./data/processed/df_modeling.csv")

library(priceR)
df_modeling = df_modeling%>%
  mutate(adj_cost_total = adjust_for_inflation(cost_total,year, "CA", to_date = 2015))

data_trv_cost <-df_modeling%>%
  distinct(locality_id,observer_id, .keep_all = TRUE)%>%
  select(locality_id, observer_id, adj_cost_total)
mean(data_trv_cost$adj_cost_total)
sd(data_trv_cost$adj_cost_total)
max(data_trv_cost$adj_cost_total)
min(data_trv_cost$adj_cost_total)

# parks
df_park <- df_modeling%>%
  select(locality_id, nat_bin)
df_nat <- df_park%>%
  distinct(locality_id, .keep_all=TRUE) 
df_nat <- filter(df_nat, nat_bin == 1)#60 national parks
60/(60+128+680)


df_park <- df_modeling%>%
  select(locality_id, prov_bin)
df_prov <- filter(df_park, prov_bin == 1)
df_prov <- df_prov%>%
  distinct(locality_id, .keep_all=TRUE) #120 Provincial parks


df_park <- df_modeling%>%
  select(locality_id, otr_bin)
df_otr <- filter(df_park, otr_bin == 1)
df_otr <- df_otr%>%
  distinct(locality_id, .keep_all=TRUE)#8 Other park

#total of provincial and other park = 128
128/(60+128+680)


df_park <- df_modeling%>%
  select(locality_id,out_bin )
df_othr <- filter(df_park, out_bin == 1)
df_othr <- df_othr%>%
  distinct(locality_id, .keep_all=TRUE) #680 observation outside parks
680/(60+128+680)



# number of species
num_sp <- df_modeling%>%
  select(locality_id,num_species)
num_sp <- num_sp%>%
  distinct(locality_id, .keep_all=TRUE)%>%
  select(locality_id,num_species)
mean(num_sp$num_species)
sd(num_sp$num_species)
max(num_sp$num_species)
min(num_sp$num_species)
quantile(num_sp$num_species)
median(num_sp$num_species)
#library(plyr)

df <- count(num_sp$num_species)

#distribution of number of speceis withn 878 hotspot
#if want distribution of total set please change "df_num_species_set to "df_num_species
num_sp %>%
  ggplot( aes(x=num_species)) +
  geom_histogram( binwidth=3, color="#e9ecef") +
  #stat_bin(bins = 100)+
  theme(plot.title = element_text(size=15)) + theme_bw() +
  labs(x="Number of species", y = "count")


# number of species at risk
num_sp_risk <- df_modeling%>%
  select(locality_id,num_species_endangered)
num_sp_risk <- num_sp_risk%>%
  distinct(locality_id, .keep_all=TRUE)%>%
  select(locality_id,num_species_endangered)
mean(num_sp_risk$num_species_endangered)
sd(num_sp_risk$num_species_endangered)
max(num_sp_risk$num_species_endangered)
min(num_sp_risk$num_species_endangered)

