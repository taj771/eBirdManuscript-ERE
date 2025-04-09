################################################################################
# Title: Trips that are below 20km                                             #
# Date: 10/14/2021                                                             #
# Description:Assess the sample bias with recData                              #
################################################################################

################################################################################
#Assessing number of day trips (<20km) during 2011 only - whole data set       #
################################################################################
rm(list=ls(all=TRUE))

# Load packages
library(tidyverse)
library(lubridate)

#-------------------------------------------------------------------------------

# Import data

# Person-trips
df_pt <- read_csv("./data/processed/ab-ebdusers-person-trips.csv")

# Travel costs
df_travel_costs <- read_csv("./data/processed/ab-ebd-travel-costs.csv")

# User postal codes
df_pc_sub <- read_csv("./data/processed/ab-ebdusers-pc-locations.csv")

# Euclidean distances
df_euc_dist <- read_csv("./data/processed/pc-hotspot-euclidean-distances.csv")

# All hotspot locations
df_hot_loc <- read_csv("./data/processed/ab-ebd-hotspot-locations.csv")

#-------------------------------------------------------------------------------
#set 20km as high because we are interested in trips within 20km to comply with
#recdata 
# maximum euclidian distance is 1206 km
travel_cutoff_lo = 20
travel_cutoff_hi = 1206

max(df_euc_dist$euc_distance_km)

# Retrieve relevant person-trips (have postal codes, > 20km)

df_pt_rel <- df_pt %>%
  left_join(df_pc_sub, by = "observer_id") %>%
  # Filter out trips where we don't have postal code info; might want these later though.
  filter(!is.na(postal_code)) %>%
  left_join(df_euc_dist, by = c("postal_code", "locality_id")) %>%
  filter(euc_distance_km <= travel_cutoff_hi & euc_distance_km >= travel_cutoff_lo) %>%
  select(observer_id, observation_date, locality_id, locality) # missing Kanaskis travel costs. 

# Calculate trip counts by month-year 

df_trip_counts <- df_pt_rel %>%
  mutate(month = as.character(month(observation_date, label = TRUE)),
         year = year(observation_date)) %>%
  group_by(observer_id, locality_id, month, year) %>%
  tally()
# calculate number of trips
df_modeling <- df_trip_counts%>%
  filter(n > 0)%>%
  group_by(observer_id)%>%
  summarise(count = sum(n))

# since it account for the trips during 2009 to 2020 (jan)- 121 month
# we have to scale it to 12 month as RecCanada data account only for one year 2011

df_modeling <- df_modeling%>%
  mutate(ave_trip = count/121*12)

# make sure that only consider eBird members within the our consideration set
obs_id <- read.csv("./data/processed/df_modeling.csv")
obs_id <- obs_id%>%
  select(observer_id)%>%
  distinct(observer_id)

df_indi_conside <- obs_id%>%
  left_join(df_modeling, by = "observer_id")%>%
  drop_na()

mean(df_indi_conside$count)
max(df_indi_conside$count)
mean(df_indi_conside$ave_trip)
# histogram number of trips per year above 20km - eBird members
df_modeling %>%
  ggplot( aes(x=ave_trip)) +
  geom_histogram( binwidth=7, color="#e9ecef") +
  #stat_bin(bins = 100)+
  theme(plot.title = element_text(size=15)) + theme_bw() +
  labs(x="Number of Annual Trips", y = "Number of eBird members")

###############################################################################
#RecData - National level data                                                #
###############################################################################
df_recBird <- read_csv("data/clean/CleanData.csv") 
library(tidyverse)
df_recBird <- df_recBird%>%
  filter(activity == "birding")
df_activities_day <- df_recBird %>%
  filter(type == "day_close")%>%
  filter(value > 0)
df_activities_day <- df_activities_day%>%
  drop_na()

check <- df_activities_day%>%
  filter(value == 365)

# average trips per person from recdata data  = 103.80
mean(df_activities_day$value)

# histogram number of oneday trips  - Recreational data
df_activities_day %>%
  ggplot( aes(x=value)) +
  geom_histogram( binwidth=12, color="#e9ecef") +
  #stat_bin(bins = 100)+
  theme(plot.title = element_text(size=15)) + theme_bw() +
  labs(x="Number of Annual Trips", y = "Number of individual- Nature Survey")

#-------------------------------------------------------------------------------
p <- total %>%
  ggplot( aes(x=count, fill=cat)) +
  geom_histogram( binwidth = 3,color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(x="Number of Annual Trips", y = "Number of birders")

plot(p)


###############################################################################
# only take eBird memers detail during 2011

df_trip_count_2011 <- df_trip_counts%>%
  subset(year == "2011")

df_trip_count_2011 <- df_trip_count_2011%>%
  filter(n > 0)%>%
  group_by(observer_id)%>%
  summarise(count = sum(n))


df_indi_2011 <- obs_id%>%
  left_join(df_trip_count_2011, by = "observer_id")%>%
  drop_na()

# histogram number of trips per year above 20km - eBird members
df_indi_2011 %>%
  ggplot( aes(x=count)) +
  geom_histogram( binwidth=7, color="#e9ecef") +
  #stat_bin(bins = 100)+
  theme(plot.title = element_text(size=15)) + theme_bw() +
  labs(x="Number of Annual Trips", y = "Number of eBird members")

mean(df_indi_2011$count)

#-------------------------------------------------------------------------------
# addition graph option - ignore following keep as reference
# ------------------------------------------------------------------------------
# boxplot of recData and eBird data - number of trips
# recreational day
df_Rec_box <- df_activities_day%>%
  mutate(df_activities_day, cat = "rec")
df_Rec_box <- df_Rec_box%>%
  select(cat,value)
colnames(df_Rec_box)[2] <- "count"

write.csv(df_Rec_box,"./data/processed/recdata_plot.csv")

df_eBird <- df_indi_conside%>%
  mutate(df_indi_conside, cat = "eBird")
df_eBird <- df_eBird%>%
  select(cat, ave_trip)
colnames(df_eBird)[2] <- "count"

total <- rbind(df_Rec_box, df_eBird)

# Boxplot with eBird members and Nature survey
library(ggplot2)
ggplot(total) + geom_boxplot(aes(x=cat, y=count, color=cat))+ theme_bw()
# Plot trips by group and color by group


library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)


# second option of the graphical representation


# library
library(ggplot2)
library(ggExtra)
library(hrbrthemes)

# load the data set
full <- read.csv("./data/processed/full_rec_ebird.csv")

# plot with Can rec data
p <- ggplot(full, aes(x=ID_rec, y=count)) + 
  geom_point(size=0.8, color = "darkturquoise")+
  xlab("Individual of Nature survey")+
  ylab("Number of trips per year") + 
  theme_bw()
# marginal plot with Can rec data
p1 <- ggMarginal(p, type="boxplot", margins = c("y"))
plot(p1)

# plot with eBird (who made >20km within the consideration set)
p <- ggplot(full, aes(x=ID_ebird, y=count_1)) + 
  geom_point(size=0.8, color = "deeppink4")+
  xlab("Individual eBird member")+
  ylab("Number of trips per year")+
  theme_bw()

p2 <- ggMarginal(p, type="boxplot", margins = c("y")) 

plot(p2)


# third option
p2 <- ggplot(data=total, aes(x=count, group=cat, fill=cat)) +
  geom_density(adjust=1.5, alpha=.5) +
  theme_ipsum()
  

plot(p2)
#p2


hist(df_modeling$per, breaks = 100)
hist(df_activities_day$per, breaks = 100)



ggplot(df_activities_day,aes(value))+geom_histogram(binwidth = 0.1)+stat_bin(bins = 50)+ggtitle("Number of trips - Rec data")+labs(x="Number of Trips", y = "count")
ggplot(df_modeling,aes(count))+geom_histogram(binwidth = 0.1)+stat_bin(bins = 50)+ggtitle("Number of trips - eBird data")+labs(x="Number of Trips", y = "count")
ggplot(total,aes(count, fill = value))+geom_histogram(binwidth = 5)



hist(total$value, main = "Total number of Trips",breaks = 25,
     col = rgb(1, 0, 0, 0.5),
     xlab = "Number of Trips",
     xlim = c(1,400),
     ylim = c(0,1500))
hist(total$count,breaks = 25,
     col = rgb(0, 0, 1, 0.5),
     add = TRUE)
labels <- c("recdata", "eBird")
legend("topright", legend = labels,
       cex = 0.8,
       inset = 0.01,
       pch = 15,
       col = c(rgb(1, 0, 0, 0.5),
               rgb(0, 0, 1, 0.5)))


#normality test
# t test
x = df_modeling$count
y = df_activities_day$value

t.test(x, y, alternative = "two.sided", var.equal = FALSE)



#F test for compare variance
res.ftest <- var.test(count ~ cat, data = total)
res.ftest




