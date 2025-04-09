library(plyr)
y=count(df_consideration_sets,"observer_id")
mean(y[["freq"]])
library(dplyr)
obsr283574 = filter(df_modeling,observer_id == "obsr283574")
obsr283574_1 = filter(obsr283574, choice == 1)
2269/19440*100
obsr283574_0 = filter(obsr283574, choice == 0)
16200/19449*100

identify

obsr433096 = filter(df_modeling,observer_id == "obsr433096")
obsr433096_1 = filter(obsr433096, choice == 1)
70/450*100
obsr433096_0 = filter(obsr433096, choice == 0)
375/450*100
library(plyr)
x=count(df_modeling,"observer_id")
mean(x[["freq"]])
quantile(x$freq)

Z=count(database,"observer_id")
mean(Z[["freq"]])
M=count(Z,"freq")

boxplot(Z$freq ~ observer_id, data = Z,
        main = "Frequency of appearacne",
        ylab = "freq",
        xlab = "observer id")


boxplot(x$freq ~ observer_id, data = x,
        main = "Frequency of appearacne",
        ylab = "freq",
        xlab = "observer id")

