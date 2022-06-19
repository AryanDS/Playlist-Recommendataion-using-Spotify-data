#Combining DC and Aryan code 

library(data.table)


data = fread("SpotifyFeatures.csv")
head(data)

summary(data)
str(data)
colnames(data)


library(cluster)