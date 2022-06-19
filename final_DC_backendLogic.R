library(data.table)
library(dplyr)
library(lsa)

data = read.csv('SpotifyFeatures.csv')
summary(data)
str(data)


colnames(data)
input = c(10, 0.6, 0.44, 0.002)

# Inputs
no_of_playlist = 4
songs_per_playlist = 10
#input = c(0,10,11,12)

# Calculate cosine similarity
final_features = c('popularity', 'danceability', 'acousticness', 'energy' )
new_data = data[,final_features] # Getting our target similarity columns

summary(data)

summary(new_data)

str(input)

table_features = c('popularity', 'danceability', 'acousticness', 'energy' , 'artist_name', "track_name" )
new_table_data =  data[,table_features]


summary(new_table_data)

song_name = c('artist_name', "track_name" )

data_names = data[, song_name]

head(data_names)
summary(data_names)




similarity = function(x, output) {
  return (cosine(x, input))
}
# Applying function and appending new column in our dataset
ss <- apply(new_data, 1, similarity)
new_data <- cbind(new_data, similar = ss)
summary(new_data)

#merging the new_data and data_names

data_songs_merge = cbind(data_names, new_data)
head(data_songs_merge)
head(new_data)

#Trying with songs names 
# Getting subset and sorting it based on similarity
ordered_ <- data_songs_merge[order(-data_songs_merge$similar),]
total = 40 
subset_songs = ordered_[1:total,]

head(subset_songs)
summary(subset_songs)

#creating another data frame object with the song, artist names
artist_song_name = subset_songs[,song_name]

head(artist_song_name)


sub_subset_songs = subset_songs[,final_features]
head(sub_subset_songs)




#subset_songs <- subset_songs[,final_features]
# Performing clustering and assigning cluster number to our data
km.res_songs <- kmeans(sub_subset_songs, no_of_playlist)
sub_subset_songs <- cbind(sub_subset_songs, cluster = as.vector(km.res_songs$cluster),artist_song_name)
head(sub_subset_songs) 



#taking a subset of the dataframe sub_subset_songs for clustering plot
colnames(sub_subset_songs)
sub_subset_songs_plot <- subset(sub_subset_songs, select = c("popularity","danceability","acousticness","energy", "cluster" ))

head(sub_subset_songs_plot)

#plotting the clusters 
plot_songs =  fviz_cluster(km.res_songs  , data = sub_subset_songs_plot, geom=c("point"), ellipse.type="euclid")
plot_songs












#Above logic has a one object with the names of the songs and one object is with the artist song names for plotting.


# Getting subset and sorting it based on similarity
ordered <- new_data[order(-new_data$similar),]
total = 40 
subset = ordered[1:total,]

subset <- subset[,final_features]
head(subset)

summary(subset)



# Performing clustering and assigning cluster number to our data
km.res <- kmeans(subset, no_of_playlist)
subset <- cbind(subset, cluster = as.vector(km.res$cluster))
head(subset) 


# Splitting the dataset based on cluster number
X <- split(subset, subset$cluster)
X[1]
X[2]
X[3]
X[4]



head(subset)
library(ggplot2)

library(tidyverse)
library(readxl)
library(FactoMineR)
library(factoextra)

plot_1 =  fviz_cluster(km.res  , data = subset, geom=c("point"), ellipse.type="euclid")
plot_1

library(shiny)


ui <- fluidPage(
  plotOutput("plot_1", click = "plot_click"),
  verbatimTextOutput("info")
)

server <- function(input, output) {
  output$plot_1 <- renderPlot({
    plot(plot_1)
  })
  
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })
}

shinyApp(ui, server)

