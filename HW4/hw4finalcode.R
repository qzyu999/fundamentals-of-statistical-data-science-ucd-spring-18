library(ggmap); library(ggplot2); library(lubridate); library(geosphere); library(gridExtra) # Libraries
library(MASS); library(xts); library(forecast); library(tseries); library(ggrepel)

hawks = read.csv("the_hawks.csv") # Load csv

### 1
get_center = function(x) {
  sapply(x[c("long", "lat")], function(x) mean(range((x))))
} # Find median of map to begin plot.

loc = get_center(hawks) # Save coordinates

hawk_map = get_map(loc, zoom = 10, scale = 1, source = "google", maptype = "terrain", col = "bw") # Create map type
hawk_plot = ggmap(hawk_map, # Plot hawks by tag
      base_layer = ggplot(hawks, aes(long, lat, color = factor(tag)))) +
  geom_point(alpha = 0.3) + labs(x = "Longitude", y = "Latitude", # Labels
                                 title = "Locations of Hawks", subtitle = "Separated by 5 Different Hawk Tags") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), # Title setting
        legend.justification = c(1, 1), legend.position = c(1, 1)) +
  scale_color_discrete("Hawk Tags") + # Legend title
  guides(colour = guide_legend(override.aes = list(alpha = 1))) # Change legend alpha levels

### 2
unique(hawks$tag[hawks$stage == "arrival"]) # 105936, 105928
arrival1 = subset(hawks, (stage == "arrival" & tag == 105928)) # Subset the two arrival hawks
arrival2 = subset(hawks, (stage == "arrival" & tag == 105936))

get_center2 = function(x) {
  sapply(x[c("long", "lat")], function(x) median((x)))
} # Find median of map to begin plot.

arrival1_loc = get_center2(arrival1) # Create maps for the two arrival hawks
arrival1_map = get_map(arrival1_loc, zoom = 15, scale = 1, source = "google", maptype = "terrain")
arrival2_loc = get_center2(arrival2)
arrival2_map = get_map(arrival2_loc, zoom = 15, scale = 1, source = "google", maptype = "terrain")

arrival1_plot = ggmap(arrival1_map, # Plot arrival1
                      base_layer = ggplot(arrival1, aes(long, lat))) +
  geom_point(aes(color = speed, size = height), alpha = 0.5) + 
  scale_colour_gradient2(high = 'orange', mid = 'green') +
  geom_segment(aes(xend = c(tail(long, n = -1), NA), yend = c(tail(lat, n = -1), NA)),
               arrow = arrow(length = unit(0.3, 'cm'))) +
  geom_label_repel(data = tail(arrival1, 1), aes(label = 'end'),
                   colour = 'red', point.padding =0.5, segment.color = 'red',
                   nudge_x = 0.002, nudge_y = 0.001) +
  geom_label_repel(data = head(arrival1, 1), aes(label = 'start'),
                   colour = 'red', point.padding = 0.5, segment.color = 'red',
                   nudge_x = 0.005, nudge_y = -0.001) +
  labs(x = "Longitude", y = "Latitude", size = 'Height', color = 'Speed', 
       title = "Hawk's Arrival Sequence", subtitle = "Hawk 105928") +
  theme(plot.title = element_text(hjust = 0.5, size = 14), 
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        legend.justification = c(1, 1), legend.position = c(1, 1)) +
  guides(size = guide_legend(order = 1))

arrival2_plot = ggmap(arrival2_map, # Plot arrival2
                      base_layer = ggplot(arrival2, aes(long, lat))) +
  geom_point(aes(color = speed, size = height), alpha = 0.5) + 
  scale_colour_gradient2(high = 'orange', mid = 'green') +
  geom_segment(aes(xend = c(tail(long, n = -1), NA), yend = c(tail(lat, n = -1), NA)),
               arrow = arrow(length = unit(0.3, 'cm'))) +
  geom_label_repel(data = tail(arrival2, 1), aes(label = 'end'),
                   colour = 'red', point.padding =0.5, segment.color = 'red',
                   nudge_x = -0.003, nudge_y = 0.001) +
  geom_label_repel(data = head(arrival2, 1), aes(label = 'start'),
                   colour = 'red', point.padding = 0.5, segment.color = 'red', 
                   nudge_x = -0.002, nudge_y = -0.002) +
  labs(x = "Longitude", y = "Latitude", size = 'Height', color = 'Speed', 
       title = "Hawk's Arrival Sequence", subtitle = "Hawk 105936") +
  theme(plot.title = element_text(hjust = 0.5, size = 14), 
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        legend.justification = c(1, 1), legend.position = c(1, 1))

grid.arrange(arrival1_plot, arrival2_plot, nrow = 1) # Combine plots using gridExtra

### 3
levels(as.factor(hawks$tag)) # Find the different hawk tags

hawk1 = subset(hawks, tag == "105923"); hawk2 = subset(hawks, tag == "105928")
hawk3 = subset(hawks, tag == "105930"); hawk4 = subset(hawks, tag == "105936")
hawk5 = subset(hawks, tag == "117527") # Subset each of the hawks

nest_kde = function(hawk) {
  dens = kde2d(hawk$long, hawk$lat, n = 100)
  long = dens$x[which.max(dens$z) %% length(dens$x)]
  lat = dens$y[floor(which.max(dens$z)/length(dens$y))]
  coords = c(long, lat)
  return(coords)
} # Determine the KDE coordinate

nest1 = nest_kde(hawk1); nest2 = nest_kde(hawk2); nest3 = nest_kde(hawk3)
nest4 = nest_kde(hawk4); nest5 = nest_kde(hawk5) # Find out where the nest likely is per hawk

flying1 = round(distGeo(nest1, hawk1[,3:4])); flying2 = round(distGeo(nest2, hawk2[,3:4]))
flying3 = round(distGeo(nest3, hawk3[,3:4])); flying4 = round(distGeo(nest4, hawk4[,3:4]))
flying5 = round(distGeo(nest5, hawk5[,3:4])) # Find out when the bird is likely flying

time_plot = function(hawk_num, nest_vector) {
  hawk_time = as.POSIXct(hawk_num$time)
  hawk = xts(nest_vector, order.by = hawk_time)
  return(hawk)
} # Time series version of distances

hwk1 = time_plot(hawk1, flying1); hwk2 = time_plot(hawk2, flying2)
hwk3 = time_plot(hawk3, flying3); hwk4 = time_plot(hawk4, flying4)
hwk5 = time_plot(hawk5, flying5) # Match the time with each hawks' flying vector

plot1 = plot(hwk1, main = "Time Series Hawk 105923"); plot2 = plot(hwk2, main = "Time Series Hawk 105928")
plot3 = plot(hwk3, main = "Time Series Hawk 105930"); plot4 = plot(hwk4, main = "Time Series Hawk 105936")
plot5 = plot(hwk5, main = "Time Series Hawk 117527"); par(mfrow=c(2,3)); plot1; plot2; plot3; plot4; plot5
dev.off() # Plot the time series for each of the hawks in a single chart

### 4
# Adjust nest data into a dataframe that fits with ggplot
nest2a = as.data.frame(t(nest2)); colnames(nest2a) = c("long", "lat")

departure2 = subset(hawks, tag == 105928) # Subset the hawk type
departure2$Distance = flying2 # Add distance column

View(departure2) # Examine the latest dates to see from which indices did the hawk begin to depart

departure2_loc = get_center2(departure2) # Obtain median coordinates
departure2_loc[2] = departure2_loc[2]-0.15 # Get adjusted coordinates
departure2 = departure2[1699:1706,] # Subset the time of departure

departure2_map = get_map(departure2_loc, zoom = 10, scale = 1, source = "google", maptype = "terrain")
departure2_plot = ggmap(departure2_map, # Plot the departure
                        base_layer = ggplot(departure2, aes(long, lat))) +
  geom_point(aes(color = speed, size = height), alpha = 0.8) + 
  scale_colour_gradient2(high = 'red', mid = 'blue') +
  geom_segment(aes(xend = c(tail(long, n = -1), NA), yend = c(tail(lat, n = -1), NA)),
               arrow = arrow(length = unit(0.3, 'cm'))) +
  geom_label_repel(data = tail(departure2, 1), aes(label = 'end'),
                   colour = 'red', point.padding =0.5, segment.color = 'red', 
                   nudge_x = 0.02, nudge_y = 0.01) +
  geom_label_repel(data = head(departure2, 1), aes(label = 'start'),
                   colour = 'red', point.padding = 0.5, segment.color = 'red',
                   nudge_x = 0.01, nudge_y = 0.01) +
  labs(x = "Longitude", y = "Latitude", size = 'Height', color = 'Speed', 
       title = "Hawk's Departure Sequence", subtitle = "Hawk 105928") +
  theme(plot.title = element_text(hjust = 0.5, size = 14), 
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.justification = c(1, 1), legend.position = c(1, 1)) +
  geom_point(x = nest2[1], y = nest2[2], color = "blue") +
  geom_label_repel(data = nest2a, aes(label = 'nest'),
                   colour = 'blue', point.padding = 0.5, segment.color = 'blue',
                   nudge_x = -0.01, nudge_y = 0.03) +
  geom_label_repel(aes(label = paste(month(departure2$time), "/", 
                                     day(departure2$time), ", ", hour(departure2$time), ":00",
                                     ", ", departure2$Distance)), nudge_x = -0.05)
