# California UFO Project

library(ggmap)
library(tidyverse)
library(stringr)
library(viridis)
library(plyr)
library(ggrepel)
library(MASS)
library(stringr)

bbox = c(-124.48,32.53,-114.13,42.01)
CA = get_stamenmap(bbox, zoom = 10, maptype = "toner")
ggmap(CA) 

bbox = c(-124.48,32.53,-114.13,42.01)
CA2 = get_stamenmap(bbox, zoom = 8, maptype = "terrain")
ggmap(CA2) 


save(CA, file = "CA.RData")
load(file = "CA.RData")

# California
# Toner
ggmap(CA) + stat_density2d(data = CA_UFO, aes(x = longitude, y = latitude,fill = ..level.., alpha = 0.6), geom = 'polygon',size = 0.01,bins = 15)+
  scale_fill_gradientn(colours=rev(rainbow(100, start=0, end=0.75)))+labs(title = "CA UFO Sightings",x = "Longitude", y = "Latitude") +
  theme(legend.title = element_text(size=10,face="bold"))+theme(plot.title = element_text(face = "bold"))+ scale_alpha(guide = 'none')
#+ guides(fill = FALSE)

# Terrain
ggmap(CA2) + stat_density2d(data = CA_UFO, aes(x = longitude, y = latitude,fill = ..level.., alpha = 0.1), geom = 'polygon',size = 0.01,bins = 15)+
  scale_fill_gradientn(colours=rev(rainbow(100, start=0, end=0.75)))+labs(title = "CA UFO Sightings",x = "Longitude", y = "Latitude") +
  theme(legend.title = element_text(size=10,face="bold"))+theme(plot.title = element_text(face = "bold"))+ scale_alpha(guide = 'none')+#+ guides(fill = FALSE)
  labs(fill='Relative Density Level')



# United States

bbox2 = c(-125.37,24.41,-66.44,49.55)
US = get_stamenmap(bbox2, zoom = 5, maptype = "toner")
US2 = get_stamenmap(bbox2, zoom = 5, maptype = "terrain")
ggmap(US) 
ggmap(US2)


# Toner
# This graph doesn't really show all the points but I suppose that's okay.
ggmap(US) + stat_density2d(data = US_UFO, aes(x = longitude, y = latitude,fill = ..level..,alpha = 0.1), geom = 'polygon',size = 0.01,bins = 15)+
  scale_fill_gradientn(colours=rev(rainbow(100, start=0, end=0.75)))+labs(title = "United States UFO Sightings",x = "Longitude", y = "Latitude") +
  theme(legend.title = element_text(size=10,face="bold"))+theme(plot.title = element_text(face = "bold"))+ scale_alpha(guide = 'none')+ guides(fill = FALSE)

# Terrain
ggmap(US2) + stat_density2d(data = US_UFO, aes(x = longitude, y = latitude,fill = ..level..,alpha = .1), geom = 'polygon',size = 0.01,bins = 5)+
  scale_fill_gradientn(colours=rev(rainbow(100, start=0, end=0.75)))+labs(title = "United States UFO Sightings",x = "Longitude", y = "Latitude") +
  theme(legend.title = element_text(size=10,face="bold"))+theme(plot.title = element_text(face = "bold"))+ scale_alpha(guide = 'none')+ guides(fill = FALSE)



# Plot for sightings and population
sighting_population = ggplot(states, aes(x = states$`2010_pop`,y = states$ufo_counts))+geom_point()+geom_text_repel(aes(label = ifelse(states$ufo_counts > 2000,state,'')),hjust = 1,vjust = -1)+
  labs(title = "UFO Sightings by State and Population", y = "Sightings", x = 'State Population (2010)') + ylim(0,8000) + xlim(0,40000000)
ggsave(filename="sighting_population.jpg", plot=sighting_population)


# Boxplot for Sightings by Day 
day_counts$new_date = str_sub(day_counts$date, start= -5)

day_boxplot = ggplot(day_counts, aes(x = day_counts$month_name,y = day_counts$count,fill = day_counts$month_name))+geom_boxplot()+scale_x_discrete(name ="Month", limits=c("Jan","Feb","Mar",'Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))+
    labs(title = "UFO Sightings by Day of the Year", y = "Sightings", x = 'Month')+ guides(fill=FALSE)+geom_text_repel(aes(label = ifelse(day_counts$count >= 300,new_date,'')),hjust = 1,vjust = 2,size = 3)

ggsave(filename="day_boxplot.jpg", plot=day_boxplot)


# Map for UFO and Movie Data
ggmap(CA2)+ stat_density2d(data = movies_and_ufo, aes(x = latitude,y = longitude,fill = factor(movie_released),alpha = .1), geom = 'polygon',size = 0.01,bins = 5)+
  scale_fill_gradientn(colours=rev(rainbow(100, start=0, end=0.75)))+labs(title = "United States UFO Sightings",x = "Longitude", y = "Latitude") +
  theme(legend.title = element_text(size=10,face="bold"))+theme(plot.title = element_text(face = "bold"))+ scale_alpha(guide = 'none')+ guides(fill = FALSE)
