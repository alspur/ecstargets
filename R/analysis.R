library(dplyr)
library(ggplot2)
library(ggmap)
library(maptools)
library(RColorBrewer)
library(scales)

# load data
ecs.data <- read.csv("data/ecs15revTarget.csv", stringsAsFactors = FALSE)

# clean names
ecs.data$town.name <- tolower(ecs.data$town.name)

ecs.data$town.name <- gsub("(^|[[:space:]])([[:alpha:]])",
                           "\\1\\U\\2", 
                           ecs.data$town.name, 
                           perl=TRUE)

# calculate summary data
ecs.summary <- ecs.data %>%
    summarise(ecs.fy15 = sum(fy15),
              ecs.target = sum(as.numeric(rev.ecs.target)))%>%
    mutate(ratio = ecs.fy15 / ecs.target)

# caculate grants based on target distribution @ current funding level
ecs.data <- ecs.data %>%
    mutate(ecs.15target = rev.ecs.target * ecs.summary$ratio,
           change.15target =  ecs.15target - fy15,
           diff.15target =  fy15 - ecs.15target,
           pct.15target = fy15/ecs.15target)

# load shapefile
towns <- readShapePoly("data/townct_37800_0000_2010_s100_census_1_shp/wgs84/townct_37800_0000_2010_s100_census_1_shp_wgs84.shp")


# convert shapefile to dataframe

town.geom <- fortify(towns)

town.geom$id <- as.integer(town.geom$id)

# load map name key

map.key <- data.frame(id = 0:172, town.name = towns@data$NAME10)

# join geom and key info

town.map <- left_join(town.geom, map.key)

town.map$town.name <- as.character(town.map$town.name)

town.data <- left_join(town.map, ecs.data, by = "town.name")


#plot ecs vs adjusted fy15 ecs
ggplot(town.data,
       aes(x=long, y = lat, group = group, fill = diff.15target))+
    geom_polygon()+
    scale_fill_gradient2("Difference",
                         low = "firebrick4",
                         high = "forestgreen", 
                         midpoint = 1,
                         na.value = "grey54",
                         labels = dollar)+
    labs(x="",y="")+
    ggtitle("Difference between FY15 ECS and Adjusted FY15 ECS")+
    theme(panel.background = element_rect(fill = 'grey54', colour = 'grey54'),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          title = element_text(color = "white"),
          legend.text = element_text(colour = "white"),
          plot.background = element_rect(fill = 'grey54', colour = 'grey54'),
          legend.title = element_text(colour = "white"),
          legend.background = element_rect(fill = "grey54",colour = "grey54"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "right")
ggsave("ecsDiff.png", width = 8, height = 5, unit = "in", dpi = 300)

#plot percetage of fy15 ecs vs adjusted fy15 ecs
ggplot(town.data,
       aes(x=long, y = lat, group = group, fill = pct.15target))+
    geom_polygon()+
    scale_fill_gradient2("FY15 ECS as\nPercentage of\nAdj. FY15 ECS",
                         low = "firebrick4",
                         high = "forestgreen", 
                         midpoint = 1,
                         na.value = "grey54",
                         labels = percent)+
    labs(x="",y="")+
    ggtitle("FY15 ECS as % of Adjusted FY15 ECS")+
    theme(panel.background = element_rect(fill = 'grey54', colour = 'grey54'),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          title = element_text(color = "white"),
          legend.text = element_text(colour = "white"),
          plot.background = element_rect(fill = 'grey54', colour = 'grey54'),
          legend.title = element_text(colour = "white"),
          legend.background = element_rect(fill = "grey54",colour = "grey54"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "right")
ggsave("ecsPctDiff.png", width = 8, height = 5, unit = "in", dpi = 300)