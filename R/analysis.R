library(dplyr)
library(ggplot2)
library(ggmap)
library(maptools)
library(RColorBrewer)
library(scales)
library(readr)

# load data
ctvoices_data <- read_csv("data/ctvoicesECS.csv")
alliance_list <- read_csv("data/allianceDistricts.csv")
resident_data <- read_csv("data/resStudents.csv")

# clean alliance town names

colnames(alliance_list) <- c("town_id", "town_name", "alliance_status")

alliance_list$town_name <- tolower(alliance_list$town_name)

alliance_list$town_name <- gsub("(^|[[:space:]])([[:alpha:]])",
                           "\\1\\U\\2", 
                           alliance_list$town_name, 
                           perl=TRUE)

# clean resident data  names

colnames(resident_data)[1:2] <- c("town_id", "town_name")

resident_data$town_name <- tolower(resident_data$town_name)

resident_data$town_name <- gsub("(^|[[:space:]])([[:alpha:]])",
                                "\\1\\U\\2", 
                                resident_data$town_name, 
                                perl=TRUE)

# calculate summary data
ecs_summary <- ctvoices_data %>%
    summarise(ecs_fy16 = sum(ecs_fy16),
              ecs_target = sum(as.numeric(fully_funded_target))) %>%
    mutate(ratio = ecs_fy16 / ecs_target)

# caculate grants based on target distribution @ current funding level
ecs_data <- ctvoices_data %>%
    mutate(ecs_adj_target = fully_funded_target * ecs_summary$ratio,
           change_fy16_target =  ecs_adj_target - ecs_fy16,
           diff_fy16_target =  ecs_fy16 - ecs_adj_target,
           pct_fy16_target = ecs_fy16 / ecs_adj_target)

# join ecs data and alliance list

ecs_data <- ecs_data %>%
    left_join(alliance_list)

# join ecs data and resident student data

ecs_data <- ecs_data %>%
    left_join(resident_data)

# load shapefile
towns <- readShapePoly("data/townct_37800_0000_2010_s100_census_1_shp/wgs84/townct_37800_0000_2010_s100_census_1_shp_wgs84.shp")


# convert shapefile to dataframe

town_geom <- fortify(towns)

town_geom$id <- as.integer(town_geom$id)

# load map name key

map_key <- data.frame(id = 0:172, town_name = towns@data$NAME10)

# join geom and key info

town_map <- left_join(town_geom, map_key)

town_map$town_name <- as.character(town_map$town_name)

town_data <- left_join(town_map, ecs_data, by = "town_name")


#plot ecs vs adjusted fy16 ecs
ggplot(town_data,
       aes(x=long, y = lat, group = group, fill = diff_fy16_target))+
    geom_polygon()+
    scale_fill_gradient2("Difference",
                         low = "firebrick4",
                         high = "forestgreen", 
                         midpoint = 1,
                         na.value = "grey54",
                         labels = dollar)+
    labs(x="",y="")+
    ggtitle("Difference between FY16 ECS and Adjusted FY16 ECS")+
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
ggsave("figures/ecsDiff.png", width = 8, height = 5, unit = "in", dpi = 300)

#plot percetage of fy16 ecs vs adjusted fy16 ecs
ggplot(town_data,
       aes(x=long, y = lat, group = group, fill = pct_fy16_target))+
    geom_polygon()+
    scale_fill_gradientn(colours = c("firebrick4","firebrick2", "white", "palegreen3", "darkgreen"),
                         values = c(0.000, 0.060, 0.095, 0.105, 0.2, 1.000),
                         breaks = c(0, 1, 4, 8),
                         limits = c(0,10),
                         na.value = "grey54")+
    labs(x="",y="")+
    ggtitle("FY16 ECS as % of Adjusted FY16 ECS")+
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
ggsave("figures/ecsPctDiff.png", width = 8, height = 5, unit = "in", dpi = 300)
