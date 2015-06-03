library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(readr)

# prep data ####

# load data
budget_data <- read_csv("data/finalBudgetECS.csv")
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

# calculations ####

# calculate ecs summary data for gov's budget
# total funding compared to target funding
ecs_summary_gov <- budget_data %>%
  summarise(ecs_fy16 = sum(ecs_fy16),
            ecs_target = sum(as.numeric(fully_funded_target))) %>%
  mutate(ratio = ecs_fy16 / ecs_target)


# calculate ecs summary data for final budget
# total funding compared to target funding
ecs_summary_final <- budget_data %>%
  summarise(ecs_fy16 = sum(final_budget_fy16),
            ecs_fy17 = sum(final_budget_fy17),
            ecs_target = sum(as.numeric(fully_funded_target))) %>%
  mutate(ratio_fy16 = ecs_fy16 / ecs_target,
         ratio_fy17 = ecs_fy17 / ecs_target)

# calculate gov & final budget ecs grants based on adj. target funding
# adjusted target funding = total target * ecs % funded
# calculate absoloute and percentage changes
ecs_data <- budget_data %>%
  mutate(gov_adj_target = fully_funded_target * ecs_summary_gov$ratio,
         change_fy16_gov =  gov_adj_target - ecs_fy16,
         diff_fy16_gov =  ecs_fy16 - gov_adj_target,
         pct_fy16_gov = ecs_fy16 / gov_adj_target,
         final_adj_target_fy16 = fully_funded_target * ecs_summary_final$ratio_fy16,
         final_adj_target_fy17 = fully_funded_target * ecs_summary_final$ratio_fy17,
         change_fy16_final =  final_adj_target_fy16 - final_budget_fy16,
         diff_fy16_final =  final_budget_fy16 - final_adj_target_fy16,
         pct_fy16_final = final_budget_fy16 / final_adj_target_fy16,
         change_fy17_final =  final_adj_target_fy17 - final_budget_fy17,
         diff_fy17_final =  final_budget_fy17 - final_adj_target_fy17,
         pct_fy17_final = final_budget_fy17 / final_adj_target_fy17,
         final_v_gov_fy16 = final_budget_fy16 - ecs_fy16,
         final_v_gov_fy17 = final_budget_fy17 - ecs_fy16,
         final_v_approps_fy16 = final_budget_fy16 - approps_fy16,
         final_v_approps_fy17 = final_budget_fy17 - approps_fy17,
         final_fy16_v_final_fy17 = final_budget_fy17 - final_budget_fy16,
         pct_fy17_gov = ecs_fy16 / final_adj_target_fy17,
         pct_change_fy17 = pct_fy17_final - pct_fy17_gov,
         funded_pct = ecs_fy16 / fully_funded_target,
         funded_pct_fy17 = final_budget_fy17 / fully_funded_target,
         funded_pct_change_fy17 = funded_pct_fy17 - funded_pct)

# join ecs data and alliance list
ecs_data <- ecs_data %>%
  left_join(alliance_list)

# join ecs data and resident student data by town id
ecs_data <- ecs_data %>%
  left_join(resident_data %>% select(-town_name), by = "town_id") %>%
  mutate(frpl_pct = resFRPL / resTotal)

# plot change in funding vs funded pct ####

# filter for towns that are less than 106% funded
# no town over 106% of their ECS target got an increase
ggplot(ecs_data %>% filter(funded_pct < 1.06), aes(x = funded_pct, funded_pct_change_fy17, label = town_name,
                     size = resTotal, color = alliance_status))+
  geom_text() + 
  scale_color_manual(values = c("indianred", "lightsteelblue4"),
                     name = "Alliance Status") +
  scale_size(range = c(2, 6), guide=FALSE) + 
  scale_x_continuous(label = percent)+
  scale_y_continuous(label = percent)+
  labs(x = "Current ECS Grant as % of ECS Grant Target", y = "% increase ECS / ECS Target by FY17") +
  theme(legend.position = c(.8, .8))
ggsave("figures/finalBudgetPctChange.png", width = 8, height = 5, unit = "in", dpi = 300)

ggplot(ecs_data, aes(x = fully_funded_target - ecs_fy16, final_v_gov_fy17, label = town_name,
                                                   size = resTotal, color = alliance_status))+
  geom_text() + 
  scale_color_manual(values = c("indianred", "lightsteelblue4"),
                     name = "Alliance Status") +
  scale_size(range = c(2, 6), guide=FALSE) + 
  scale_x_continuous(label = dollar)+
  scale_y_continuous(label = dollar)+
  labs(x = "Gap between current ECS grant and ECS Grant Target", y = "Increase in ECS Grant by FY17") +
  theme(legend.position = c(.2, .8))
ggsave("figures/finalBudgetDollarChange.png", width = 8, height = 5, unit = "in", dpi = 300)

# plot histogram of pct funded by fy17

ggplot(ecs_data, aes(x = funded_pct_fy17))+
  geom_histogram(binwidth = .1, fill = "indianred")+
  labs(x = "% of Target ECS Grant by FY17", y = "Number of Towns")+
  scale_x_continuous(label = percent)
ggsave("figures/finalBudgetFY17fundedPct.png", width = 8, height = 5, unit = "in", dpi = 300)


# previous chart w/ log10 x axis
ggplot(ecs_data, aes(x = funded_pct_fy17))+
  geom_histogram(binwidth = .1, fill = "indianred")+
  labs(x = "% of Target ECS Grant by FY17", y = "Number of Towns")+
  scale_x_log10(label = percent)
ggsave("figures/finalBudgetFY17fundedPctLog.png", width = 8, height = 5, unit = "in", dpi = 300)
