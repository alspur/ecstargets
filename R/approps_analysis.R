library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(readr)

# prep data ####

# load data
approps_data <- read_csv("data/appropsECS.csv")
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
ecs_summary_gov <- approps_data %>%
  summarise(ecs_fy16 = sum(ecs_fy16),
            ecs_target = sum(as.numeric(fully_funded_target))) %>%
  mutate(ratio = ecs_fy16 / ecs_target)

# calculate ecs summary data for approps budget
# total funding compared to target funding
ecs_summary_approps <- approps_data %>%
  summarise(ecs_fy16 = sum(approps_fy16),
            ecs_fy17 = sum(approps_fy17),
            ecs_target = sum(as.numeric(fully_funded_target))) %>%
  mutate(ratio_fy16 = ecs_fy16 / ecs_target,
         ratio_fy17 = ecs_fy17 / ecs_target)

# calculate gov & approps ecs grants based on adj. target funding
# adjusted target funding = total target * ecs % funded
# calculate absoloute and percentage changes
ecs_data <- approps_data %>%
  mutate(gov_adj_target = fully_funded_target * ecs_summary_gov$ratio,
         change_fy16_gov =  gov_adj_target - ecs_fy16,
         diff_fy16_gov =  ecs_fy16 - gov_adj_target,
         pct_fy16_gov = ecs_fy16 / gov_adj_target,
         approps_adj_target_fy16 = fully_funded_target * ecs_summary_approps$ratio_fy16,
         approps_adj_target_fy17 = fully_funded_target * ecs_summary_approps$ratio_fy17,
         change_fy16_approps =  approps_adj_target_fy16 - approps_fy16,
         diff_fy16_approps =  approps_fy16 - approps_adj_target_fy16,
         pct_fy16_approps = approps_fy16 / approps_adj_target_fy16,
         change_fy17_approps =  approps_adj_target_fy17 - approps_fy17,
         diff_fy17_approps =  approps_fy17 - approps_adj_target_fy17,
         pct_fy17_approps = approps_fy17 / approps_adj_target_fy17)

# join ecs data and alliance list
ecs_data <- ecs_data %>%
  left_join(alliance_list)

# join ecs data and resident student data by town id
ecs_data <- ecs_data %>%
  left_join(resident_data %>% select(-town_name), by = "town_id") %>%
  mutate(frpl_pct = resFRPL / resTotal)

# plot current ecs proposal data ####
ggplot(ecs_data, aes(x = frpl_pct, pct_fy16_gov, label = town_name,
                     size = resTotal, color = alliance_status))+
  geom_text() + 
  scale_color_manual(values = c("indianred", "lightsteelblue4"),
                     name = "Alliance Status") +
  geom_hline(yintercept = 1) +
  scale_size(range = c(2, 6), guide=FALSE) + 
  scale_x_continuous(label = percent)+
  scale_y_continuous(label = percent)+
  labs(x = "Resident Students, % FRPL", y = "% of Adj. FY16 ECS Grant") +
  theme(legend.position = c(.8, .8))
ggsave("figures/ecsFRPL.png", width = 8, height = 5, unit = "in", dpi = 300)

ggplot(ecs_data, aes(x = frpl_pct, pct_fy16_gov, label = town_name,
                     size = resTotal, color = alliance_status))+
  geom_hline(yintercept = 1) +
  geom_text() + 
  scale_color_manual(values = c("indianred", "lightsteelblue4"),
                     name = "Alliance Status") +
  scale_size(range = c(2, 6), guide=FALSE) + 
  scale_y_log10(label = percent)+
  scale_x_continuous(label = percent)+
  labs(x = "Resident Students, % FRPL", y = "% of Adj. FY16 ECS Grant") +
  theme(legend.position = c(.8, .8))
ggsave("figures/ecsFRPLlog.png", width = 8, height = 5, unit = "in", dpi = 300)

ggplot(ecs_data, aes(x = frpl_pct, pct_fy17_approps, label = town_name,
                     size = resTotal, color = alliance_status))+
  geom_hline(yintercept = 1) +
  geom_text() + 
  scale_color_manual(values = c("indianred", "lightsteelblue4"),
                     name = "Alliance Status") +
  scale_size(range = c(2, 6), guide=FALSE) + 
  scale_y_log10(label = percent)+
  scale_x_continuous(label = percent)+
  labs(x = "Resident Students, % FRPL", y = "% of Adj. FY17 ECS Grant") +
  theme(legend.position = c(.8, .8))
ggsave("figures/appropsFRPL.png", width = 8, height = 5, unit = "in", dpi = 300)

# ecs target vs. fy16
ggplot(ecs_data, aes(x = frpl_pct, diff_fy16_gov, label = town_name,
                     size = resTotal, color = alliance_status))+
  geom_hline(yintercept = 0) +
  geom_text() + 
  scale_color_manual(values = c("indianred", "lightsteelblue4"),
                     name = "Alliance Status") +
  scale_size(range = c(2, 6), guide=FALSE) + 
  scale_x_continuous(label = percent)+
  scale_y_continuous(label = dollar)+
  labs(x = "Resident Students, % FRPL", y = "Difference between FY16 ECS and Adj. FY16 Target Amount") +
  theme(legend.position = c(.2, .8))
ggsave("figures/ecsFRPLdollar.png", width = 8, height = 5, unit = "in", dpi = 300)

ggplot(ecs_data, aes(x = frpl_pct, diff_fy16_approps, label = town_name,
                     size = resTotal, color = alliance_status))+
  geom_text() + 
  scale_color_manual(values = c("indianred", "lightsteelblue4"),
                     name = "Alliance Status") +
  scale_size(range = c(3, 8), guide=FALSE) + 
  scale_x_continuous(label = percent)+
  scale_y_continuous(label = dollar)+
  labs(x = "% Resident FRPL Students", y = "Difference between FY16 ECS and Adj. FY16 Target Amount") +
  theme(legend.position = c(.2, .8))

# look at low-poverty towns ####

# filter for overfunded towns w/ <25% FRPL students
# arrange by most overfunded
# set proposed FY16 = adj. ECS target, eliminating overfunding
overfunded_lowfrpl_towns <- ecs_data %>%
  filter(frpl_pct < .25, diff_fy16_gov >0) %>%
  arrange(desc(diff_fy16_gov)) %>%
  mutate(proposed_fy16 = gov_adj_target)

# calculate the total amount of overfunding & average FRPL rate
overfunded_summary <- overfunded_lowfrpl_towns %>%
  summarise(count = n(),
            total = sum(diff_fy16_gov),
            frpl_pct = sum(resFRPL) / sum(resTotal))

# look at under-funded towns ####

# filter for towns w/ less than 75% of their adj. ECS grant
# calculate "gap_amount" to equal the difference between their 
# current fy16 ECS grant and 75% of their adj. ECS grant
# set propsed fy16 = 75% of adj. ECS grant
underfunded_towns <- ecs_data %>%
  filter(pct_fy16_gov < .75) %>%
  mutate(ratio_factor = .75 - pct_fy16_gov,
         gap_amount = ratio_factor * gov_adj_target,
         proposed_fy16 = gov_adj_target * .75)

# unaffected towns ####

# filter for towns that don't qualify as an overfunded or
# underfunded town above. no change to their fy16 ECS
unaffected_towns <- ecs_data %>%
  anti_join(overfunded_lowfrpl_towns, by = "town_name") %>%
  anti_join(underfunded_towns, by = "town_name") %>%
  mutate(proposed_fy16 = ecs_fy16)

# create proposed fix ####

# join three town df's: overfunded, underfunded, and unaffected
# calculate funding ratios and $ gaps of proposed fix
proposed_fix <- unaffected_towns %>%
  rbind(underfunded_towns %>% select(-ratio_factor, -gap_amount)) %>%
  rbind(overfunded_lowfrpl_towns) %>%
  mutate(proposed_ratio = proposed_fy16 / gov_adj_target,
         proposed_gap = proposed_fy16 - gov_adj_target)

# plot fix ####
ggplot(proposed_fix, aes(x = frpl_pct, proposed_ratio, label = town_name,
                     size = resTotal, color = alliance_status))+
  geom_hline(yintercept = 1) +
  geom_text() + 
  scale_color_manual(values = c("indianred", "lightsteelblue4"),
                     name = "Alliance Status") +
  scale_size(range = c(2, 6), guide=FALSE) + 
  scale_x_continuous(label = percent)+
  scale_y_continuous(label = percent, limits  = c(0.5, 1.5))+
  labs(x = "Resident Students, % FRPL", y = "Difference btwn. Proposed FY16 ECS\nand Adj. FY16 Target Amount") +
  theme(legend.position = c(.2, .8))
ggsave("figures/proposedECS.png", width = 8, height = 5, unit = "in", dpi = 300)

ggplot(proposed_fix, aes(x = frpl_pct, proposed_gap, label = town_name,
                         size = resTotal, color = alliance_status))+
  geom_hline(yintercept = 0) +
  geom_text() + 
  scale_color_manual(values = c("indianred", "lightsteelblue4"),
                     name = "Alliance Status") +
  scale_size(range = c(2, 6), guide=FALSE) + 
  scale_x_continuous(label = percent)+
  scale_y_continuous(label = dollar)+
  labs(x = "Resident Students, % FRPL", y = "Difference btwn. Proposed FY16 ECS\nand Adj. FY16 Target Amount") +
  theme(legend.position = c(.2, .8))
ggsave("figures/proposedECSdollar.png", width = 8, height = 5, unit = "in", dpi = 300)


# close gap for towns w/ >$7m gap ####

# calculate bonus for towns w/ > $7m gap
bonus_towns <- proposed_fix %>% 
  filter(proposed_gap < -7000000) %>%
  mutate(bonus = abs(proposed_gap + 7000000))

# calculate total cost of the bonus
bonus_summary <- bonus_towns %>%
  summarise(count = n(),
            total = sum(bonus))

# determine non-bonus towns
non_bonus_towns <- proposed_fix %>%
  anti_join(bonus_towns, by = "town_id")

# join bonus and non-bonus towns
# calculate $ gap and funding ratio
bonus_fix <- bonus_towns %>%
  rbind(non_bonus_towns %>% mutate(bonus = 0)) %>%
          mutate(proposed_bonus = proposed_fy16 + bonus,
                 bonus_gap = proposed_bonus - gov_adj_target,
                 bonus_pct = proposed_bonus / gov_adj_target)


# plot bonus fix ####
ggplot(bonus_fix, aes(x = frpl_pct, bonus_gap, label = town_name,
                         size = resTotal, color = alliance_status))+
  geom_hline(yintercept = 0) +
  geom_text() + 
  scale_color_manual(values = c("indianred", "lightsteelblue4"),
                     name = "Alliance Status") +
  scale_size(range = c(2, 6), guide=FALSE) + 
  scale_x_continuous(label = percent)+
  scale_y_continuous(label = dollar)+
  labs(x = "Resident Students, % FRPL", y = "Difference between FY16 ECS\nand Adj. FY16 Target Amount") +
  theme(legend.position = c(.2, .8))
ggsave("figures/proposedbonusECSdollars.png", width = 8, height = 5, unit = "in", dpi = 300)

ggplot(bonus_fix, aes(x = frpl_pct, bonus_pct, label = town_name,
                      size = resTotal, color = alliance_status))+
  geom_hline(yintercept = 1) +
  geom_text() + 
  scale_color_manual(values = c("indianred", "lightsteelblue4"),
                     name = "Alliance Status") +
  scale_size(range = c(2, 6), guide=FALSE) + 
  scale_x_continuous(label = percent)+
  scale_y_continuous(label = percent)+
  labs(x = "Resident Students, % FRPL", y = "Difference between FY16 ECS\nand Adj. FY16 Target Amount") +
  theme(legend.position = c(.2, .8))
ggsave("figures/proposedbonusECS.png", width = 8, height = 5, unit = "in", dpi = 300)