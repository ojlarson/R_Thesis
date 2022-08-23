library(tidyverse)

setwd("C:/Users/owenl/Documents/R_Thesis")

EMC_Comp_Sum_Tidy <- EMC_Comp_Sum %>%
  mutate(Date = as.POSIXct(Date)) %>%
  pivot_longer(cols = 2:11, values_to = "EMC Total Change") %>%
  mutate(Variable = ifelse(grepl('DOC', name), 'DOC',
                    ifelse(grepl('Nitrate', name), 'Nitrate',
                    ifelse(grepl('SRP', name), 'SRP',
                    ifelse(grepl('TN', name), 'TN','TP')))),
         Type = ifelse(grepl('Stream', name), 'Stream', 'Lake'),
         DateName = paste(as.character(Date), name)) %>%
  filter(Date != "2019-12-08") %>%
  filter(Variable != "DOC") %>%
  mutate(Date = as.Date(Date))

EMC_Comp_Tidy <- EMC_Comp %>%
  select(-Stream_Q, -Lake_Q) %>%
  pivot_longer(cols = 2:15, values_to = "Weighted_Conc") %>%
  mutate(Location = ifelse(grepl('PB', name), 'Park Boundary',
                    ifelse(grepl('LB', name), 'Lake Boundary', 'Below Dam')),
         Variable = ifelse(grepl('DOC', name), 'DOC',
                    ifelse(grepl('Nitrate', name), 'Nitrate',
                    ifelse(grepl('SRP', name), 'SRP',
                    ifelse(grepl('TN', name), 'TN','TP'))))) %>%
  filter(Date != "2019-12-08") %>%
  filter(Variable != "DOC") %>%
  mutate(Date = as.Date(Date),
         Location = factor(Location, level = c("Park Boundary", "Lake Boundary", "Below Dam"))) %>%
  select(-name)


EMC_Comp_Sum_Tidy %>%
  mutate(Date = as.Date(Date),
         Variable = factor(Variable, levels = c("TN", "Nitrate", "TP", "SRP"))) %>%
  filter(Variable != "DOC") %>%
  ggplot(aes(x = Date, y = `EMC Total Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  facet_grid(Variable ~ ., scales = "free") +
  theme_minimal() +
  scale_x_date(date_labels = "%B %Y", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())
ggsave("Aug_22_Graphs/All_By_Date.png", width = 11, height = 8.5, bg = "white")
EMC_Comp_Sum_Tidy %>%
  #filter(Variable == "TN") %>%
  ggplot(aes(x = Type, y = `EMC Total Change`, color = Type)) +
  geom_boxplot() +
  facet_wrap(vars(Variable), scales = "free") +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Total Change (mg/L)")
ggsave("Aug_22_Graphs/Change_Boxplots.png", width = 11, height = 8.5, bg = "white")

EMC_Comp_Tidy %>%
  ggplot(aes(x = Location, y = Weighted_Conc, fill = Location)) +
  geom_boxplot() +
  facet_grid(Variable ~ ., scales = "free") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette="Set2")
ggsave("Aug_22_Graphs/All_Boxplots.png", width = 11, height = 8.5, bg = "white")
Q_Comp_Tidy <- Q_Comp %>%
  pivot_longer(cols = 2:3, names_to = "Type", values_to = "Q") %>%
  select(1, 4:5)

Q_Comp_Tidy %>%
  mutate(Date = as.Date(Date)) %>%
  ggplot(aes(Date, Q, color = Type)) +
  geom_point() +
  theme_minimal() +
  scale_x_date(date_labels = "%B %Y", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  ylab("Q (m/s^2)")
ggsave("Aug_22_Graphs/Total_Q.png", width = 11, height = 8.5, bg = "white")
Q_Change_Tidy <- Q_Comp %>%
  mutate(Date = as.Date(Date)) %>%
  pivot_longer(cols = 4:5, names_to = "Type", values_to = "Change") %>%
  select(1, 4:5)
library(ggrepel)

Q_Change_Tidy %>%
  filter(Date > '2021-01-01') %>%
  ggplot(aes(Date, Change, label = (as.character(Date)))) +
  geom_point() + 
  #geom_label_repel() +
  facet_grid(Type ~ ., scales = "free") +
  theme(axis.title.y = element_blank())
ggsave("Aug_22_Graphs/Q_Change.png", width = 11, height = 8.5, bg = "white")
