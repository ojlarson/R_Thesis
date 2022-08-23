library(tidyverse)
EMC_Comp_Percent_Tidy <- EMC_Comp_Percent %>%
  mutate(Date = as.POSIXct(Date)) %>%
  pivot_longer(cols = 2:11, values_to = "EMC Percent Change") %>%
  mutate(Variable = ifelse(grepl('DOC', name), 'DOC',
                    ifelse(grepl('Nitrate', name), 'Nitrate',
                    ifelse(grepl('SRP', name), 'SRP',
                    ifelse(grepl('TN', name), 'TN','TP')))),
         Type = ifelse(grepl('Stream', name), 'Stream', 'Lake'),
         DateName = paste(as.character(Date), name))

EMC_Comp_Sum_Tidy <- EMC_Comp_Sum %>%
  mutate(Date = as.POSIXct(Date)) %>%
  pivot_longer(cols = 2:11, values_to = "EMC Total Change") %>%
  mutate(Variable = ifelse(grepl('DOC', name), 'DOC',
                    ifelse(grepl('Nitrate', name), 'Nitrate',
                    ifelse(grepl('SRP', name), 'SRP',
                    ifelse(grepl('TN', name), 'TN','TP')))),
         Type = ifelse(grepl('Stream', name), 'Stream', 'Lake'),
         DateName = paste(as.character(Date), name))

Load_Comp_Percent_Tidy <- Load_Comp_Percent %>%
  mutate(Date = as.POSIXct(Date)) %>%
  filter(Date > "2021-01-01 EST") %>%
  pivot_longer(cols = 2:11, values_to = "Load Percent Change") %>%
  mutate(Variable = ifelse(grepl('DOC', name), 'DOC',
                           ifelse(grepl('Nitrate', name), 'Nitrate',
                                  ifelse(grepl('SRP', name), 'SRP',
                                         ifelse(grepl('TN', name), 'TN','TP')))),
         Type = ifelse(grepl('Stream', name), 'Stream', 'Lake'),
         DateName = paste(as.character(Date), name))

Load_Comp_Sum_Tidy <- Load_Comp_Sum %>%
  mutate(Date = as.POSIXct(Date)) %>%
  filter(Date > "2021-01-01 EST") %>%
  pivot_longer(cols = 2:11, values_to = "Load Total Change") %>%
  mutate(Variable = ifelse(grepl('DOC', name), 'DOC',
                           ifelse(grepl('Nitrate', name), 'Nitrate',
                                  ifelse(grepl('SRP', name), 'SRP',
                                         ifelse(grepl('TN', name), 'TN','TP')))),
         Type = ifelse(grepl('Stream', name), 'Stream', 'Lake'),
         DateName = paste(as.character(Date), name))

All_Comp_Tidy <- EMC_Comp_Percent_Tidy %>%
  left_join(EMC_Comp_Sum_Tidy, by = "DateName", keep = FALSE) %>%
  left_join(Load_Comp_Percent_Tidy, by = "DateName", keep = FALSE) %>%
  left_join(Load_Comp_Sum_Tidy, by = "DateName", keep = FALSE) %>%
  select(1:6, 11, 16, 22) %>%
  rename(Date = Date.x, Q_Change = Q_Change.x, Site_Name = name.x,
         Variable = Variable.x, Type = Type.x) %>%
  filter(Date != "2019-12-08") %>%
  mutate(Date = as.Date(Date))

All_Comp_Long <- All_Comp_Tidy %>%
  relocate(`EMC Percent Change`, .after = last_col()) %>%
  pivot_longer(cols = 6:9, names_to = "Calc", values_to = "Change") %>%
  mutate(TotPer = ifelse(grepl("Total", Calc), "Total", "Percent"),
         EMCLoad = ifelse(grepl("EMC", Calc), "EMC", "Load"))



All_Comp_Long %>%
  ggplot(aes(x = Date, y = Change)) +
  geom_point(aes(color = Type)) +
  facet_grid(cols = vars(EMCLoad), rows = vars(TotPer), scales = "free")

TN_Comp <- All_Comp_Tidy %>%
  filter(Variable == 'TN')
TP_Comp <- All_Comp_Tidy %>%
  filter(Variable == 'TP')
Nitrate_Comp <- All_Comp_Tidy %>%
  filter(Variable == 'Nitrate')
SRP_Comp <- All_Comp_Tidy %>%
  filter(Variable == 'SRP')

min <- as.Date('2021-01-01')
max <- as.Date('2022-01-01')

TN_Comp %>%
  ggplot(aes(x = Date, y = `EMC Percent Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal() +
  scale_x_date(date_labels = "%B %Y", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())
ggsave("Graphs/TN_EMC_Percent.png", width = 7, height = 5)
TN_Comp %>%
  ggplot(aes(x = Date, y = `EMC Total Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal() +
  scale_x_date(date_labels = "%B %Y", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  ylab("Total Change (mg/L)")
ggsave("Graphs/TN_EMC_Total.png", width = 7, height = 5)
TN_Comp %>%
  ggplot(aes(x = Date, y = `Load Percent Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal() +
  scale_x_date(date_labels = "%B %Y", date_breaks = "3 months") +
  xlim(min,max) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())
ggsave("Graphs/TN_Load_Percent.png", width = 7, height = 5)
TN_Comp %>%
  ggplot(aes(x = Date, y = `Load Total Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal() +
  scale_x_date(date_labels = "%B %Y", date_breaks = "3 months") +
  xlim(min,max) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())
ggsave("Graphs/TN_Load_Total.png", width = 7, height = 5)

TP_Comp %>%
  ggplot(aes(x = Date, y = `EMC Percent Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal() +
  scale_x_date(date_labels = "%B %Y", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())
ggsave("Graphs/TP_EMC_Percent.png", width = 7, height = 5)
TP_Comp %>%
  ggplot(aes(x = Date, y = `EMC Total Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal() +
  scale_x_date(date_labels = "%B %Y", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  ylab("Total Change (mg/L)")
ggsave("Graphs/TP_EMC_Total.png", width = 7, height = 5)
TP_Comp %>%
  ggplot(aes(x = Date, y = `Load Percent Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal() +
  scale_x_date(date_labels = "%B %Y", date_breaks = "3 months") +
  xlim(min,max) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())
ggsave("Graphs/TP_Load_Percent.png", width = 7, height = 5)
TP_Comp %>%
  ggplot(aes(x = Date, y = `Load Total Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal() +
  scale_x_date(date_labels = "%B %Y", date_breaks = "3 months") +
  xlim(min,max) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())
ggsave("Graphs/TP_Load_Total.png", width = 7, height = 5)

Nitrate_Comp %>%
  ggplot(aes(x = Date, y = `EMC Percent Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal() +
  scale_x_date(date_labels = "%B %Y", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())
ggsave("Graphs/Nitrate_EMC_Percent.png", width = 7, height = 5)
Nitrate_Comp %>%
  ggplot(aes(x = Date, y = `EMC Total Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal() +
  scale_x_date(date_labels = "%B %Y", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  ylab("Total Change (mg/L)")
ggsave("Graphs/Nitrate_EMC_Total.png", width = 7, height = 5)
Nitrate_Comp %>%
  ggplot(aes(x = Date, y = `Load Percent Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal() +
  scale_x_date(date_labels = "%B %Y", date_breaks = "3 months") +
  xlim(min,max) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())
ggsave("Graphs/Nitrate_Load_Percent.png", width = 7, height = 5)
Nitrate_Comp %>%
  ggplot(aes(x = Date, y = `Load Total Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal() +
  scale_x_date(date_labels = "%B %Y", date_breaks = "3 months") +
  xlim(min,max) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())
ggsave("Graphs/Nitrate_Load_Total.png", width = 7, height = 5)

SRP_Comp %>%
  ggplot(aes(x = Date, y = `EMC Percent Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal() +
  scale_x_date(date_labels = "%B %Y", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())
ggsave("Graphs/SRP_EMC_Percent.png", width = 7, height = 5)
SRP_Comp %>%
  ggplot(aes(x = Date, y = `EMC Total Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal() +
  scale_x_date(date_labels = "%B %Y", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  ylab(expression("Total Change ("*mu~"g/L)"))  
ggsave("Graphs/SRP_EMC_Total.png", width = 7, height = 5)
SRP_Comp %>%
  ggplot(aes(x = Date, y = `Load Percent Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal() +
  scale_x_date(date_labels = "%B %Y", date_breaks = "3 months") +
  xlim(min,max) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  ylab(expression("Total Change (" * mu ~ "g/L)"))   
ggsave("Graphs/SRP_Load_Percent.png", width = 7, height = 5)
SRP_Comp %>%
  ggplot(aes(x = Date, y = `Load Total Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal() +
  scale_x_date(date_labels = "%B %Y", date_breaks = "3 months") +
  xlim(min,max) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())
ggsave("Graphs/SRP_Load_Total.png", width = 7, height = 5)

#EMC_Comp_Percent_Tidy %>%
#  ggplot(aes(x = Date, y = `Percent Change`)) +
#         geom_point(aes(color = Type)) +
#         facet_grid(rows = vars(Variable), scales = 'free')
#ggsave("Graphs/EMC_Percent.png", width = 10, height = 10, units = "in")
#EMC_Comp_Sum_Tidy %>%
#  ggplot(aes(x = Date, y = `Total Change`)) +
#        geom_point(aes(color = Type)) +
#        facet_grid(rows = vars(Variable), scales = 'free')
#ggsave("Graphs/EMC_Total.png", width = 10, height = 10, units = "in")
#Load_Comp_Percent_Tidy %>%
#  ggplot(aes(x = Date, y = `Percent Change`)) +
#  geom_point(aes(color = Type)) +
#  facet_grid(rows = vars(Variable), scales = 'free')
#ggsave("Graphs/Load_Percent.png", width = 10, height = 10, units = "in")
#Load_Comp_Sum_Tidy %>%
#  ggplot(aes(x = Date, y = `Total Change`)) +
#  geom_point(aes(color = Type)) +
#  facet_grid(rows = vars(Variable), scales = 'free')
#ggsave("Graphs/Load_Total.png", width = 10, height = 10, units = "in")





