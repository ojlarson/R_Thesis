Q_Comp %>%
  ggplot(aes(x = Date)) +
  #geom_point(aes(y = Change, color = 'red')) +
  #geom_point(aes(y = Percent_Change, color = 'orange')) +
  geom_point(aes(y = Stream_Q, color = 'green')) +
  geom_point(aes(y = Lake_Q, color = 'blue'))


TN_Comp %>%
  ggplot(aes(x = Type, y = `EMC Total Change`, color = Type)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Total Change (mg/L)")
ggsave("Graphs/BP_TN_EMC_Total.png", width = 7, height = 5)

TP_Comp %>%
  ggplot(aes(x = Type, y = `EMC Total Change`, color = Type)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Total Change (mg/L)")
ggsave("Graphs/BP_TP_EMC_Total.png", width = 7, height = 5)
Nitrate_Comp %>%
  ggplot(aes(x = Type, y = `EMC Total Change`, color = Type)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Total Change (mg/L)")
ggsave("Graphs/BP_Nitrate_EMC_Total.png", width = 7, height = 5)
SRP_Comp %>%
  ggplot(aes(x = Type, y = `EMC Total Change`, color = Type)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("") +
  ylab(expression("Total Change ("*mu~"g/L)"))  
ggsave("Graphs/BP_SRP_EMC_Total.png", width = 7, height = 5)

TP_Comp_Lake <- TP_Comp %>%
  filter(Type == "Lake")

TP_Comp_Lake %>%
  ggplot(aes(x = Date)) +
  geom_point(aes(y = `EMC Total Change`, color = "Red")) +
  geom_line(y=0, color = 'grey') +
  theme_minimal() +
  scale_x_date(date_labels = "%B %Y", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())
