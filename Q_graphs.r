TN_Comp %>%
  ggplot(aes(x = Percent_Q_Change, y = `EMC Percent Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal()
ggsave("Graphs/TN_EMC_Percent_Q.png", width = 7, height = 5)
TN_Comp %>%
  ggplot(aes(x = Percent_Q_Change, y = `EMC Total Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal()
ggsave("Graphs/TN_EMC_Total_Q.png", width = 7, height = 5)
TN_Comp %>%
  ggplot(aes(x = Percent_Q_Change, y = `Load Percent Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal()
ggsave("Graphs/TN_Load_Percent_Q.png", width = 7, height = 5)
TN_Comp %>%
  ggplot(aes(x = Percent_Q_Change, y = `Load Total Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal()
ggsave("Graphs/TN_Load_Total_Q.png", width = 7, height = 5)

TP_Comp %>%
  ggplot(aes(x = Percent_Q_Change, y = `EMC Percent Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal()
ggsave("Graphs/TP_EMC_Percent_Q.png", width = 7, height = 5)
TP_Comp %>%
  ggplot(aes(x = Percent_Q_Change, y = `EMC Total Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal()
ggsave("Graphs/TP_EMC_Total_Q.png", width = 7, height = 5)
TP_Comp %>%
  ggplot(aes(x = Percent_Q_Change, y = `Load Percent Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal()
ggsave("Graphs/TP_Load_Percent_Q.png", width = 7, height = 5)
TP_Comp %>%
  ggplot(aes(x = Percent_Q_Change, y = `Load Total Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal()
ggsave("Graphs/TP_Load_Total_Q.png", width = 7, height = 5)

Nitrate_Comp %>%
  ggplot(aes(x = Percent_Q_Change, y = `EMC Percent Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal()
ggsave("Graphs/Nitrate_EMC_Percent_Q.png", width = 7, height = 5)
Nitrate_Comp %>%
  ggplot(aes(x = Percent_Q_Change, y = `EMC Total Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal()
ggsave("Graphs/Nitrate_EMC_Total_Q.png", width = 7, height = 5)
Nitrate_Comp %>%
  ggplot(aes(x = Percent_Q_Change, y = `Load Percent Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal()
ggsave("Graphs/Nitrate_Load_Percent_Q.png", width = 7, height = 5)
Nitrate_Comp %>%
  ggplot(aes(x = Percent_Q_Change, y = `Load Total Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal()
ggsave("Graphs/Nitrate_Load_Total_Q.png", width = 7, height = 5)

SRP_Comp %>%
  ggplot(aes(x = Percent_Q_Change, y = `EMC Percent Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal()
ggsave("Graphs/SRP_EMC_Percent_Q.png", width = 7, height = 5)
SRP_Comp %>%
  ggplot(aes(x = Percent_Q_Change, y = `EMC Total Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal()
ggsave("Graphs/SRP_EMC_Total_Q.png", width = 7, height = 5)
SRP_Comp %>%
  ggplot(aes(x = Percent_Q_Change, y = `Load Percent Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal()
ggsave("Graphs/SRP_Load_Percent_Q.png", width = 7, height = 5)
SRP_Comp %>%
  ggplot(aes(x = Percent_Q_Change, y = `Load Total Change`)) +
  geom_point(aes(color = Type)) +
  geom_line(y=0, color = 'grey') +
  theme_minimal()
ggsave("Graphs/SRP_Load_Total_Q.png", width = 7, height = 5)