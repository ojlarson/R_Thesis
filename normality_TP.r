library(ggpubr)
library(PairedData)
library(tidyverse)

EMC_Per_TP <- EMC_Comp_Percent_Tidy %>%
  filter(Variable == "TP", !is.na(`Percent Change`))

EMC_Per_TP_Diff <- EMC_Per_TP %>%
  group_by(Date) %>%
  arrange(desc(Type)) %>%
  summarise(Count = n(),
            Diff = diff(`Percent Change`))

EMC_Per_TP_Diff_Summ <- EMC_Per_TP_Diff %>%
  summarise(mean = mean(Diff),
            sd = sd(Diff))

qqnorm(EMC_Per_TP_Diff$Diff)
qqline(EMC_Per_TP_Diff$Diff)
shapiro.test(EMC_Per_TP_Diff$Diff)

EMC_Per_TP_Summ <- EMC_Per_TP %>%
  group_by(Type) %>%
  summarise(count = n_distinct(`Percent Change`),
            mean = mean(`Percent Change`, na.rm = TRUE),
            sd = sd(`Percent Change`, na.rm = TRUE))

ggboxplot(EMC_Per_TP, x = "Type", y = "Percent Change",
          color = "Type")


EMC_Sum_TP <- EMC_Comp_Sum_Tidy %>%
  filter(Variable == "TP", !is.na(`Total Change`))

EMC_Sum_TP_Diff <- EMC_Sum_TP %>%
  group_by(Date) %>%
  arrange(desc(Type)) %>%
  summarise(Count = n(),
            Diff = diff(`Total Change`))

EMC_Sum_TP_Diff_Summ <- EMC_Sum_TP_Diff %>%
  summarise(mean = mean(Diff),
            sd = sd(Diff))

qqnorm(EMC_Sum_TP_Diff$Diff)
qqline(EMC_Sum_TP_Diff$Diff)
shapiro.test(EMC_Sum_TP_Diff$Diff)

EMC_Sum_TP_Summ <- EMC_Sum_TP %>%
  group_by(Type) %>%
  summarise(count = n_distinct(`Total Change`),
            mean = mean(`Total Change`, na.rm = TRUE),
            sd = sd(`Total Change`, na.rm = TRUE))

ggboxplot(EMC_Sum_TP, x = "Type", y = "Total Change",
          color = "Type")



Load_Per_TP <- Load_Comp_Percent_Tidy %>%
  filter(Variable == "TP", !is.na(`Percent Change`))

Load_Per_TP_Diff <- Load_Per_TP %>%
  group_by(Date) %>%
  arrange(desc(Type)) %>%
  summarise(Count = n(),
            Diff = diff(`Percent Change`))

Load_Per_TP_Diff_Summ <- Load_Per_TP_Diff %>%
  summarise(mean = mean(Diff),
            sd = sd(Diff))

qqnorm(Load_Per_TP_Diff$Diff)
qqline(Load_Per_TP_Diff$Diff)
shapiro.test(Load_Per_TP_Diff$Diff)

Load_Per_TP_Summ <- Load_Per_TP %>%
  group_by(Type) %>%
  summarise(count = n_distinct(`Percent Change`),
            mean = mean(`Percent Change`, na.rm = TRUE),
            sd = sd(`Percent Change`, na.rm = TRUE))

ggboxplot(Load_Per_TP, x = "Type", y = "Percent Change",
          color = "Type")


Load_Sum_TP <- Load_Comp_Sum_Tidy %>%
  filter(Variable == "TP", !is.na(`Total Change`))

Load_Sum_TP_Diff <- Load_Sum_TP %>%
  group_by(Date) %>%
  arrange(desc(Type)) %>%
  summarise(Count = n(),
            Diff = diff(`Total Change`))

Load_Sum_TP_Diff_Summ <- Load_Sum_TP_Diff %>%
  summarise(mean = mean(Diff),
            sd = sd(Diff))

qqnorm(Load_Sum_TP_Diff$Diff)
qqline(Load_Sum_TP_Diff$Diff)
shapiro.test(Load_Sum_TP_Diff$Diff)

Load_Sum_TP_Summ <- Load_Sum_TP %>%
  group_by(Type) %>%
  summarise(count = n_distinct(`Total Change`),
            mean = mean(`Total Change`, na.rm = TRUE),
            sd = sd(`Total Change`, na.rm = TRUE))

ggboxplot(Load_Sum_TP, x = "Type", y = "Total Change",
          color = "Type")

EMC_Per_TP %>%
  wilcox.test.paired()

