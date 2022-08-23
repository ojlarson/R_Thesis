library(readr)
library(tidyverse)

#Long_Site_List <- unique(WQConc$Site)[c(38:48, 50:54)]
#Farm_BD_List <- c('0', '5', '6', '42', '12', '14', '15', '19', '20', '24', '25')
#Long_Combined <- c(Long_Site_List, Farm_BD_List)

Long_Date_List <- c("2021-06-29", "2021-08-10", "2021-11-14")
  
WQ_Long <- WQConc %>%
  #filter(Site %in% Long_Combined) %>%
  filter(Date %in% Long_Date_List) %>%
  mutate(Site = as.factor(Site)) %>%
  select(2,3,6,8,9)

WQ_Long$Site <- factor(rev(WQ_Long$Site), levels=unique(WQ_Long$Site))

Long_BD <- WQ_Long %>%
  filter(Site %in% c('0', 'BCB')) %>%
  mutate(Stream = 'BD') %>%
  arrange(Site) %>%
  arrange(Date)

ggplot(data = Long_BD, aes(x = Site)) + #, level = lf_sites))) +
  geom_point(aes(y = Nitrate)) +
  facet_grid(rows = vars(Date), scales = "free")

lf_sites <- rev(c('2', '4', '5', '6', 'LFL1', 'LFL2', 'LFL3', 'LFL4'))

Long_LF <- WQ_Long %>%
  filter(Site %in% lf_sites) %>%
  mutate(Stream = "LF")
Long_LF$Site <- factor(rev(Long_LF$Site), levels=unique(Long_LF$Site))

#%>%
#  arrange(desc(Site)) %>%
#  arrange(Date)

ggplot(data = Long_LF, aes(x = Site)) + #, level = lf_sites))) +
  geom_point(aes(y = Nitrate)) +
  facet_grid(rows = vars(Date), scales = "free")

Long_FM <- WQ_Long %>%
  filter(Site %in% c('42', '12', 'FML1', 'FML2', 'FML3', 'FML4')) %>%
  mutate(Stream = 'FM') %>%
  arrange(desc(Site)) %>%
  arrange(Date)

Long_EF <- WQ_Long %>%
  filter(Site %in% c('14', '15', 'EFL1', 'EFL2', 'EFL3')) %>%
  mutate(Stream = 'EF') %>%
  arrange(desc(Site)) %>%
  arrange(Date)

Long_MB <- WQ_Long %>%
  filter(Site %in% c('19', '20', 'MBL1', 'MBL2')) %>%
  mutate(Stream = 'MB') %>%
  arrange(desc(Site)) %>%
  arrange(Date)

Long_DE <- WQ_Long %>%
  filter(Site %in% c('24', '25', 'DEL1', 'DEL2')) %>%
  mutate(Stream = 'DE') %>%
  arrange(desc(Site)) %>%
  arrange(Date)


Long_Samples <- rbind(Long_BD, Long_LF, Long_FM, Long_EF, Long_MB, Long_DE)

ggplot(data = Long_Samples, aes(x = Site)) +
  geom_point(aes(y = Nitrate)) +
  facet_grid(rows = vars(Date), cols = vars(Stream), scales = "free")

write.csv(Long_Samples, "Long_Samples.csv")
