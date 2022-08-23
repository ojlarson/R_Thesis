library(tidyverse)
library(rstatix)
library(devtools)
library(ggpmisc)
library(ggpubr)
ndvi <- read.csv('NDVI_Master.csv', fileEncoding = 'UTF-8-BOM')
Sample_Dates <- Sample_Stages$Date.Time
DOC <- read.csv('DOC.csv', fileEncoding = 'UTF-8-BOM', header = FALSE)
DOC_Values <- DOC$V1
df <- data.frame(Sample_Dates, DOC_Values)
ndvi_val <- ndvi %>%
  mutate(tots = Mean_NDVI * Pixel_Cnt) %>%
  group_by(Date) %>%
  summarise(mean_ndvi = (sum(tots)/(sum(Pixel_Cnt))))
ndvi_vals <- ndvi_val %>%
  mutate(Date = as.POSIXct(Date,format='%m/%d/%Y')) %>%
  mutate(Date = as.character(Date)) %>%
  arrange(Date)
docDF <- df %>%
  mutate(Date = as.POSIXct(Sample_Dates,format='%m/%d/%Y')) %>%
  mutate(Date = as.character(Date)) %>%
  select(2:3)
RM_DF <- docDF %>%
  left_join(ndvi_vals, by = "Date", keep = FALSE) %>%
  left_join(EMC_Comp_Sum) %>%
  left_join(EMC_Comp) %>%
  mutate(Date = as.Date(Date))

RM_DF %>% ggplot(aes(DOC_Values, BD_DOC)) +
  stat_cor(label.x = 0.65, label.y = 3) +
  stat_regline_equation(label.x = 0.65, label.y = 3.2) +
  geom_point() +
  theme_bw() +
  #stat_poly_eq(parse=T, aes(label = ..eq.label..), formula=y~x)
  xlab('Median Pixel Value (B3 / B2)') +
  ylab('DOC Concentration (mg/L)')

ggscatter(RM_DF, x = 'DOC_Values', y = 'BD_DOC', add = "reg.line") +
  stat_cor(label.x = 0.65, label.y = 3) +
  stat_regline_equation(label.x = 0.65, label.y = 3.2) +
  xlab('Median Pixel Value (B3 / B2)') +
  ylab('DOC Concentration (mg/L)') +
  theme_bw()

ggscatter(RM_DF, x = 'mean_ndvi', y = 'Nitrate_Stream_Change') +
  geom_smooth(method=lm, formula = y~x) +
  stat_cor(label.x = 0.1, label.y = 0.25) +
  stat_regline_equation(label.x = 0.1, label.y = .35) +
  xlab('Mean NDVI') +
  ylab('Change in TN Concentration (mg/L)') +
  theme_bw()

RM_DF %>% ggplot(aes(Date, mean_ndvi)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  ylab('Mean NDVI')

RM_DF %>% ggplot(aes(Date, DOC_Values)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  ylab('Median Pixel Value (B3 / B2)')

RM_DF %>% ggplot(aes(Date, BD_DOC)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  ylab('DOC Concentration (mg/L)')

RM_DF %>% ggplot(aes(Date, LB_TN)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  ylab('TN Concentration (mg/L)')

RM_DF %>% ggplot(aes(Date, TN_Stream_Change)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  ylab('Change in TN Concentration (mg/L)')

RM_DF %>% ggplot(aes(Date, mean_ndvi)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  ylab('Mean NDVI')
