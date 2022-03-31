#Run All_Q first to get 'Sample_Q'
library(readr)
library(googlesheets4)
library(tidyverse)
Sample_Q <- read.csv("Sample_Q.csv", fileEncoding = 'UTF-8-BOM')
WQ_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/1-qtXWXC7bGvOgoFUOhLVjol8GZ5d-jruUyDezfAVw24/edit#gid=0")

WQ <- WQ_sheet %>%
  select(2,3,9,10,11,12,13,18) %>%
  mutate(Site=as.character(Site))
  
colnames(WQ) <- c('Date', 'Site', 'TN', 'TP', 'Nitrate', 'Ammonium', 'SRP', 'DOC')


FMSites <- c('8', '10', '11')
FMSites_Branch <- c('12', '41', '42')
EFSites <- c('40', '14', '15')
LFSites <- c('2', '4', '5', '6')
MBSites <- c('16', '18', '19', '20')
DESites <- c('22', '23', '24', '25')
BDSites <- c('0', '1')
FM_TD <- c('31', '32', '34')
LF_TD <- c('TD1', 'TD2', 'TD3')
FM_Long <- c('FML1', 'FML2', 'FML3', 'FML4')
LF_Long <- c('LFL1', 'LFL2', 'LFL3', 'LFL4')
EF_Long <- c('EFL1', 'EFL2', 'EFL3')
MB_Long <- c('MBL1', 'MBL2')
DE_Long <- c('DEL1', 'DEL2')
BD_Long <- c('BCB')

AllWQ <- WQ

AllWQ$Stream <- as.factor(ifelse(AllWQ$Site %in% FMSites, "FM",
                          ifelse(AllWQ$Site %in% FMSites_Branch, "FM_Branch",
                          ifelse(AllWQ$Site %in% LFSites, "LF",
                          ifelse(AllWQ$Site %in% MBSites, "MB",
                          ifelse(AllWQ$Site %in% DESites, "DE",
                          ifelse(AllWQ$Site %in% EFSites, "EF",
                          ifelse(AllWQ$Site %in% BDSites, "BD",
                          ifelse(AllWQ$Site %in% FM_TD, "FM_TD",
                          ifelse(AllWQ$Site %in% LF_TD, "LF_TD",
                          ifelse(AllWQ$Site %in% FM_Long, "FM_Long",
                          ifelse(AllWQ$Site %in% LF_Long, "LF_Long",
                          ifelse(AllWQ$Site %in% EF_Long, "EF_Long",
                          ifelse(AllWQ$Site %in% MB_Long, "MB_Long",
                          ifelse(AllWQ$Site %in% DE_Long, "DE_Long",
                          ifelse(AllWQ$Site %in% BD_Long, "BD_Long", "Wells"))))))))))))))))


mainSites = c('0', '1', '2', '4', '5', '6', '8', 
              '10', '11', '12', '14', '15', '16', '18', 
              '19', '20', '22', '23', '24', '25', '40', '41', '42')

mainWQ <- WQ %>%
  filter(Site %in% mainSites)


mainWQ$Stream <- as.factor(ifelse(mainWQ$Site %in% FMSites, "FM",
                           ifelse(mainWQ$Site %in% FMSites_Branch, "FM_Branch",
                           ifelse(mainWQ$Site %in% LFSites, "LF",
                           ifelse(mainWQ$Site %in% MBSites, "MB",
                           ifelse(mainWQ$Site %in% DESites, "DE",
                           ifelse(mainWQ$Site %in% EFSites, "EF", "BD")))))))

WQ_Q <- Sample_Q %>%
  mutate(Date.Time=as.POSIXct(Date.Time, format = "%m/%d/%Y %H:%M", tz=Sys.timezone()),
         Date=as.Date(Date.Time),
         Date.Time = NULL)

mainWQ <- mutate(mainWQ, DateStream= paste(as.character(Date), Stream))
WQ_Q <- mutate(WQ_Q, DateStream= paste(as.character(Date), Stream))

WQJoin <- mainWQ %>% left_join(WQ_Q, by = "DateStream",keep = FALSE) %>%
  select(1:10,14) %>%
  rename(Date = 'Date.x', Stream = 'Stream.x')

WQLoads <- WQJoin %>%
  mutate(TN_Load = TN * Q,
         TP_Load = TP * Q,
         Nitrate_Load = Nitrate * Q,
         Ammonium_Load = Ammonium * Q,
         SRP_Load = SRP * Q,
         DOC_Load = DOC * Q) %>%
  select(1,2,9,11:17)

write.csv(WQJoin, "WQConcentrations.csv")
write.csv(WQLoads, "WQLoads.csv")

WQ_Sums <- WQ %>%
  group_by(Site) %>%
  summarise(n = n())
