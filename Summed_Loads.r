library(readr)
library(tidyverse)

WQLoads <- read.csv("WQLoads.csv", fileEncoding = 'UTF-8-BOM')
WQConc <- read.csv("WQConcentrations.csv", fileEncoding = 'UTF-8-BOM') %>%
  mutate(DateSite = paste(Date, Site))

streamFactor = 1.112723081

Total_Stream_Q <- WQConc %>%
  group_by(Date) %>%
  filter(Site %in% c('2', '8', '16', '22')) %>%
  summarise(Stream_Q = (sum(Q*streamFactor)))
Total_Lake_Q <- WQConc %>%
  group_by(Date) %>%
  filter(Site %in% c('0')) %>%
  summarise(Lake_Q = sum(Q))
Q_Comp <- Total_Stream_Q %>%
  left_join(Total_Lake_Q, by = "Date", keep = FALSE) %>%
  mutate(Change = Lake_Q - Stream_Q,
  Percent_Change = (Lake_Q - Stream_Q)/Stream_Q * 100)
rm(Total_Stream_Q, Total_Lake_Q)
write.csv(Q_Comp, "Q_Comp.csv")

#WQEMC <- WQConc %>%
#  mutate(TNxQ = TN * Q,
#         TPxQ = TP *Q,
#         NitratexQ = Nitrate * Q,
#         SRPxQ = SRP * Q,
#         DOCxQ = DOC * Q) %>%
#  select(2,3,12:17)

Loads_Park_Boundary <- WQLoads %>%
  filter(Site %in% c('5', '42', '14', '19', '24'))

#Conc_Park_Boundary <- WQConc %>%
#  filter(Site %in% c('5', '42', '14', '19', '24'))

#EMC_Park_Boundary <- WQEMC %>%
#  filter(Site %in% c('5', '42', '14', '19', '24'))

Sum_PBL <- Loads_Park_Boundary %>%
  group_by(Date) %>%
  summarize(PB_TN = sum(TN_Load),
            PB_TP = sum(TP_Load),
            PB_Nitrate = sum(Nitrate_Load),
            PB_SRP = sum(SRP_Load),
            PB_DOC = sum(DOC_Load))

Count_PBL <- Loads_Park_Boundary %>%
  group_by(Date) %>%
  summarize(PB_TN_N = n_distinct(TN_Load),
            PB_TP_N = n_distinct(TP_Load),
            PB_Nitrate_N = n_distinct(Nitrate_Load),
            PB_SRP_N = n_distinct(SRP_Load),
            PB_DOC_N = n_distinct(DOC_Load))

#Sum_PBC <- Conc_Park_Boundary %>%
#  group_by(Date) %>%
#  summarize(PB_TN = sum(TN),
#            PB_TP = sum(TP),
#            PB_Nitrate = sum(Nitrate),
#            PB_SRP = sum(SRP),
#            PB_DOC = sum(DOC))

Sum_PBE <- Loads_Park_Boundary %>%
  group_by(Date) %>%
  summarize(PB_TN = sum(TN_Load)/sum(Q),
            PB_TP = sum(TP_Load)/sum(Q),
            PB_Nitrate = sum(Nitrate_Load)/sum(Q),
            PB_SRP = sum(SRP_Load)/sum(Q),
            PB_DOC = sum(DOC_Load)/sum(Q))

Loads_Lake_Boundary <- WQLoads %>%
  filter(Site %in% c('2', '8', '16', '22'))

#Conc_Lake_Boundary <- WQConc %>%
#  filter(Site %in% c('2', '8', '16', '22'))

#EMC_Lake_Boundary <- WQEMC %>%
#  filter(Site %in% c('2', '8', '16', '22'))

Sum_LBL <- Loads_Lake_Boundary %>%
  group_by(Date) %>%
  summarize(LB_TN = sum(TN_Load),
            LB_TP = sum(TP_Load),
            LB_Nitrate = sum(Nitrate_Load),
            LB_SRP = sum(SRP_Load),
            LB_DOC = sum(DOC_Load),
            Stream_Q = sum(Q))

Count_LBL <- Loads_Lake_Boundary %>%
  group_by(Date) %>%
  summarize(PB_TN_N = n_distinct(TN_Load),
            PB_TP_N = n_distinct(TP_Load),
            PB_Nitrate_N = n_distinct(Nitrate_Load),
            PB_SRP_N = n_distinct(SRP_Load),
            PB_DOC_N = n_distinct(DOC_Load))

Counts <- Count_PBL %>%
  left_join(Count_LBL, by = 'Date', keep = FALSE)

#Sum_LBC <- Conc_Lake_Boundary %>%
#  group_by(Date) %>%
#  summarize(LB_TN = sum(TN),
#            LB_TP = sum(TP),
#            LB_Nitrate = sum(Nitrate),
#            LB_SRP = sum(SRP),
#            LB_DOC = sum(DOC),
#            Stream_Q = sum(Q))

Sum_LBE <- Loads_Lake_Boundary %>%
  group_by(Date) %>%
  summarize(LB_TN = sum(TN_Load)/sum(Q),
            LB_TP = sum(TP_Load)/sum(Q),
            LB_Nitrate = sum(Nitrate_Load)/sum(Q),
            LB_SRP = sum(SRP_Load)/sum(Q),
            LB_DOC = sum(DOC_Load)/sum(Q),
            Stream_Q = sum(Q))

Loads_Below_Dam <- WQLoads %>%
  filter(Site %in% c('0'))

Conc_Below_Dam <- WQConc %>%
  filter(Site %in% c('0', '1')) %>%
  group_by(Date) 

ConcBD <- Conc_Below_Dam %>%
  group_by(Date) %>%
  mutate(Site = as.numeric(Site)) %>%
  summarize(Site = mean(Site)) %>%
  mutate(Site = as.character(ifelse(Site < 1, 0, 1)),
         DateSite = paste(Date, Site)) %>%
  left_join(WQConc, by = 'DateSite', keep = FALSE) %>%
  select(1,2,7:12,15) %>%
  mutate(Date = Date.x)

Sum_BDL <- Loads_Below_Dam %>%
  group_by(Date) %>%
  summarize(BD_TN = (TN_Load),
            BD_TP = (TP_Load),
            BD_Nitrate = (Nitrate_Load),
            BD_SRP = (SRP_Load),
            BD_DOC = (DOC_Load),
            Lake_Q = (Q))

Sum_BDC <- ConcBD %>%
  group_by(Date) %>%
  summarize(BD_TN = TN,
            BD_TP = TP,
            BD_Nitrate = Nitrate,
            BD_SRP = SRP,
            BD_DOC = DOC,
            Lake_Q = Q)

Load_Comp <- Sum_PBL %>%
  left_join(Sum_LBL, by = "Date", keep = FALSE) %>%
  left_join(Sum_BDL, by = "Date", keep = FALSE)

#Conc_Comp <- Sum_PBC %>%
#  left_join(Sum_LBC, by = "Date", keep = FALSE) %>%
#  left_join(Sum_BDC, by = "Date", keep = FALSE)

EMC_Comp <- Sum_PBE %>%
  left_join(Sum_LBE, by = "Date", keep = FALSE) %>%
  left_join(Sum_BDC, by = "Date", keep = FALSE)

Load_Comp_Sum <- Load_Comp %>%
  group_by(Date) %>%
  summarise(TN_Stream_Change = -(PB_TN - LB_TN),
            TN_Lake_Change = -((LB_TN*streamFactor) - BD_TN),
            TP_Stream_Change = -(PB_TP - LB_TP),
            TP_Lake_Change = -((streamFactor*LB_TP) - BD_TP),
            Nitrate_Stream_Change = -(PB_Nitrate - LB_Nitrate),
            Nitrate_Lake_Change = -((streamFactor*LB_Nitrate) - BD_Nitrate),
            SRP_Stream_Change = -(PB_SRP - LB_SRP),
            SRP_Lake_Change = -((streamFactor*LB_SRP) - BD_SRP),
            DOC_Stream_Change = -(PB_DOC - LB_DOC),
            DOC_Lake_Change = -((streamFactor*LB_DOC) - BD_DOC),
            Q_Change = -(Stream_Q*streamFactor - Lake_Q))

#ggplot(data = Load_Comp_Sum, aes(x = DOC_Lake_Change, y = Q_Change)) +
#  geom_point()
  

Load_Comp_Percent <- Load_Comp %>%
  group_by(Date) %>%
  summarise(TN_Stream_Change = (100*((LB_TN - PB_TN)/PB_TN)),
            TN_Lake_Change = (100*(BD_TN - (streamFactor*LB_TN))/(streamFactor*LB_TN)),
            TP_Stream_Change = (100*((LB_TP - PB_TP)/PB_TP)),
            TP_Lake_Change = (100*(BD_TP - (streamFactor*LB_TP))/(streamFactor*LB_TP)),
            Nitrate_Stream_Change = (100*((LB_Nitrate - PB_Nitrate)/PB_Nitrate)),
            Nitrate_Lake_Change = (100*(BD_Nitrate - (streamFactor*LB_Nitrate))/(streamFactor*LB_Nitrate)),
            SRP_Stream_Change = (100*((LB_SRP - PB_SRP)/PB_SRP)),
            SRP_Lake_Change = (100*(BD_SRP - (streamFactor*LB_SRP))/(streamFactor*LB_SRP)),
            DOC_Stream_Change = (100*((LB_DOC - PB_DOC)/PB_DOC)),
            DOC_Lake_Change = (100*(BD_DOC - (streamFactor*LB_DOC))/(streamFactor*LB_DOC)))

#Conc_Comp_Sum <- Conc_Comp %>%
#  group_by(Date) %>%
#  summarise(TN_Stream_Change = -(PB_TN - LB_TN),
#            TN_Lake_Change = -(LB_TN - BD_TN),
#            TP_Stream_Change = -(PB_TP - LB_TP),
#            TP_Lake_Change = -(LB_TP - BD_TP),
#            Nitrate_Stream_Change = -(PB_Nitrate - LB_Nitrate),
#            Nitrate_Lake_Change = -(LB_Nitrate - BD_Nitrate),
#            SRP_Stream_Change = -(PB_SRP - LB_SRP),
#            SRP_Lake_Change = -(LB_SRP - BD_SRP),
#            DOC_Stream_Change = -(PB_DOC - LB_DOC),
#            DOC_Lake_Change = -(LB_DOC - BD_DOC),
#            Percent_Q_Change = (100*((Lake_Q - Stream_Q)/Stream_Q)))

#Conc_Comp_Percent <- Conc_Comp %>%
#  group_by(Date) %>%
#  summarise(TN_Stream_Change = (100*((LB_TN - PB_TN)/PB_TN)),
#            TN_Lake_Change = (100*((BD_TN - LB_TN)/LB_TN)),
#            TP_Stream_Change = (100*((LB_TP - PB_TP)/PB_TP)),
#            TP_Lake_Change = (100*((BD_TP - LB_TP)/LB_TP)),
#            Nitrate_Stream_Change = (100*((LB_Nitrate - PB_Nitrate)/PB_Nitrate)),
#            Nitrate_Lake_Change = (100*((BD_Nitrate - LB_Nitrate)/LB_Nitrate)),
#            SRP_Stream_Change = (100*((LB_SRP - PB_SRP)/PB_SRP)),
#            SRP_Lake_Change = (100*((BD_SRP - LB_SRP)/LB_SRP)),
#            DOC_Stream_Change = (100*((LB_DOC - PB_DOC)/PB_DOC)),
#            DOC_Lake_Change = (100*((BD_DOC - LB_DOC)/LB_DOC)),
#            Percent_Q_Change = (100*((Lake_Q - Stream_Q)/Stream_Q)))

EMC_Comp_Sum <- EMC_Comp %>%
  group_by(Date) %>%
  summarise(TN_Stream_Change = -(PB_TN - LB_TN),
            TN_Lake_Change = -((LB_TN*streamFactor) - BD_TN),
            TP_Stream_Change = -(PB_TP - LB_TP),
            TP_Lake_Change = -((streamFactor*LB_TP) - BD_TP),
            Nitrate_Stream_Change = -(PB_Nitrate - LB_Nitrate),
            Nitrate_Lake_Change = -((streamFactor*LB_Nitrate) - BD_Nitrate),
            SRP_Stream_Change = -(PB_SRP - LB_SRP),
            SRP_Lake_Change = -((streamFactor*LB_SRP) - BD_SRP),
            DOC_Stream_Change = -(PB_DOC - LB_DOC),
            DOC_Lake_Change = -((streamFactor*LB_DOC) - BD_DOC),
            Q_Change = -(Stream_Q*streamFactor - Lake_Q))

EMC_Comp_Percent <- EMC_Comp %>%
  group_by(Date) %>%
  summarise(TN_Stream_Change = -(PB_TN - LB_TN),
            TN_Lake_Change = -((LB_TN*streamFactor) - BD_TN),
            TP_Stream_Change = -(PB_TP - LB_TP),
            TP_Lake_Change = -((streamFactor*LB_TP) - BD_TP),
            Nitrate_Stream_Change = -(PB_Nitrate - LB_Nitrate),
            Nitrate_Lake_Change = -((streamFactor*LB_Nitrate) - BD_Nitrate),
            SRP_Stream_Change = -(PB_SRP - LB_SRP),
            SRP_Lake_Change = -((streamFactor*LB_SRP) - BD_SRP),
            DOC_Stream_Change = -(PB_DOC - LB_DOC),
            DOC_Lake_Change = -((streamFactor*LB_DOC) - BD_DOC),
            Q_Change = -(Stream_Q*streamFactor - Lake_Q))
  
#rm(Sum_BDC, Sum_BDL, Sum_LBC, Sum_LBL, Sum_PBC, Sum_PBL, Loads_Below_Dam,
#   Loads_Lake_Boundary, Loads_Park_Boundary, Conc_Below_Dam, Conc_Lake_Boundary, Conc_Park_Boundary)

#write.csv(Conc_Comp, "Conc_Comp.csv")
#write.csv(Conc_Comp_Percent, "Conc_Comp_Percent.csv")
#write.csv(Conc_Comp_Sum, "Conc_Comp_Sum.csv")
#write.csv(Load_Comp, "Load_Comp.csv")
#write.csv(Load_Comp_Percent, "Load_Comp_Percent.csv")
#write.csv(Load_Comp_Sum, "Load_Comp_Sum.csv")
#write.csv(EMC_Comp, "EMC_Comp.csv")
#write.csv(EMC_Comp_Sum, "EMC_Comp_Sum.csv")
#write.csv(EMC_Comp_Percent, "EMC_Comp_Percent.csv")

