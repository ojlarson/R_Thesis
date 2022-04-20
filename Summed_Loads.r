library(readr)
library(tidyverse)

WQLoads <- read.csv("WQLoads.csv", fileEncoding = 'UTF-8-BOM')
WQConc <- read.csv("WQConcentrations.csv", fileEncoding = 'UTF-8-BOM')

Total_Stream_Q <- WQConc %>%
  group_by(Date) %>%
  filter(Site %in% c('2', '8', '16', '22')) %>%
  summarise(Total_Q = sum(Q))

WQEMC <- WQConc %>%
  mutate(TNxQ = TN * Q,
         TPxQ = TP *Q,
         NitratexQ = Nitrate * Q,
         SRPxQ = SRP * Q,
         DOCxQ = DOC * Q) %>%
  select(2,3,12:17)

Loads_Park_Boundary <- WQLoads %>%
  filter(Site %in% c('5', '42', '14', '19', '24'))

Conc_Park_Boundary <- WQConc %>%
  filter(Site %in% c('5', '42', '14', '19', '24'))

EMC_Park_Boundary <- WQEMC %>%
  filter(Site %in% c('5', '42', '14', '19', '24'))

Sum_PBL <- Loads_Park_Boundary %>%
  group_by(Date) %>%
  summarize(PB_TN = sum(TN_Load, na.omit = TRUE),
            PB_TP = sum(TP_Load, na.omit = TRUE),
            PB_Nitrate = sum(Nitrate_Load, na.omit = TRUE),
            PB_SRP = sum(SRP_Load, na.omit = TRUE),
            PB_DOC = sum(DOC_Load, na.omit = TRUE))

Sum_PBC <- Conc_Park_Boundary %>%
  group_by(Date) %>%
  summarize(PB_TN = sum(TN, na.omit = TRUE),
            PB_TP = sum(TP, na.omit = TRUE),
            PB_Nitrate = sum(Nitrate, na.omit = TRUE),
            PB_SRP = sum(SRP, na.omit = TRUE),
            PB_DOC = sum(DOC, na.omit = TRUE))

Sum_PBE <- EMC_Park_Boundary %>%
  group_by(Date) %>%
  summarize(PB_TN = sum(TNxQ, na.omit = TRUE)/sum(Q),
            PB_TP = sum(TPxQ, na.omit = TRUE)/sum(Q),
            PB_Nitrate = sum(NitratexQ, na.omit = TRUE)/sum(Q),
            PB_SRP = sum(SRPxQ, na.omit = TRUE)/sum(Q),
            PB_DOC = sum(DOCxQ, na.omit = TRUE)/sum(Q))

Loads_Lake_Boundary <- WQLoads %>%
  filter(Site %in% c('2', '8', '16', '22'))

Conc_Lake_Boundary <- WQConc %>%
  filter(Site %in% c('2', '8', '16', '22'))

EMC_Lake_Boundary <- WQEMC %>%
  filter(Site %in% c('2', '8', '16', '22'))

Sum_LBL <- Loads_Lake_Boundary %>%
  group_by(Date) %>%
  summarize(LB_TN = sum(TN_Load, na.omit = TRUE),
            LB_TP = sum(TP_Load, na.omit = TRUE),
            LB_Nitrate = sum(Nitrate_Load, na.omit = TRUE),
            LB_SRP = sum(SRP_Load, na.omit = TRUE),
            LB_DOC = sum(DOC_Load, na.omit = TRUE),
            Stream_Q = sum(Q, na.omit = TRUE))

Sum_LBC <- Conc_Lake_Boundary %>%
  group_by(Date) %>%
  summarize(LB_TN = sum(TN, na.omit = TRUE),
            LB_TP = sum(TP, na.omit = TRUE),
            LB_Nitrate = sum(Nitrate, na.omit = TRUE),
            LB_SRP = sum(SRP, na.omit = TRUE),
            LB_DOC = sum(DOC, na.omit = TRUE),
            Stream_Q = sum(Q, na.omit = TRUE))

Sum_LBE <- EMC_Lake_Boundary %>%
  group_by(Date) %>%
  summarize(LB_TN = sum(TNxQ, na.omit = TRUE)/sum(Q),
            LB_TP = sum(TPxQ, na.omit = TRUE)/sum(Q),
            LB_Nitrate = sum(NitratexQ, na.omit = TRUE)/sum(Q),
            LB_SRP = sum(SRPxQ, na.omit = TRUE)/sum(Q),
            LB_DOC = sum(DOCxQ, na.omit = TRUE)/sum(Q),
            Stream_Q = sum(Q), na.omit = TRUE)

Loads_Below_Dam <- WQLoads %>%
  filter(Site %in% c('0'))

Conc_Below_Dam <- WQConc %>%
  filter(Site %in% c('0', '1')) %>%
  group_by(Date)

Sum_BDL <- Loads_Below_Dam %>%
  group_by(Date) %>%
  summarize(BD_TN = (TN_Load),
            BD_TP = (TP_Load),
            BD_Nitrate = (Nitrate_Load),
            BD_SRP = (SRP_Load),
            BD_DOC = (DOC_Load),
            BD_Q = (Q))

Sum_BDC <- Conc_Below_Dam %>%
  group_by(Date) %>%
  summarize(BD_TN = mean(TN),
            BD_TP = mean(TP),
            BD_Nitrate = mean(Nitrate),
            BD_SRP = mean(SRP),
            BD_DOC = mean(DOC),
            Lake_Q = sum(Q))

Load_Comp <- Sum_PBL %>%
  left_join(Sum_LBL, by = "Date", keep = FALSE) %>%
  left_join(Sum_BDL, by = "Date", keep = FALSE)

Conc_Comp <- Sum_PBC %>%
  left_join(Sum_LBC, by = "Date", keep = FALSE) %>%
  left_join(Sum_BDC, by = "Date", keep = FALSE)

EMC_Comp <- Sum_PBE %>%
  left_join(Sum_LBE, by = "Date", keep = FALSE) %>%
  left_join(Sum_BDC, by = "Date", keep = FALSE)

Load_Comp_Sum <- Load_Comp %>%
  group_by(Date) %>%
  summarise(TN_Stream_Change = -(PB_TN - LB_TN),
            TN_Lake_Change = -(LB_TN - BD_TN),
            TP_Stream_Change = -(PB_TP - LB_TP),
            TP_Lake_Change = -(LB_TP - BD_TP),
            Nitrate_Stream_Change = -(PB_Nitrate - LB_Nitrate),
            Nitrate_Lake_Change = -(LB_Nitrate - BD_Nitrate),
            SRP_Stream_Change = -(PB_SRP - LB_SRP),
            SRP_Lake_Change = -(LB_SRP - BD_SRP),
            DOC_Stream_Change = -(PB_DOC - LB_DOC),
            DOC_Lake_Change = -(LB_DOC - BD_DOC),
            Q_Change = -(Stream_Q - BD_Q))

#ggplot(data = Load_Comp_Sum, aes(x = DOC_Lake_Change, y = Q_Change)) +
#  geom_point()
  

Load_Comp_Percent <- Load_Comp %>%
  group_by(Date) %>%
  summarise(TN_Stream_Change = (100*((LB_TN - PB_TN)/PB_TN)),
            TN_Lake_Change = (100*((BD_TN - LB_TN)/LB_TN)),
            TP_Stream_Change = (100*((LB_TP - PB_TP)/PB_TP)),
            TP_Lake_Change = (100*((BD_TP - LB_TP)/LB_TP)),
            Nitrate_Stream_Change = (100*((LB_Nitrate - PB_Nitrate)/PB_Nitrate)),
            Nitrate_Lake_Change = (100*((BD_Nitrate - LB_Nitrate)/LB_Nitrate)),
            SRP_Stream_Change = (100*((LB_SRP - PB_SRP)/PB_SRP)),
            SRP_Lake_Change = (100*((BD_SRP - LB_SRP)/LB_SRP)),
            DOC_Stream_Change = (100*((LB_DOC - PB_DOC)/PB_DOC)),
            DOC_Lake_Change = (100*((BD_DOC - LB_DOC)/LB_DOC)))

Conc_Comp_Sum <- Conc_Comp %>%
  group_by(Date) %>%
  summarise(TN_Stream_Change = -(PB_TN - LB_TN),
            TN_Lake_Change = -(LB_TN - BD_TN),
            TP_Stream_Change = -(PB_TP - LB_TP),
            TP_Lake_Change = -(LB_TP - BD_TP),
            Nitrate_Stream_Change = -(PB_Nitrate - LB_Nitrate),
            Nitrate_Lake_Change = -(LB_Nitrate - BD_Nitrate),
            SRP_Stream_Change = -(PB_SRP - LB_SRP),
            SRP_Lake_Change = -(LB_SRP - BD_SRP),
            DOC_Stream_Change = -(PB_DOC - LB_DOC),
            DOC_Lake_Change = -(LB_DOC - BD_DOC),
            Percent_Q_Change = (100*((Lake_Q - Stream_Q)/Stream_Q)))

Conc_Comp_Percent <- Conc_Comp %>%
  group_by(Date) %>%
  summarise(TN_Stream_Change = (100*((LB_TN - PB_TN)/PB_TN)),
            TN_Lake_Change = (100*((BD_TN - LB_TN)/LB_TN)),
            TP_Stream_Change = (100*((LB_TP - PB_TP)/PB_TP)),
            TP_Lake_Change = (100*((BD_TP - LB_TP)/LB_TP)),
            Nitrate_Stream_Change = (100*((LB_Nitrate - PB_Nitrate)/PB_Nitrate)),
            Nitrate_Lake_Change = (100*((BD_Nitrate - LB_Nitrate)/LB_Nitrate)),
            SRP_Stream_Change = (100*((LB_SRP - PB_SRP)/PB_SRP)),
            SRP_Lake_Change = (100*((BD_SRP - LB_SRP)/LB_SRP)),
            DOC_Stream_Change = (100*((LB_DOC - PB_DOC)/PB_DOC)),
            DOC_Lake_Change = (100*((BD_DOC - LB_DOC)/LB_DOC)),
            Percent_Q_Change = (100*((Lake_Q - Stream_Q)/Stream_Q)))

EMC_Comp_Sum <- EMC_Comp %>%
  group_by(Date) %>%
  summarise(TN_Stream_Change = -(PB_TN - LB_TN),
            TN_Lake_Change = -(LB_TN - BD_TN),
            TP_Stream_Change = -(PB_TP - LB_TP),
            TP_Lake_Change = -(LB_TP - BD_TP),
            Nitrate_Stream_Change = -(PB_Nitrate - LB_Nitrate),
            Nitrate_Lake_Change = -(LB_Nitrate - BD_Nitrate),
            SRP_Stream_Change = -(PB_SRP - LB_SRP),
            SRP_Lake_Change = -(LB_SRP - BD_SRP),
            DOC_Stream_Change = -(PB_DOC - LB_DOC),
            DOC_Lake_Change = -(LB_DOC - BD_DOC),
            Percent_Q_Change = (100*((Lake_Q - Stream_Q)/Stream_Q)))

EMC_Comp_Percent <- EMC_Comp %>%
  group_by(Date) %>%
  summarise(TN_Stream_Change = (100*((LB_TN - PB_TN)/PB_TN)),
            TN_Lake_Change = (100*((BD_TN - LB_TN)/LB_TN)),
            TP_Stream_Change = (100*((LB_TP - PB_TP)/PB_TP)),
            TP_Lake_Change = (100*((BD_TP - LB_TP)/LB_TP)),
            Nitrate_Stream_Change = (100*((LB_Nitrate - PB_Nitrate)/PB_Nitrate)),
            Nitrate_Lake_Change = (100*((BD_Nitrate - LB_Nitrate)/LB_Nitrate)),
            SRP_Stream_Change = (100*((LB_SRP - PB_SRP)/PB_SRP)),
            SRP_Lake_Change = (100*((BD_SRP - LB_SRP)/LB_SRP)),
            DOC_Stream_Change = (100*((LB_DOC - PB_DOC)/PB_DOC)),
            DOC_Lake_Change = (100*((BD_DOC - LB_DOC)/LB_DOC)),
            Percent_Q_Change = (100*((Lake_Q - Stream_Q)/Stream_Q)))
  
rm(Sum_BDC, Sum_BDL, Sum_LBC, Sum_LBL, Sum_PBC, Sum_PBL, Loads_Below_Dam,
   Loads_Lake_Boundary, Loads_Park_Boundary, Conc_Below_Dam, Conc_Lake_Boundary, Conc_Park_Boundary)

write.csv(Conc_Comp, "Conc_Comp.csv")
write.csv(Conc_Comp_Percent, "Conc_Comp_Percent.csv")
write.csv(Conc_Comp_Sum, "Conc_Comp_Sum.csv")
write.csv(Load_Comp, "Load_Comp.csv")
write.csv(Load_Comp_Percent, "Load_Comp_Percent.csv")
write.csv(Load_Comp_Sum, "Load_Comp_Sum.csv")
write.csv(EMC_Comp_Sum, "EMC_Comp_Sum.csv")
write.csv(EMC_Comp_Percent, "EMC__Comp_Percent.csv")

