WQLoads <- read.csv("WQLoads.csv", fileEncoding = 'UTF-8-BOM')
WQConc <- read.csv("WQConcentrations.csv", fileEncoding = 'UTF-8-BOM')

Loads_Park_Boundary <- WQLoads %>%
  filter(Site %in% c('5', '42', '14', '19', '24'))

Conc_Park_Boundary <- WQConc %>%
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

Loads_Lake_Boundary <- WQLoads %>%
  filter(Site %in% c('2', '8', '16', '22'))

Conc_Lake_Boundary <- WQConc %>%
  filter(Site %in% c('2', '8', '16', '22'))

Sum_LBL <- Loads_Lake_Boundary %>%
  group_by(Date) %>%
  summarize(LB_TN = sum(TN_Load, na.omit = TRUE),
            LB_TP = sum(TP_Load, na.omit = TRUE),
            LB_Nitrate = sum(Nitrate_Load, na.omit = TRUE),
            LB_SRP = sum(SRP_Load, na.omit = TRUE),
            LB_DOC = sum(DOC_Load, na.omit = TRUE))

Sum_LBC <- Conc_Lake_Boundary %>%
  group_by(Date) %>%
  summarize(LB_TN = sum(TN, na.omit = TRUE),
            LB_TP = sum(TP, na.omit = TRUE),
            LB_Nitrate = sum(Nitrate, na.omit = TRUE),
            LB_SRP = sum(SRP, na.omit = TRUE),
            LB_DOC = sum(DOC, na.omit = TRUE),
            Stream_Q = sum(Q, na.omit = TRUE))

Loads_Below_Dam <- WQLoads %>%
  filter(Site %in% c('0'))

Conc_Below_Dam <- WQConc %>%
  filter(Site %in% c('0'))

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
  summarize(BD_TN = (TN),
            BD_TP = (TP),
            BD_Nitrate = (Nitrate),
            BD_SRP = (SRP),
            BD_DOC = (DOC),
            Lake_Q = (Q))

Load_Comp <- Sum_PBL %>%
  left_join(Sum_LBL, by = "Date", keep = FALSE) %>%
  left_join(Sum_BDL, by = "Date", keep = FALSE)

Conc_Comp <- Sum_PBC %>%
  left_join(Sum_LBC, by = "Date", keep = FALSE) %>%
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
            DOC_Lake_Change = -(LB_DOC - BD_DOC))

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

rm(Sum_BDC, Sum_BDL, Sum_LBC, Sum_LBL, Sum_PBC, Sum_PBL, Loads_Below_Dam,
   Loads_Lake_Boundary, Loads_Park_Boundary, Conc_Below_Dam, Conc_Lake_Boundary, Conc_Park_Boundary)

