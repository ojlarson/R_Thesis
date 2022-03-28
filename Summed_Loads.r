Loads_Park_Boundary <- WQLoads %>%
  filter(Site %in% c('5', '42', '14', '19', '24'))

Sum_PBL <- Loads_Park_Boundary %>%
  group_by(Date) %>%
  summarize(PB_TN = sum(TN_Load, na.omit = TRUE),
            PB_TP = sum(TP_Load, na.omit = TRUE),
            PB_Nitrate = sum(Nitrate_Load, na.omit = TRUE),
            PB_SRP = sum(SRP_Load, na.omit = TRUE),
            PB_DOC = sum(DOC_Load, na.omit = TRUE))

Loads_Lake_Boundary <- WQLoads %>%
  filter(Site %in% c('2', '8', '16', '22'))

Sum_LBL <- Loads_Lake_Boundary %>%
  group_by(Date) %>%
  summarize(LB_TN = sum(TN_Load, na.omit = TRUE),
            LB_TP = sum(TP_Load, na.omit = TRUE),
            LB_Nitrate = sum(Nitrate_Load, na.omit = TRUE),
            LB_SRP = sum(SRP_Load, na.omit = TRUE),
            LB_DOC = sum(DOC_Load, na.omit = TRUE))

Loads_Below_Dam <- WQLoads %>%
  filter(Site %in% c('0'))

Sum_BDL <- Loads_Below_Dam %>%
  group_by(Date) %>%
  summarize(BD_TN = (TN_Load),
            BD_TP = (TP_Load),
            BD_Nitrate = (Nitrate_Load),
            BD_SRP = (SRP_Load),
            BD_DOC = (DOC_Load))

Load_Comp <- Sum_PBL %>%
  left_join(Sum_LBL, by = "Date", keep = FALSE) %>%
  left_join(Sum_BDL, by = "Date", keep = FALSE)

Load_Comp_Sum <- Load_Comp %>%
  group_by(Date) %>%
  summarise(TN_Stream_Change = (PB_TN - LB_TN),
            TN_Lake_Change = (LB_TN - BD_TN),
            TP_Stream_Change = (PB_TP - LB_TP),
            TP_Lake_Change = (LB_TP - BD_TP),
            Nitrate_Stream_Change = (PB_Nitrate - LB_Nitrate),
            Nitrate_Lake_Change = (LB_Nitrate - BD_Nitrate),
            SRP_Stream_Change = (PB_SRP - LB_SRP),
            SRP_Lake_Change = (LB_SRP - BD_SRP),
            DOC_Stream_Change = (PB_DOC - LB_DOC),
            DOC_Lake_Change = (LB_DOC - BD_DOC))

Load_Comp_Sum %>% ggplot(aes(Date)) +
  geom_line(aes(y = DOC_Stream_Change), color = 'red') +
  geom_line(aes(y = DOC_Lake_Change), color = 'blue')

