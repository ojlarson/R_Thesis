Loads_Park_Boundary <- WQLoads %>%
  filter(Site %in% c('5', '42', '14', '19', '24'))

Sum_PBL <- Loads_Park_Boundary %>%
  group_by(Date) %>%
  summarize(Sum_TN = sum(TN_Load, na.omit = TRUE),
            Sum_TP = sum(TP_Load, na.omit = TRUE),
            Sum_Nitrate = sum(Nitrate_Load, na.omit = TRUE),
            Sum_SRP = sum(SRP_Load, na.omit = TRUE),
            Sum_DOC = sum(DOC_Load, na.omit = TRUE))
