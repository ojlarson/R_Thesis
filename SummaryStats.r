WQSumMean <- WQ %>%
  group_by(Site) %>%
  summarise(DOC_mean = mean(DOC, na.rm = TRUE),
            TN_mean = mean(TN, na.rm = TRUE),
            TP_mean = mean(TP, na.rm = TRUE),
            Nitrate_mean = mean(Nitrate, na.rm = TRUE),
            SRP_mean = mean(SRP, na.rm = TRUE)) 

WQ_717 <- WQ %>%
  #mutate(Site = fct_reorder(Site, Nitrate)) %>%
  filter(Date == WQ$Date[38])

WQ_717 %>% ggplot(aes(Site, Nitrate)) +
  geom_point()

WQLoadsSumMean <- WQLoads %>%
  group_by(Site) %>%
  summarise(DOC_mean = mean(DOC_Load, na.rm = TRUE),
            TN_mean = mean(TN_Load, na.rm = TRUE),
            TP_mean = mean(TP_Load, na.rm = TRUE),
            Nitrate_mean = mean(Nitrate_Load, na.rm = TRUE),
            SRP_mean = mean(SRP_Load, na.rm = TRUE)) 


WQLoadsSumMean %>%
  mutate(Site = fct_reorder(Site, SRP_mean)) %>%
  ggplot(aes(Site, SRP_mean)) +
  geom_point()
