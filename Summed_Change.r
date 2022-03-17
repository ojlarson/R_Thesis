WQLchange <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(WQLchange) <- c('Date', 'Stream', 'DTN', 'DTP', 'DNitrate', 'DSRP', 'DDOC')

DFM <- filter(WQLoads, Site %in% c('8', '11')) %>%
  group_by(Date) %>%
  arrange(desc(Site)) %>%
  summarize(DTN = diff(TN_Load),
            DTP = diff(TP_Load),
            DNitrate = diff(Nitrate_Load),
            DSRP = diff(SRP_Load),
            DDOC = diff(DOC_Load))

DLF <- filter(WQLoads, Site %in% c('2', '5')) %>%
  group_by(Date) %>%
  arrange(desc(Site)) %>%
  summarize(DTN = diff(TN_Load),
            DTP = diff(TP_Load),
            DNitrate = diff(Nitrate_Load),
            DSRP = diff(SRP_Load),
            DDOC = diff(DOC_Load))
DMB <- filter(WQLoads, Site %in% c('16', '19')) %>%
  group_by(Date) %>%
  arrange(desc(Site)) %>%
  summarize(DTN = diff(TN_Load),
            DTP = diff(TP_Load),
            DNitrate = diff(Nitrate_Load),
            DSRP = diff(SRP_Load),
            DDOC = diff(DOC_Load))
DDE <- filter(WQLoads, Site %in% c('22', '24')) %>%
  group_by(Date) %>%
  arrange(desc(Site)) %>%
  summarize(DTN = diff(TN_Load),
            DTP = diff(TP_Load),
            DNitrate = diff(Nitrate_Load),
            DSRP = diff(SRP_Load),
            DDOC = diff(DOC_Load))
DStreams <- rbind(DFM, DLF, DMB, DDE) %>%
  group_by(Date) %>%
  summarise(DTN = sum(DTN),
            DTP = sum(DTP),
            DNitrate = sum(DNitrate),
            DSRP = sum(DSRP),
            DDOC = sum(DDOC))
ggplot(DStreams, aes(Date, DNitrate)) +
  geom_point()
