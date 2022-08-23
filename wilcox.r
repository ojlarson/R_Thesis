# Stats
library(tidyverse)
library(rstatix)
TN_Comp_Tidy <- EMC_Comp %>%
  select(1,2,7,13) %>%
  filter(Date != "2019-12-08") %>%
  pivot_longer(cols = 2:4, values_to = "EMC")
TN_Stats_Stream <- TN_Comp_Tidy %>%
  filter(name != "BD_TN")
TN_Stats_Lake <- TN_Comp_Tidy %>%
  filter(name != "PB_TN")
TN_Stats_Total <- TN_Comp_Tidy %>%
  filter(name != "LB_TN")
wilcox.TN.stream <- TN_Stats_Stream  %>%
  wilcox_test(EMC ~ name, paired = TRUE) %>%
  add_significance()
wilcox.TN.lake <- TN_Stats_Lake  %>%
  wilcox_test(EMC ~ name, paired = TRUE) %>%
  add_significance()
wilcox.TN.total <- TN_Stats_Total  %>%
  wilcox_test(EMC ~ name, paired = TRUE) %>%
  add_significance()
wilcox.TN.change <- TN_Comp %>%
  wilcox_test(`EMC Total Change` ~ Type, paired = TRUE) %>%
  add_significance()


TP_Comp_Tidy <- EMC_Comp %>%
  select(1,3,8,14) %>%
  filter(Date != "2019-12-08") %>%
  pivot_longer(cols = 2:4, values_to = "EMC")
TP_Stats_Stream <- TP_Comp_Tidy %>%
  filter(name != "BD_TP")
TP_Stats_Lake <- TP_Comp_Tidy %>%
  filter(name != "PB_TP")
TP_Stats_Total <- TP_Comp_Tidy %>%
  filter(name != "LB_TP")
wilcox.TP.stream <- TP_Stats_Stream  %>%
  wilcox_test(EMC ~ name, paired = TRUE) %>%
  add_significance()
wilcox.TP.lake <- TP_Stats_Lake  %>%
  wilcox_test(EMC ~ name, paired = TRUE) %>%
  add_significance()
wilcox.TP.total <- TP_Stats_Total  %>%
  wilcox_test(EMC ~ name, paired = TRUE) %>%
  add_significance()
wilcox.TP.change <- TP_Comp %>%
  wilcox_test(`EMC Total Change` ~ Type, paired = TRUE) %>%
  add_significance()


Nitrate_Comp_Tidy <- EMC_Comp %>%
  select(1,4,9,15) %>%
  filter(Date != "2019-12-08") %>%
  pivot_longer(cols = 2:4, values_to = "EMC")
Nitrate_Stats_Stream <- Nitrate_Comp_Tidy %>%
  filter(name != "BD_Nitrate")
Nitrate_Stats_Lake <- Nitrate_Comp_Tidy %>%
  filter(name != "PB_Nitrate")
Nitrate_Stats_Total <- Nitrate_Comp_Tidy %>%
  filter(name != "LB_Nitrate")
wilcox.Nitrate.stream <- Nitrate_Stats_Stream  %>%
  wilcox_test(EMC ~ name, paired = TRUE) %>%
  add_significance()
wilcox.Nitrate.lake <- Nitrate_Stats_Lake  %>%
  wilcox_test(EMC ~ name, paired = TRUE) %>%
  add_significance()
wilcox.Nitrate.total <- Nitrate_Stats_Total  %>%
  wilcox_test(EMC ~ name, paired = TRUE) %>%
  add_significance()
wilcox.Nitrate.change <- Nitrate_Comp %>%
  wilcox_test(`EMC Total Change` ~ Type, paired = TRUE) %>%
  add_significance()

SRP_Comp_Tidy <- EMC_Comp %>%
  select(1,5,10,16) %>%
  filter(Date != "2019-12-08") %>%
  pivot_longer(cols = 2:4, values_to = "EMC")
SRP_Stats_Stream <- SRP_Comp_Tidy %>%
  filter(name != "BD_SRP")
SRP_Stats_Lake <- SRP_Comp_Tidy %>%
  filter(name != "PB_SRP")
SRP_Stats_Total <- SRP_Comp_Tidy %>%
  filter(name != "LB_SRP")
wilcox.SRP.stream <- SRP_Stats_Stream  %>%
  wilcox_test(EMC ~ name, paired = TRUE) %>%
  add_significance()
wilcox.SRP.lake <- SRP_Stats_Lake  %>%
  wilcox_test(EMC ~ name, paired = TRUE) %>%
  add_significance()
wilcox.SRP.total <- SRP_Stats_Total  %>%
  wilcox_test(EMC ~ name, paired = TRUE) %>%
  add_significance()
wilcox.SRP.change <- SRP_Comp %>%
  wilcox_test(`EMC Total Change` ~ Type, paired = TRUE) %>%
  add_significance()

p_vals <- c(wilcox.TN.stream$p,
            wilcox.TN.lake$p,
            wilcox.TN.total$p,
            wilcox.TN.change$p,
            wilcox.TP.stream$p,
            wilcox.TP.lake$p,
            wilcox.TP.total$p,
            wilcox.TP.change$p,
            wilcox.Nitrate.stream$p,
            wilcox.Nitrate.lake$p,
            wilcox.Nitrate.total$p,
            wilcox.Nitrate.change$p,
            wilcox.SRP.stream$p,
            wilcox.SRP.lake$p,
            wilcox.SRP.total$p,
            wilcox.SRP.change$p)
p_sig <- c(wilcox.TN.stream$p.signif,
            wilcox.TN.lake$p.signif,
            wilcox.TN.total$p.signif,
            wilcox.TN.change$p.signif,
            wilcox.TP.stream$p.signif,
            wilcox.TP.lake$p.signif,
            wilcox.TP.total$p.signif,
            wilcox.TP.change$p.signif,
            wilcox.Nitrate.stream$p.signif,
            wilcox.Nitrate.lake$p.signif,
            wilcox.Nitrate.total$p.signif,
            wilcox.Nitrate.change$p.signif,
            wilcox.SRP.stream$p.signif,
            wilcox.SRP.lake$p.signif,
            wilcox.SRP.total$p.signif,
            wilcox.SRP.change$p.signif)
p_names <- c('wilcox.TN.stream',
             'wilcox.TN.lake',
             'wilcox.TN.total',
             'wilcox.TN.change',
             'wilcox.TP.stream',
             'wilcox.TP.lake',
             'wilcox.TP.total',
             'wilcox.TP.change',
             'wilcox.Nitrate.stream',
             'wilcox.Nitrate.lake',
             'wilcox.Nitrate.total',
             'wilcox.Nitrate.change',
             'wilcox.SRP.stream',
             'wilcox.SRP.lake',
             'wilcox.SRP.total',
             'wilcox.SRP.change')
p_tests <- data.frame(Test = c(p_names), P_Value = c(p_vals), Significance = c(p_sig))
view(p_tests)
write.csv(p_tests, "p_tests.csv", row.names = FALSE)

