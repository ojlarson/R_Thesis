library(readr)
library(tidyverse)
Eqs <- read.csv("Equations.csv", fileEncoding = 'UTF-8-BOM')
Q_MSMTS <- read.csv("Q_MSMTS.csv", fileEncoding = 'UTF-8-BOM')
Sample_Stages <- read.csv("Sample_Date_Stages.csv", fileEncoding = 'UTF-8-BOM')

Sample_Q <- data.frame(matrix(ncol=4, nrow=0))
colnames(Sample_Q) <- c('Date.Time', 'Stream', 'Stage', 'Q')


#--------------Four Mile--------------

# Create Stream-specific dataframes
FMS_Stage <- Sample_Stages %>%
  select(1,2) %>%
  rename(Stage = FMS)
FMS_Eqs <- Eqs %>%
  filter(Stream == "FM")

#Set up categories for using different equations
FMS_Breaks = as.numeric(unlist(FMS_Eqs[2]))
FMS_Stage$category <- cut(FMS_Stage$Stage,
                          breaks = c(-Inf, FMS_Breaks),
                          labels = c("low", "med", "med-high", "high"))

#Create dataframes for each equation
FMSLow <- FMS_Stage %>%
  filter(category == "low")
FMSMed <- FMS_Stage %>%
  filter(category == "med")
FMSMedHigh <- FMS_Stage %>%
  filter(category == "med-high")
FMSHigh <- FMS_Stage %>%
  filter(category == "high")

#Create Q column, fill using stage and equation
FMSLow$Q = ((FMSLow$Stage - FMS_Eqs$Intercept[1]) / FMS_Eqs$x[1])
FMSMed$Q = exp(((FMSMed$Stage - FMS_Eqs$Intercept[2]) / FMS_Eqs$x[2]))
FMSMedHigh$Q = ((FMSMedHigh$Stage - FMS_Eqs$Intercept[3]) / FMS_Eqs$x[3])
FMSHigh$Q = ((FMSHigh$Stage - FMS_Eqs$Intercept[4]) / FMS_Eqs$x[4])

#Bind dataframes together
FMSAll <- rbind(FMSLow, FMSMed, FMSMedHigh, FMSHigh) %>%
  select(1,2,4)
FM_Sample <- FMSAll %>%
  mutate(Stream = 'FM') %>%
  relocate(Stage, Q,.after=last_col())
EF_Sample <- FMSAll %>%
  mutate(Stream = 'EF',
         Q = Q * 0.23031) %>%
  relocate(Stage, Q,.after=last_col())
FM_Branch_Sample <- FMSAll %>%
  mutate(Stream = 'FM_Branch',
         Q = Q * 0.76795) %>%
  relocate(Stage, Q,.after=last_col())

#Bring in Q measurement data, tidy and bind with calculated Qs
FM_MSMTS <- Q_MSMTS %>%
  filter(Stream == "FM") %>%
  select(3,4) %>%
  mutate(Type = 'Q Measurement')
FMSAll <- FMSAll %>%
  mutate(Type = 'Sample',
         Date.Time = NULL)
FM_All <- rbind(FMSAll, FM_MSMTS)

#Create and save plot
#ggplot(data = FM_All) +
#  geom_point(mapping = aes(Q, Stage, color = Type)) +
#  labs(title = "FM")
#ggsave("Figs/FM_Stage_Revised.png")

#Clean up workspace by deleting dataframes
rm(FMSLow, FMSMed, FMSHigh, FMSAll, FM_MSMTS, FMS_Breaks, FMS_Stage, FMS_Eqs, FMSMedHigh)

#--------------Little Four Mile--------

# Create Stream-specific dataframes
LFS_Stage <- Sample_Stages %>%
  select(1,3) %>%
  rename(Stage = LFS)
LFS_Eqs <- Eqs %>%
  filter(Stream == "LF")

#Set up categories for using different equations
#LF needs additional category for WinXS modeled data
LFS_Breaks = as.numeric(unlist(LFS_Eqs[2]))
LFS_Stage$category <- cut(LFS_Stage$Stage,
                          breaks = c(-Inf, LFS_Breaks),
                          labels = c("low", "med", "high", "WinXS"))

#Create dataframes for each equation
LFSLow <- LFS_Stage %>%
  filter(category == "low")
LFSMed <- LFS_Stage %>%
  filter(category == "med")
LFSHigh <- LFS_Stage %>%
  filter(category == "high")
LFSWinXS <- LFS_Stage %>%
  filter(category == "WinXS")

#Create Q column, fill using stage and equation
LFSLow$Q = ((LFSLow$Stage - LFS_Eqs$Intercept[1]) / Eqs$x[1])
LFSMed$Q = exp(((LFSMed$Stage - Eqs$Intercept[2]) / Eqs$x[2]))
LFSHigh$Q = ((LFSHigh$Stage - Eqs$Intercept[3]) / Eqs$x[3])
LFSWinXS$Q = (Eqs$Intercept[4])


#Bind dataframes together
LFSAll <- rbind(LFSLow, LFSMed, LFSHigh, LFSWinXS) %>%
  select(1,2,4)
LF_Sample <- LFSAll %>%
  mutate(Stream = 'LF') %>%
  relocate(Stage, Q,.after=last_col())


#Bring in Q measurement data, tidy and bind with calculated Qs
LF_MSMTS <- Q_MSMTS %>%
  filter(Stream == "LF") %>%
  select(3,4) %>%
  mutate(Type = 'Q Measurement')
LFSAll <- LFSAll %>%
  mutate(Type = 'Sample',
         Date.Time = NULL)
LF_All <- rbind(LFSAll, LF_MSMTS)


#ggplot(data = LF_All) +
#  geom_point(mapping = aes(Q, Stage, color = Type)) +
#  labs(title = "LF")
#ggsave("Figs/LF_Stage.png")

rm(LFSLow, LFSMed, LFSHigh, LFSAll, LF_MSMTS, LFS_Breaks, LFS_Stage, LFSWinXS, LFS_Eqs)

#-------------Marshall's branch--------

# Create Stream-specific dataframes
MBS_Stage <- Sample_Stages %>%
  select(1,4) %>%
  rename(Stage = MBS)
MBS_Eqs <- Eqs %>%
  filter(Stream == "MB")

#Set up categories for using different equations
MBS_Breaks = as.numeric(unlist(MBS_Eqs[2]))
MBS_Stage$category <- cut(MBS_Stage$Stage,
                          breaks = c(-Inf, MBS_Breaks),
                          labels = c("low", "med", "high"))

#Create dataframes for each equation
MBSLow <- MBS_Stage %>%
  filter(category == "low")
MBSMed <- MBS_Stage %>%
  filter(category == "med")
MBSHigh <- MBS_Stage %>%
  filter(category == "high")

#Create Q column, fill using stage and equation
MBSLow$Q = ((MBSLow$Stage - MBS_Eqs$Intercept[1]) / MBS_Eqs$x[1])
MBSMed$Q = exp(((MBSMed$Stage - MBS_Eqs$Intercept[2]) / MBS_Eqs$x[2]))
MBSHigh$Q = ((MBSHigh$Stage - MBS_Eqs$Intercept[3]) / MBS_Eqs$x[3])

#Bind dataframes together
MBSAll <- rbind(MBSLow, MBSMed, MBSHigh) %>%
  select(1,2,4)
MB_Sample <- MBSAll %>%
  mutate(Stream = 'MB') %>%
  relocate(Stage, Q,.after=last_col())


#Bring in Q measurement data, tidy and bind with calculated Qs
MB_MSMTS <- Q_MSMTS %>%
  filter(Stream == "MB") %>%
  select(3,4) %>%
  mutate(Type = 'Q Measurement')
MBSAll <- MBSAll %>%
  mutate(Type = 'Sample',
         Date.Time = NULL)
MB_All <- rbind(MBSAll, MB_MSMTS)

#Create plot
#ggplot(data = MB_All) +
#  geom_point(mapping = aes(Q, Stage, color = Type)) +
#  labs(title = "MB")
#ggsave("Figs/MB_Stage.png")

#Clean up workspace by deleting dataframes
rm(MBSLow, MBSMed, MBSHigh, MBSAll, MB_MSMTS, MBS_Breaks, MBS_Stage, MBS_Eqs)

#-----------------Deer's Ear-------
# Create Stream-specific dataframes
DES_Stage <- Sample_Stages %>%
  select(1,5) %>%
  rename(Stage = DE)
DES_Eqs <- Eqs %>%
  filter(Stream == "DE")

#Set up categories for using different equations
DES_Breaks = as.numeric(unlist(DES_Eqs[2]))
DES_Stage$category <- cut(DES_Stage$Stage,
                          breaks = c(-Inf, DES_Breaks),
                          labels = c("low", "med", "high"))

#Create dataframes for each equation
DESLow <- DES_Stage %>%
  filter(category == "low")
DESMed <- DES_Stage %>%
  filter(category == "med")
DESHigh <- DES_Stage %>%
  filter(category == "high")

#Create Q column, fill using stage and equation
DESLow$Q = exp(((DESLow$Stage - DES_Eqs$Intercept[1]) / DES_Eqs$x[1]))
DESMed$Q = ((DESMed$Stage - DES_Eqs$Intercept[2]) / DES_Eqs$x[2])
DESHigh$Q = ((DESHigh$Stage - DES_Eqs$Intercept[3]) / DES_Eqs$x[3])

#Bind dataframes together
DESAll <- rbind(DESLow, DESMed, DESHigh) %>%
  select(1,2,4)
DE_Sample <- DESAll %>%
  mutate(Stream = 'DE') %>%
  relocate(Stage, Q,.after=last_col())

#Bring in Q measurement data, tidy and bind with calculated Qs
DE_MSMTS <- Q_MSMTS %>%
  filter(Stream == "DE") %>%
  select(3,4) %>%
  mutate(Type = 'Q Measurement')
DESAll <- DESAll %>%
  mutate(Type = 'Sample',
         Date.Time = NULL)
DE_All <- rbind(DESAll, DE_MSMTS)

#Create plot
#ggplot(data = DE_All) +
#  geom_point(mapping = aes(Q, Stage, color = Type)) +
#  labs(title = "DE")
#ggsave("Figs/DE_Stage.png")

#Clean up workspace by deleting dataframes
rm(DESLow, DESMed, DESHigh, DESAll, DE_MSMTS, DES_Breaks, DES_Stage, DES_Eqs)


#--------------Below Dam--------------

# Create Stream-specific dataframes
BDS_Stage <- Sample_Stages %>%
  select(1,6) %>%
  rename(Stage = BD)
BDS_Eqs <- Eqs %>%
  filter(Stream == "BD")

#Set up categories for using different equations
BDS_Breaks = as.numeric(unlist(BDS_Eqs[2]))
BDS_Stage$category <- cut(BDS_Stage$Stage,
                          breaks = c(-Inf, BDS_Breaks),
                          labels = c("low", "med", "high"))

#Create dataframes for each equation
BDSLow <- BDS_Stage %>%
  filter(category == "low")
BDSMed <- BDS_Stage %>%
  filter(category == "med")
BDSHigh <- BDS_Stage %>%
  filter(category == "high")

#Create Q column, fill using stage and equation
BDSLow$Q = ((BDSLow$Stage - BDS_Eqs$Intercept[1]) * BDS_Eqs$x[1])
BDSMed$Q = exp(((BDSMed$Stage - BDS_Eqs$Intercept[2]) / BDS_Eqs$x[2]))
BDSHigh$Q = ((BDSHigh$Stage - BDS_Eqs$Intercept[3]) / BDS_Eqs$x[3])

#Bind dataframes together
BDSAll <- rbind(BDSLow, BDSMed, BDSHigh) %>%
  select(1,2,4)
BD_Sample <- BDSAll %>%
  mutate(Stream = 'BD') %>%
  relocate(Stage, Q,.after=last_col())

#Bring in Q measurement data, tidy and bind with calculated Qs
BD_MSMTS <- Q_MSMTS %>%
  filter(Stream == "BD") %>%
  select(3,4) %>%
  mutate(Type = 'Q Measurement')
BDSAll <- BDSAll %>%
  mutate(Type = 'Sample',
         Date.Time = NULL)
BD_All <- rbind(BDSAll, BD_MSMTS)

#Create and save plot
#ggplot(data = BD_All) +
#  geom_point(mapping = aes(Q, Stage, color = Type)) +
#  labs(title = "BD")
#ggsave("Figs/BD_Stage.png")

#Clean up workspace by deleting dataframes
rm(BDSLow, BDSMed, BDSHigh, BDSAll, BD_MSMTS, BDS_Breaks, BDS_Stage, BDS_Eqs)

##-----------------------

#Export Sample Qs to full dataset
Sample_Q <- rbind(FM_Sample, LF_Sample, MB_Sample, DE_Sample, BD_Sample, EF_Sample, FM_Branch_Sample)

FM_Wide <- FM_Sample %>%
  rename(FMQ = Q) %>%
  select(1,4)
EF_Wide <- EF_Sample %>%
  rename(EFQ = Q) %>%
  select(1,4)
FM_Branch_Wide <- FM_Branch_Sample %>%
  rename(FM_BranchQ = Q) %>%
  select(1,4)
LF_Wide <- LF_Sample %>%
  rename(LFQ = Q) %>%
  select(1,4)
MB_Wide <- MB_Sample %>%
  rename(MBQ = Q) %>%
  select(1,4)
DE_Wide <- DE_Sample %>%
  rename(DEQ = Q) %>%
  select(1,4)
BD_Wide <- BD_Sample %>%
  rename(BDQ = Q) %>%
  select(1,4)

Sample_Q_Wide_List <- list(FM_Wide, EF_Wide, FM_Branch_Wide, LF_Wide, MB_Wide, DE_Wide, BD_Wide)

Sample_Q_Wide <- Sample_Q_Wide_List %>% reduce(full_join, by='Date.Time') %>%
  mutate(DEQ = if_else(is.na(DEQ), MBQ * .754799, DEQ),
         LFQ = if_else(is.na(LFQ), FMQ * .604106, LFQ),
         Date.Time=as.POSIXct(Date.Time, format = "%m/%d/%Y %H:%M", tz=Sys.timezone())) %>%
  arrange(Date.Time) %>%
  filter(Date.Time != "2019-12-08 09:59:00")

Sample_Q_Long <- Sample_Q_Wide
colnames(Sample_Q_Long) <- (c("Date.Time", "FM", "EF", "FM_Branch", "LF" , "MB", "DE", "BD")) #%>%
Sample_Q_Long <- pivot_longer(data = Sample_Q_Long, cols = 2:8, names_to = "Stream", values_to = "Q") %>%
  arrange(Stream)

rm(FM_Sample, LF_Sample, MB_Sample, DE_Sample, BD_Sample, EF_Sample, FM_Branch_Sample,
   BD_All, DE_All, FM_All, LF_All, MB_All, Eqs, Q_MSMTS, Sample_Stages,
   Sample_Q_Wide_List, FM_Wide, EF_Wide, FM_Branch_Wide, LF_Wide, MB_Wide, DE_Wide, BD_Wide)

#Export Sample_Q to csv
write.csv(Sample_Q_Long, 'Sample_Q_Long.csv')
write.csv(Sample_Q_Wide, 'Sample_Q_Wide.csv')

