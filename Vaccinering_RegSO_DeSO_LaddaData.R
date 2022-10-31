library(sjmisc)
library(sf)
library(viridis)
library(htmlTable)
library(shiny)
library(kableExtra)
library(tidyverse)
library(data.table)
library(stringi)
library(ggrepel)
library(DBI)
library(odbc)

conn_analys <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "analys.ltdalarna.se", Database = "Analys", Trusted_Connection = "True", Encoding = "windows-1252")

##################### Deso utan ålder ####################################

dt1 <- read.csv(file = "E:/Filer/admgumjon/NVR_Deso/Dalarna_vaccinationstackning_DeSO_NVR_SCB_2021-09-03_PerDeso1.csv", sep=";", header=TRUE, skip=1) %>% #head(5) %>%
  rename(minst.två.doser = X2.doser..,
         minst.en.dos = Minst.1.dos..,
         minst.tre.dos = Minst.3.doser..,
         minst.fyra.dos = Minst.4.doser..,
         minst.fem.dos = Minst.5.doser..,
         Deso  =DeSO.kod) %>% 
  mutate(Period = as.POSIXct("2021-09-03"))
dt2 <- read.csv(file = "E:/Filer/admgumjon/NVR_Deso/Dalarna_vaccinationstackning_DeSO_NVR_SCB_2021-09-29_PerDeso1.csv", sep=";", header=TRUE, skip=1) %>%
  rename(minst.två.doser = X2.doser..,
         minst.en.dos = Minst.1.dos..,
         minst.tre.dos = Minst.3.doser..,
         minst.fyra.dos = Minst.4.doser..,
         minst.fem.dos = Minst.5.doser..,
         Deso  =DeSO.kod) %>%
  mutate(Period = as.POSIXct("2021-09-29"))
dt3 <- read.csv(file = "E:/Filer/admgumjon/NVR_Deso/Dalarna_vaccinationstackning_DeSO_NVR_SCB_2021-10-26_PerDeso1.csv", sep=";", header=TRUE, skip=1) %>%
  rename(minst.två.doser = Minst.2.doser..,
         minst.en.dos = Minst.1.dos..,
         minst.tre.dos = Minst.3.doser..,
         minst.fyra.dos = Minst.4.doser..,
         minst.fem.dos = Minst.5.doser..,
         Deso  = DeSO.kod) %>%
  mutate(Period = as.POSIXct("2021-10-26"))
dt4 <- read.csv(file = "E:/Filer/admgumjon/NVR_Deso/Dalarna_vaccinationstackning_DeSO_NVR_SCB_2021-11-24_PerDeso1.csv", sep=";", header=TRUE, skip=1) %>%
  rename(minst.två.doser = Minst.2.doser..,
         minst.en.dos = Minst.1.dos..,
         minst.tre.dos = Minst.3.doser..,
         minst.fyra.dos = Minst.4.doser..,
         minst.fem.dos = Minst.5.doser..,
         Deso  = DeSO.kod) %>%
  mutate(Period = as.POSIXct("2021-11-24"))
dt5 <- read.csv(file = "E:/Filer/admgumjon/NVR_Deso/Dalarna_vaccinationstackning_DeSO_NVR_SCB_2021-12-21_PerDeso1.csv", sep=";", header=TRUE, skip=1) %>%
  rename(minst.två.doser = Minst.2.doser..,
         minst.en.dos = Minst.1.dos..,
         minst.tre.dos = Minst.3.doser..,
         minst.fyra.dos = Minst.4.doser..,
         minst.fem.dos = Minst.5.doser..,
         Deso  = DeSO.kod) %>%
  mutate(Period = as.POSIXct("2021-12-21"))
dt6 <- read.csv(file = "E:/Filer/admgumjon/NVR_Deso/Dalarna_vaccinationstackning_DeSO_NVR_SCB_2022-01-18_PerDeso1.csv", sep=";", header=TRUE, skip=1) %>%
  rename(minst.två.doser = Minst.2.doser..,
         minst.en.dos = Minst.1.dos..,
         minst.tre.dos = Minst.3.doser..,
         minst.fyra.dos = Minst.4.doser..,
         minst.fem.dos = Minst.5.doser..,
         Deso  = DeSO.kod) %>%
  mutate(Period = as.POSIXct("2022-01-18"))
dt7 <- read.csv(file = "E:/Filer/admgumjon/NVR_Deso/Dalarna_vaccinationstackning_DeSO_NVR_SCB_2022-02-17_PerDeso1.csv", sep=";", header=TRUE, skip=1) %>%
  rename(minst.två.doser = Minst.2.doser..,
         minst.en.dos = Minst.1.dos..,
         minst.tre.dos = Minst.3.doser..,
         minst.fyra.dos = Minst.4.doser..,
         minst.fem.dos = Minst.5.doser..,
         Deso  = DeSO.kod) %>%
  mutate(Period = as.POSIXct("2022-02-17"))
dt8 <- read.csv(file = "E:/Filer/admgumjon/NVR_Deso/Dalarna_vaccinationstackning_DeSO_NVR_SCB_2022-04-12_PerDeso1.csv", sep=";", header=TRUE, skip=1) %>%
  rename(minst.två.doser = Minst.2.doser..,
         minst.en.dos = Minst.1.dos..,
         minst.tre.dos = Minst.3.doser..,
         minst.fyra.dos = Minst.4.doser..,
         minst.fem.dos = Minst.5.doser..,
         Deso  = DeSO.kod) %>%
  mutate(Period = as.POSIXct("2022-04-12"))
dt9 <- read.csv(file = "E:/Filer/admgumjon/NVR_Deso/Dalarna_vaccinationstackning_DeSO_NVR_SCB_2022-10-18_PerDeso1.csv", sep=";", header=TRUE, skip=1) %>%
  rename(minst.två.doser = Minst.2.doser..,
         minst.en.dos = Minst.1.dos..,
         minst.tre.dos = Minst.3.doser..,
         minst.fyra.dos = Minst.4.doser..,
         minst.fem.dos = Minst.5.doser..,
         Deso  = DeSO.kod) %>%
  mutate(Period = as.POSIXct("2022-10-18"))

# dbWriteTable(conn_analys, name = Id(schema = "dbo", table = "NVR_RegSO_Deso"), value = dt1[,c(1,3,2,4:7)], overwrite=TRUE, append = FALSE)
# dbWriteTable(conn_analys, name = Id(schema = "dbo", table = "NVR_RegSO_Deso"), value = dt2[,c(1,3,2,4:7)], overwrite=FALSE, append = TRUE)
# dbWriteTable(conn_analys, name = Id(schema = "dbo", table = "NVR_RegSO_Deso"), value = dt3[,c(1,3,2,4:7)], overwrite=FALSE, append = TRUE)
# dbWriteTable(conn_analys, name = Id(schema = "dbo", table = "NVR_RegSO_Deso"), value = dt4[,c(1,3,2,4:7)], overwrite=FALSE, append = TRUE)
# dbWriteTable(conn_analys, name = Id(schema = "dbo", table = "NVR_RegSO_Deso"), value = dt5, overwrite=FALSE, append = TRUE)
# dbWriteTable(conn_analys, name = Id(schema = "dbo", table = "NVR_RegSO_Deso"), value = dt6, overwrite=TRUE, append = TRUE)
# dbWriteTable(conn_analys, name = Id(schema = "dbo", table = "NVR_RegSO_Deso"), value = dt7, overwrite=TRUE, append = TRUE)
# dbWriteTable(conn_analys, name = Id(schema = "dbo", table = "NVR_RegSO_Deso"), value = dt8, overwrite=TRUE, append = TRUE)
# dbWriteTable(conn_analys, name = Id(schema = "dbo", table = "NVR_RegSO_Deso"), value = dt9, overwrite=TRUE, append = TRUE)

z <- rbind(dt1[,c(1,3,2,4:7)],
           dt2[,c(1,3,2,4:7)],
           dt3[,c(1,3,2,4:7)],
           dt4[,c(1,3,2,4:7)],
           dt5,dt6,dt7,dt8,dt9)

dbWriteTable(conn_analys, name = Id(schema = "dbo", table = "NVR_RegSO_Deso"), value = z, overwrite=TRUE, append = FALSE)

colnames(z)
