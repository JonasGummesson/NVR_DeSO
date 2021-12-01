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

##################### Deso utan �lder ####################################

dt1 <- read.csv(file = "E:/Filer/admgumjon/NVR_Deso/Dalarna_vaccinationstackning_DeSO_NVR_SCB_2021-09-03_PerDeso.csv", sep=";", header=TRUE, skip=1) %>% #head(5) %>%
  rename(minst.tv�.doser = X2.doser..,
         minst.en.dos = Minst.1.dos..,
         Deso  =DeSO.kod) %>%
  mutate(Period = as.POSIXct("2021-09-03"))
dt2 <- read.csv(file = "E:/Filer/admgumjon/NVR_Deso/Dalarna_vaccinationstackning_DeSO_NVR_SCB_2021-09-29_PerDeso.csv", sep=";", header=TRUE, skip=1) %>%
  rename(minst.tv�.doser = X2.doser..,
         minst.en.dos = Minst.1.dos..,
         Deso  =DeSO.kod) %>%
  mutate(Period = as.POSIXct("2021-09-29"))
dt3 <- read.csv(file = "E:/Filer/admgumjon/NVR_Deso/Dalarna_vaccinationstackning_DeSO_NVR_SCB_2021-10-26_PerDeso.csv", sep=";", header=TRUE, skip=1) %>%
  rename(minst.tv�.doser = Minst.2.doser..,
         minst.en.dos = Minst.1.dos..,
         Deso  = DeSO.kod) %>%
  mutate(Period = as.POSIXct("2021-10-26"))


dbWriteTable(conn_analys, name = Id(schema = "dbo", table = "NVR_RegSO_Deso"), value = dt1, overwrite=TRUE, append = FALSE)
dbWriteTable(conn_analys, name = Id(schema = "dbo", table = "NVR_RegSO_Deso"), value = dt2, overwrite=FALSE, append = TRUE)
dbWriteTable(conn_analys, name = Id(schema = "dbo", table = "NVR_RegSO_Deso"), value = dt3, overwrite=FALSE, append = TRUE)


