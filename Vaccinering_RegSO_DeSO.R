# visualisering av data från Nationella vaccinationsregistret
library(sjmisc)
library(sf)
library(viridis)
library(htmlTable)
# test




dt1 <- read.csv(file = "E:/Filer/admgumjon/NVR_Deso/Dalarna täckning per DeSo 210705.csv", sep=";", header=TRUE, skip=1) %>%
  select(1:5, X.65.år, X65.plus) %>%
  rename(Deso = Radetiketter,
         minst.en.dos.0_64 = minst.en.dos,
         två.doser.0_64 = X2.doser,
         minst.en.dos.65plus = minst.en.dos.1,
         två.doser.65plus = X2.doser.1,
         totalt.0_64 = X.65.år,
         totalt.65plus = X65.plus) %>%
  pivot_longer(cols = 2:7, names_to = "Variabel", values_to = "Antal") %>%
  rowwise() %>%
  mutate(Ålder = ifelse(str_contains(Variabel, "65"), "65+", "<65"),
          Dos = ifelse(str_contains(Variabel, "totalt"), "totalt", ifelse(str_contains(Variabel, "minst"), "Minst 1 dos", "2 doser"))) %>%
  pivot_wider(id_cols = c(Deso, Ålder), names_from = Dos, values_from = Antal) %>%
  mutate(`Minst 1 dos, andel` = `Minst 1 dos` / totalt,
         `2 doser, andel` = `2 doser` / totalt)

  
dt2 <- read.csv(file = "E:/Filer/admgumjon/NVR_Deso/Dalarna_vaccinationstackning_DeSO_NVR_SCB_2021-09-03.csv", sep=";", header=TRUE, skip=1) %>% #head(5) %>%
  rename(två.doser = X2.doser..,
         minst.en.dos = Minst.1.dos..,
         Deso  =DeSO.kod) %>%
  separate(två.doser, sep = "-", into = c("två.doser.lower", "två.doser.higher"), remove = FALSE) %>%
  separate(minst.en.dos, sep = "-", into = c("minst.en.dos.lower", "minst.en.dos.higher"), remove = FALSE) %>%
  mutate(minst.en.dos1 = fct_reorder(minst.en.dos, desc(minst.en.dos.lower))) %>%
  mutate(period = as.POSIXct("2021-09-03"))%>%
  mutate(minst.en.dos.lower = as.integer(minst.en.dos.lower))
  
dt2


  dt3 <- read.csv(file = "E:/Filer/admgumjon/NVR_Deso/Dalarna_vaccinationstackning_DeSO_NVR_SCB_2021-09-29.csv", sep=";", header=TRUE, skip=1) %>%
    rename(två.doser = X2.doser..,
           minst.en.dos = Minst.1.dos..,
           Deso  =DeSO.kod) %>%
    #mutate(två.doser.split = två.doser, minst.en.dos.split = minst.en.dos) %>%
    separate(två.doser, sep = "-", into = c("två.doser.lower", "två.doser.higher"), remove = FALSE) %>%
    separate(minst.en.dos, sep = "-", into = c("minst.en.dos.lower", "minst.en.dos.higher"), remove = FALSE) %>%
    mutate(minst.en.dos1 = fct_reorder(minst.en.dos, desc(minst.en.dos.lower))) %>%
    mutate(period = as.POSIXct("2021-09-29")) %>%
    mutate(minst.en.dos.lower = as.integer(minst.en.dos.lower))
    #head(5)
  dt3
  
  #dt <- rbind(dt2, dt3)
  
  dt <- dt2 %>%
    select(Deso, Ålder, minst.en.dos.lower) %>%
    rename(minst.en.dos.lower.fg = minst.en.dos.lower) %>%
    inner_join(dt3 %>% select(Deso, Ålder, minst.en.dos.lower), by = c("Deso" = "Deso", "Ålder" = "Ålder")) %>%
    mutate(FörändringPct = minst.en.dos.lower - minst.en.dos.lower.fg)
  
  ################## ladda kommunkartor #################
  
  sf_kommuner_dalarna <- st_read(dsn = "E:/Filer/admgumjon/Kommungränser_Dalarna") %>%
    st_set_crs(3006) %>%
    mutate(KOMMUNNAMN = iconv(KOMMUNNAMN, "1252", "UTF-8"))
  
  
  
  ################## ladda deso #################
  sf_deso_dalarna <- st_read(dsn = "E:/Filer/admgumjon/deso_shp_fixed") %>%
    st_set_crs(3006) %>%
    filter(startsWith(Deso, "20"))
  
  
  #sf_deso_dalarna = sf_deso %>% st_join(sf_kommuner_dalarna, st_join = within, left = FALSE)

  sf_deso_dalarna %>%
      inner_join(dt1, by=c("Deso" = "Deso")) %>%
  ggplot() + 
    geom_sf(aes(fill =`Minst 1 dos, andel`))+  
    geom_sf(data = sf_kommuner_dalarna, fill = NA, color = "black", linetype = "dashed")+
    scale_fill_viridis_c(option = "plasma", direction=1)+
    theme_minimal()+
    theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
    theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    facet_grid(~Ålder)
  
  
  
  sf_deso_dalarna %>%
    inner_join(dt3, by=c("Deso" = "Deso")) %>%
    select(Deso, Ålder, två.doser, minst.en.dos) %>%
    pivot_longer(cols = c("två.doser", "minst.en.dos"), names_to = "Doser", values_to = "Andel") %>%
    ggplot() + 
    geom_sf(aes(fill = Andel))+  
    geom_sf(data = sf_kommuner_dalarna, fill = NA, color = "black", linetype = "dashed")+
    scale_fill_viridis_d(option = "plasma", direction=1)+
    theme_minimal()+
    theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
    theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    facet_wrap(~Ålder+Doser)
  

  
  
  grid.arrange(p1,p2, )
  # diffplot
  
  sf_deso_dalarna %>%
    inner_join(dt, by=c("Deso" = "Deso")) %>%
    ggplot() + 
    geom_sf(aes(fill = FörändringPct))+  
    geom_sf(data = sf_kommuner_dalarna, fill = NA, color = "black", linetype = "dashed")+
    scale_fill_viridis_c(option = "plasma", direction=1)+
    theme_minimal()+
    theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
    theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    facet_grid(~Ålder)
  
  dt %>% 
    select(Deso, Ålder, FörändringPct) %>%
    pivot_wider(id_cols = Deso, names_from = Ålder, values_from = FörändringPct) %>%
    arrange(desc(`18-64`)) %>%
    tidyHtmlTable(cgroup = Ålder,rnames = Deso)
  
  
  
  
  