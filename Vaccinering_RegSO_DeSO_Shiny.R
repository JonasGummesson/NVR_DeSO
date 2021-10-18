# visualisering av data från Nationella vaccinationsregistret
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

dt1 <- read.csv(file = "E:/Filer/admgumjon/NVR_Deso/Dalarna_vaccinationstackning_DeSO_NVR_SCB_2021-09-03.csv", sep=";", header=TRUE, skip=1) %>% #head(5) %>%
  rename(två.doser = X2.doser..,
         minst.en.dos = Minst.1.dos..,
         Deso  =DeSO.kod) %>%
  mutate(Period = as.POSIXct("2021-09-03"))




dt2 <- read.csv(file = "E:/Filer/admgumjon/NVR_Deso/Dalarna_vaccinationstackning_DeSO_NVR_SCB_2021-09-29.csv", sep=";", header=TRUE, skip=1) %>%
  rename(två.doser = X2.doser..,
         minst.en.dos = Minst.1.dos..,
         Deso  =DeSO.kod) %>%
  mutate(Period = as.POSIXct("2021-09-29"))


# union datasets
dt <- 
  rbind(
    dt1 %>% select(Period, Deso, Ålder, minst.en.dos, två.doser),
    dt2 %>% select(Period, Deso, Ålder, minst.en.dos, två.doser)
  ) %>%
  pivot_longer(cols = c(minst.en.dos, två.doser), names_to = "Doser", values_to = "Intervall") %>%
  separate(Intervall, sep = "-", into = c("Intervall.låg", "Intervall.hög"), remove = FALSE) %>%
  mutate(Intervall.låg = as.integer(Intervall.låg),
         Intervall.hög = as.integer(Intervall.hög)) %>%
  group_by(Deso, Ålder, Doser) %>%
  mutate(Intervall.låg.fg = lag(Intervall.låg, n = 1, order_by = Period)) %>%
  mutate(Skillnad.fg.månad = Intervall.låg - Intervall.låg.fg) 

#dt %>% filter(Period == as.POSIXct("2021-09-29"))

################## ladda kommunkartor #################

sf_kommuner_dalarna <- st_read(dsn = "E:/Filer/admgumjon/Kommungränser_Dalarna") %>%
  st_set_crs(3006) %>%
  mutate(KOMMUNNAMN = iconv(KOMMUNNAMN, "1252", "UTF-8"))


################## Ladda DeSO #################
sf_deso_dalarna <- st_read(dsn = "E:/Filer/admgumjon/deso_shp_fixed") %>%
  st_set_crs(3006) %>%
  filter(startsWith(Deso, "20")) %>%
  mutate(Kommunkod = as.integer(substring(Deso, 1, 4)))

################## Ladda RegSO #################
sf_regso_dalarna <- st_read(dsn = "E:/Filer/admgumjon/Kartor/RegSO_2018") %>%
  st_set_crs(3006) %>%
  filter(startsWith(kommun, "20")) %>%
  rename(RegSO = regso) %>%
  rename(Kommunkod = kommun) %>%
  mutate(Kommunkod = as.integer(Kommunkod)) %>%
  mutate(RegSO = str_trim(RegSO))

################## Koppling RegSO/DeSO #################
dt_koppling <- read.csv("E:/Filer/admgumjon/Kartor/kopplingstabell-deso-regso-20210702-v2.csv", skip=3, sep=";") %>% #head(15) %>%
  rename(Kommunkod = bro) %>%
  select(Kommunkod, Kommunnamn, DeSO, RegSO)%>%
  mutate(RegSO = str_trim(RegSO))



sf_result <-
  sf_deso_dalarna %>%
  inner_join(dt, by=c("Deso" = "Deso")) %>%
  inner_join(sf_kommuner_dalarna %>% as.data.table() %>% select(KOMMUNNAMN, KOMMUNKOD) %>% mutate(Kommunkod = as.integer(KOMMUNKOD)), by=c("Kommunkod" = "Kommunkod"))

sf_result_regso <-
  sf_regso_dalarna %>%
  left_join(
    dt %>% 
      inner_join(
        dt_koppling %>% select(DeSO, RegSO), by=c("Deso" = "DeSO")) %>%
      group_by(Period, Ålder, Doser, RegSO) %>%
      #filter(RegSO == "Ludvika norra") %>%
      summarise(Intervall.hög = mean(Intervall.hög), 
                Intervall.låg = mean(Intervall.låg),
                Skillnad.fg.månad = mean(Skillnad.fg.månad)) %>%
      mutate(Intervall = paste0(as.character(round(Intervall.låg,1)), "-", as.character(round(Intervall.hög,1))))
    ,by=c("RegSO" = "RegSO")) %>%
  inner_join(sf_kommuner_dalarna %>% as.data.table() %>% select(KOMMUNNAMN, KOMMUNKOD) %>% mutate(Kommunkod = as.integer(KOMMUNKOD)), by=c("Kommunkod" = "Kommunkod"))%>%
  
  mutate(RegSO_x = st_coordinates(st_centroid(geometry))[,1],
         RegSO_y = st_coordinates(st_centroid(geometry))[,2]) %>%
  mutate(Doser = recode(Doser, "två.doser" = "2 doser", "minst.en.dos" = "Minst 1 dos"))






options(shiny.reactlog=TRUE)
ui <- fluidPage(
  titlePanel(
    "Analys av Covid-19 vaccinationer i Dalarna"
  ),
  sidebarLayout(
    sidebarPanel( 
      radioButtons(inputId = "ålder",
                   label = "Ålder",
                   choices = list("18-64","65+"),
                   selected = c("18-64")),
      radioButtons(inputId = "doser",
                   label = "Doser",
                   choices = list("Minst 1 dos","2 doser"),
                   selected = c("Minst 1 dos")),
      radioButtons(inputId = "kommun",
                   label = "Kommun",
                   choices = append("Alla", unique(sf_kommuner_dalarna$KOMMUNNAMN)),
                   selected = "Alla"),
      radioButtons(inputId = "datum",
                   label = "Datum",
                   choices = unique(sf_result$Period),
                   selected = as.POSIXct("2021-09-29"))
    ),
    mainPanel(   
      tabsetPanel(type = "tabs",
                  tabPanel("Aktuellt läge", 
                           plotOutput("kartaAktuelltLäge", width = "100%", height = "800px"),
                           htmlOutput("tabellAktuelltLäge")
                  ),
                  tabPanel("Förändring sedan föregående månad", 
                           plotOutput("kartaFörändring", width = "100%", height = "800px"),
                           htmlOutput("tabellFörändring")
                  )
      )
    )
  )
)


server <- function(input, output) {
  
 # sf_result_filtered <- reactive({ 
#    sf_result %>% {if(input$kommun != "Alla") filter(., KOMMUNNAMN == input$kommun) else .} %>%
#      filter(Period == input$datum) %>%
#      select(Deso, Ålder, Doser, Intervall, Skillnad.fg.månad)
#  })
  
  
  sf_result_regso_filtered <- reactive({ 
    sf_result_regso %>% {if(input$kommun != "Alla") filter(., KOMMUNNAMN == input$kommun) else .} %>%
      filter(Period == input$datum) %>%
      filter(Doser %in% input$doser) %>%
      filter(Ålder %in% input$ålder) %>%
      select(RegSO, Ålder, Doser, Intervall, Skillnad.fg.månad, RegSO_x, RegSO_y)
  })
  
  sf_result_regso_filtered_kommun <- reactive({ 
    sf_result_regso %>% {if(input$kommun != "Alla") filter(., KOMMUNNAMN == input$kommun) else .} %>%
      filter(Period == input$datum) %>%
      mutate_if(is.numeric, round, 1) %>%
      select(RegSO, Ålder, Doser, Intervall, Skillnad.fg.månad, RegSO_x, RegSO_y)
  })
  
  
  
  output$kartaAktuelltLäge <- renderPlot({
    sf_result_regso_filtered()  %>%
      ggplot() + 
      geom_sf(aes(fill = Intervall))+  
      {if(input$kommun != "Alla") geom_label_repel(aes(label = RegSO,  x = RegSO_x, y = RegSO_y), force=50, size=4, max.overlaps = 1000, color = "black")   }+
      scale_fill_viridis_d(option = "magma", direction=1)+
      theme_minimal()+
      theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
      theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
  })
  
  output$tabellAktuelltLäge <- renderUI({
    sf_result_regso_filtered_kommun()  %>%
      as.data.table() %>%
      addHtmlTableStyle(align = "r") %>% 
        tidyHtmlTable(value = Intervall,
                      header =  Doser,
                      cgroup = Ålder,
                      rnames = RegSO)
  })
  
  output$kartaFörändring <- renderPlot({
    sf_result_regso_filtered()  %>%
      ggplot() + 
      geom_sf(aes(fill = Skillnad.fg.månad))+  
      {if(input$kommun != "Alla") geom_label_repel(aes(label = RegSO,  x = RegSO_x, y = RegSO_y), force=50, size=4, max.overlaps = 1000, color = "black")   }+
      scale_fill_viridis_c(option = "magma", direction=1)+
      theme_minimal()+
      theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
      theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    
  })
  
  output$tabellFörändring <- renderUI({
    sf_result_regso_filtered_kommun()  %>%
      as.data.table() %>%
      addHtmlTableStyle(align = "r") %>%
      tidyHtmlTable(value = Skillnad.fg.månad,
                    header =  Doser,
                    cgroup = Ålder,
                    rnames = RegSO)
  })
}

shinyApp(ui = ui, server = server)


#?scale_fill_viridis_d



