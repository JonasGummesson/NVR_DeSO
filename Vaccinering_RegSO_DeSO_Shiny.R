# visualisering av data fr�n Nationella vaccinationsregistret
library(sjmisc)
library(sf)
library(viridis)
library(htmlTable)
library(shiny)
library(kableExtra)

dt1 <- read.csv(file = "E:/Filer/admgumjon/NVR_Deso/Dalarna_vaccinationstackning_DeSO_NVR_SCB_2021-09-03.csv", sep=";", header=TRUE, skip=1) %>% #head(5) %>%
  rename(tv�.doser = X2.doser..,
         minst.en.dos = Minst.1.dos..,
         Deso  =DeSO.kod) %>%
  mutate(Period = as.POSIXct("2021-09-03"))
  
dt2 <- read.csv(file = "E:/Filer/admgumjon/NVR_Deso/Dalarna_vaccinationstackning_DeSO_NVR_SCB_2021-09-29.csv", sep=";", header=TRUE, skip=1) %>%
  rename(tv�.doser = X2.doser..,
         minst.en.dos = Minst.1.dos..,
         Deso  =DeSO.kod) %>%
  mutate(Period = as.POSIXct("2021-09-29"))
  

# union datasets
dt <- 
  rbind(
    dt1 %>% select(Period, Deso, �lder, minst.en.dos, tv�.doser),
    dt2 %>% select(Period, Deso, �lder, minst.en.dos, tv�.doser)
  ) %>%
  pivot_longer(cols = c(minst.en.dos, tv�.doser), names_to = "Doser", values_to = "Intervall") %>%
  separate(Intervall, sep = "-", into = c("Intervall.l�g", "Intervall.h�g"), remove = FALSE) %>%
  mutate(Intervall.l�g = as.integer(Intervall.l�g),
         Intervall.h�g = as.integer(Intervall.h�g)) %>%
  group_by(Deso, �lder, Doser) %>%
  mutate(Intervall.l�g.fg = lag(Intervall.l�g, n = 1, order_by = Period)) %>%
  mutate(Skillnad.fg.m�nad = Intervall.l�g - Intervall.l�g.fg)
  


################## ladda kommunkartor #################

sf_kommuner_dalarna <- st_read(dsn = "E:/Filer/admgumjon/Kommungr�nser_Dalarna") %>%
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
  mutate(Kommunkod = as.integer(Kommunkod))

################## Koppling RegSO/DeSO #################
dt_koppling <- read.csv("E:/Filer/admgumjon/Kartor/kopplingstabell-deso-regso-20210702-v2.csv", skip=3, sep=";") %>% #head(15) %>%
  rename(Kommunkod = bro) %>%
  select(Kommunkod, Kommunnamn, DeSO, RegSO)



sf_result <-
  sf_deso_dalarna %>%
  inner_join(dt, by=c("Deso" = "Deso")) %>%
  inner_join(sf_kommuner_dalarna %>% as.data.table() %>% select(KOMMUNNAMN, KOMMUNKOD) %>% mutate(Kommunkod = as.integer(KOMMUNKOD)), by=c("Kommunkod" = "Kommunkod"))
  
sf_result_regso <-
  sf_regso_dalarna %>%

  inner_join(
    dt %>% 
      inner_join(
        dt_koppling %>% select(DeSO, RegSO), by=c("Deso" = "DeSO")) %>%
      group_by(Period, �lder, Doser, RegSO) %>%
      #filter(RegSO == "Ludvika norra") %>%
      summarise(Intervall.h�g = mean(Intervall.h�g), 
                Intervall.l�g = mean(Intervall.l�g),
                Skillnad.fg.m�nad = mean(Skillnad.fg.m�nad)) %>%
    mutate(Intervall = paste0(as.character(round(Intervall.l�g,1)), "-", as.character(round(Intervall.h�g,1))))
    ,by=c("RegSO" = "RegSO")) %>%
  inner_join(sf_kommuner_dalarna %>% as.data.table() %>% select(KOMMUNNAMN, KOMMUNKOD) %>% mutate(Kommunkod = as.integer(KOMMUNKOD)), by=c("Kommunkod" = "Kommunkod"))%>%
 
  mutate(RegSO_x = st_coordinates(st_centroid(geometry))[,1],
         RegSO_y = st_coordinates(st_centroid(geometry))[,2])


#t <- 
  sf_result_regso %>%
  as.data.table() %>%
  filter(Period == as.POSIXct("2021-09-29")) %>%
  #filter(RegSO == "Ludvika norra") %>%
  select(Period, �lder, Doser, RegSO, Intervall, Skillnad.fg.m�nad) %>%
  mutate(Skillnad.fg.m�nad = round(Skillnad.fg.m�nad,1)) %>%
  #dplyr::mutate_if(is.character, .funs = function(x){return(`Encoding<-`(x, "UTF-8"))})%>%
  #dplyr::mutate_if(is.character, .funs = function(x){iconv(x, from = "1252", to = "UTF-8")})%>%
  select(RegSO, �lder, Doser, Intervall) %>%
    pivot_wider(names_from = c(�lder, Doser), values_from = Intervall)
 # kbl()
    kbl(row.names= TRUE, col.names = c("RegSO", "�lder", "Doser", "Intervall"))
  #?kbl
#addHtmlTableStyle(align = "r") %>% 
  #tidyHtmlTable(value = Intervall,
    #  header =  Doser,
    #            cgroup = �lder,
     #           rnames = RegSO)
                #rgroup = per_metric)


t %>%
  kbl() %>%
  kable_paper("hover", full_width = F)

colnames(t) <- iconv(colnames(t), from = "1252", to = "UTF-8")
t %>% addHtmlTableStyle(align = "r") %>% 
  tidyHtmlTable(value = Intervall,
                header =  Doser,
                cgroup = �lder,
                rnames = RegSO)

#sf_result %>% {if(length(list("1", "2")>0)) filter(.,KOMMUNNAMN %in% c("Avesta","3")) }

#?filter_if
options(shiny.reactlog=TRUE)
ui <- fluidPage(
  titlePanel(
    "Analys av Covid-19 vaccinationer i Dalarna"
  ),
  sidebarLayout(
    sidebarPanel( 
      radioButtons(inputId = "�lder",
                         label = "�lder",
                         choices = list("18-64","65+"),
                         selected = c("18-64")),
      radioButtons(inputId = "doser",
                         label = "Doser",
                         choices = list("minst.en.dos","tv�.doser"),
                         selected = c("minst.en.dos")),
      radioButtons(inputId = "kommun",
                   label = "Kommun",
                   choices = append("Alla", unique(sf_kommuner_dalarna$KOMMUNNAMN)),
                   selected = "Alla"),
      radioButtons(inputId = "datum",
                   label = "Datum",
                   choices = unique(sf_result$period),
                   selected = sf_result$period[1])
    ),
    mainPanel(   
      tabsetPanel(type = "tabs",
                  tabPanel("Aktuellt l�ge", 
                           plotOutput("kartaAktuelltL�ge", width = "100%", height = "800px")
                  ),
                  tabPanel("F�r�ndring sedan f�reg�ende m�nad", 
                           plotOutput("kartaF�r�ndring", width = "100%", height = "800px")
                  )
      )
    )
  )
)


server <- function(input, output) {
  
  sf_result_filtered <- reactive({ 
    sf_result %>% {if(input$kommun != "Alla") filter(., KOMMUNNAMN == input$kommun) else .} %>%
      filter(period == input$datum) %>%
      select(Deso, �lder, Doser, Intervall, f�r�ndring)
  })
  
  
  sf_result_regso_filtered <- reactive({ 
    sf_result_regso %>% {if(input$kommun != "Alla") filter(., KOMMUNNAMN == input$kommun) else .} %>%
      filter(period == input$datum) %>%
      filter(Doser %in% input$doser) %>%
      filter(�lder %in% input$�lder) %>%
      select(regso, �lder, Doser, Intervall, f�r�ndring, RegSO_x, RegSO_y)
  })
  
 
  
  output$kartaAktuelltL�ge <- renderPlot({
    sf_result_regso_filtered()  %>%
      ggplot() + 
      geom_sf(aes(fill = Intervall))+  
      {if(input$kommun != "Alla") geom_text_repel(aes(label = regso,  x = RegSO_x, y = RegSO_y), color = "blue", size=4, max.overlaps = 1000, color = "white")   }+
      scale_fill_viridis_d(option = "plasma", direction=1)+
      #{ if(length(unique(sf_result_regso_filtered()$�lder)) > 1 || length(unique(sf_result_regso_filtered()$Doser)) > 1) facet_grid(cols = vars(�lder), rows = vars(Doser)) }+
      theme_minimal()+
      theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
      theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
  })
  
  output$kartaF�r�ndring <- renderPlot({
    sf_result_regso_filtered()  %>%
      ggplot() + 
      geom_sf(aes(fill = f�r�ndring))+  
      scale_fill_viridis_c(option = "plasma", direction=1)+
      #{ if(length(unique(sf_result_regso_filtered()$�lder)) > 1 || length(unique(sf_result_regso_filtered()$Doser)) > 1) facet_grid(cols = vars(�lder), rows = vars(Doser)) }+
      theme_minimal()+
      theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
      theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
     
    
  })
}

shinyApp(ui = ui, server = server)





