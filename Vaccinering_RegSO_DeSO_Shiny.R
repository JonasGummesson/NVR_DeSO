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
library(DBI)  
  library(odbc)

  
  ##################### Deso utan ålder ####################################
# denna kod flyttad till Vaccinering_RegSO_DeSO_LaddaData.R!!
    
  
#  dt1 <- read.csv(file = "E:/Filer/admgumjon/NVR_Deso/Dalarna_vaccinationstackning_DeSO_NVR_SCB_2021-09-03_PerDeso.csv", sep=";", header=TRUE, skip=1) %>% #head(5) %>%
#    rename(minst.två.doser = X2.doser..,
#           minst.en.dos = Minst.1.dos..,
#           Deso  =DeSO.kod) %>%
#    mutate(Period = as.POSIXct("2021-09-03"))
#  dt2 <- read.csv(file = "E:/Filer/admgumjon/NVR_Deso/Dalarna_vaccinationstackning_DeSO_NVR_SCB_2021-09-29_PerDeso.csv", sep=";", header=TRUE, skip=1) %>%
#    rename(minst.två.doser = X2.doser..,
#           minst.en.dos = Minst.1.dos..,
##           Deso  =DeSO.kod) %>%
#    mutate(Period = as.POSIXct("2021-09-29"))
#  dt3 <- read.csv(file = "E:/Filer/admgumjon/NVR_Deso/Dalarna_vaccinationstackning_DeSO_NVR_SCB_2021-10-26_PerDeso.csv", sep=";", header=TRUE, skip=1) %>%
#    rename(minst.två.doser = Minst.2.doser..,
#           minst.en.dos = Minst.1.dos..,
#           Deso  = DeSO.kod) %>%
#    mutate(Period = as.POSIXct("2021-10-26"))
conn_analys <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "analys.ltdalarna.se", Database = "Analys", Trusted_Connection = "True", Encoding = "windows-1252")

grunddata_nvr <- dbGetQuery(conn_analys, "select * from NVR_RegSO_Deso") %>% 
    mutate(Period = as.Date(Period)) %>% 
    as_tibble()
  
  
 
  dt_deso <- 
    grunddata_nvr %>%
      pivot_longer(cols = c(minst.en.dos, minst.två.doser), names_to = "Doser", values_to = "Procent") %>%
    group_by(Deso, Doser) %>%
    mutate(Procent = as.integer(Procent))%>%
    mutate(Procent.fg = lag(Procent, n = 1, order_by = Period)) %>%
    mutate(Skillnad.fg.månad = Procent - Procent.fg) 
  
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
  
  
  
  sf_result_deso <-
    sf_deso_dalarna %>%
    inner_join(dt_deso, by=c("Deso" = "Deso")) %>%
    inner_join(sf_kommuner_dalarna %>% as.data.table() %>% select(KOMMUNNAMN, KOMMUNKOD) %>% mutate(Kommunkod = as.integer(KOMMUNKOD)), by=c("Kommunkod" = "Kommunkod"))
  
  # data till dalarnas tidningar
#dalarnasTidningarExport <-  dt_deso %>% 
    #filter(Period == as.POSIXct("2021-10-26")) %>%
    #inner_join(
     # dt_koppling %>% select(DeSO, RegSO, Kommunnamn), by=c("Deso" = "DeSO"))# %>%
  #write.csv(dalarnasTidningarExport, file="E:/Filer/admgumjon/dalarnasTidningarExport.csv",row.names=TRUE)

  

  
  sf_result_regso <-
    sf_regso_dalarna %>%
    left_join(
      dt_deso %>% 
        inner_join(
          dt_koppling %>% select(DeSO, RegSO), by=c("Deso" = "DeSO")) %>%
        group_by(Period, Doser, RegSO) %>%
        #filter(RegSO == "Ludvika norra") %>%
        summarise(Procent = mean(Procent),
                  Skillnad.fg.månad = mean(Skillnad.fg.månad)) 
      ,by=c("RegSO" = "RegSO")) %>%
    inner_join(sf_kommuner_dalarna %>% as.data.table() %>% select(KOMMUNNAMN, KOMMUNKOD) %>% mutate(Kommunkod = as.integer(KOMMUNKOD)), by=c("Kommunkod" = "Kommunkod"))%>%
    
    mutate(RegSO_x = st_coordinates(st_centroid(geometry))[,1],
           RegSO_y = st_coordinates(st_centroid(geometry))[,2]) %>%
    mutate(Doser = recode(Doser, "minst.två.doser" = "Minst 2 doser", "minst.en.dos" = "Minst 1 dos"))# %>%
   # group_by(RegSO, Doser) %>%
    #mutate(Procent.fg = lag(Procent, order_by = Period))
  
  
  #sf_result_regso %>% filter(RegSO == "Horndal")
  
  options(shiny.reactlog=TRUE)
  ui <- fluidPage(
    titlePanel(
      "Analys av Covid-19 vaccinationer i Dalarna"
    ),
    sidebarLayout(
      sidebarPanel( 
        radioButtons(inputId = "doser",
                     label = "Doser",
                     choices = unique(sf_result_regso$Doser),
                     selected = max(sf_result_regso$Doser)),
        radioButtons(inputId = "kommun",
                     label = "Kommun",
                     choices = append("Alla", unique(sf_kommuner_dalarna$KOMMUNNAMN)),
                     selected = "Alla"),
        radioButtons(inputId = "datum",
                     label = "Datum",
                     choices = unique(sf_result_regso$Period),
                     selected = max(sf_result_regso$Period)),
        width=2
      ),
      mainPanel(   
        tabsetPanel(type = "tabs",
                    tabPanel("Aktuell vaccinationsgrad (per RegSO)", 
                             plotOutput("kartaAktuelltLäge", width = "100%", height = "800px"),
                             htmlOutput("tabellAktuelltLäge")
                    ),
                    tabPanel("Förändring sedan föregående mätning (per RegSO)", 
                             plotOutput("kartaFörändring", width = "100%", height = "800px"),
                             htmlOutput("tabellFörändring")
                    ),
                    tabPanel("Tabell över tid (per RegSO)", 
                             tableOutput("tabellÖverTid")
                    )
                
        )
      )
    )
  )
  
  
  server <- function(input, output) {
    
    sf_result_regso_filtered <- reactive({ 
      sf_result_regso %>% {if(input$kommun != "Alla") filter(., KOMMUNNAMN == input$kommun) else .} %>%
        filter(Period == input$datum) %>%
        filter(Doser %in% input$doser) %>%
        select(RegSO, Doser, Procent, Skillnad.fg.månad, RegSO_x, RegSO_y, Period)
    })
    
    sf_result_regso_filtered_trend <- reactive({ 
      sf_result_regso %>% {if(input$kommun != "Alla") filter(., KOMMUNNAMN == input$kommun) else .} %>%
        filter(Doser %in% input$doser) %>%
        mutate(Period = format(Period, "%Y%m%d"))%>%
        mutate(Procent = round(Procent,1))%>%
        select(RegSO, Doser, Procent, Skillnad.fg.månad, RegSO_x, RegSO_y, Period)
    
    })
    
    ################# DeSO  #########################
  
    output$kartaAktuelltLäge <- renderPlot({
      sf_result_regso_filtered()  %>%
        ggplot() + 
        geom_sf(aes(fill = Procent))+ 
        #geom_sf(data = sf_kommuner_dalarna, fill = NA, linetype = "dashed", size = 3) +
        {if(input$kommun != "Alla") geom_label_repel(aes(label = RegSO,  x = RegSO_x, y = RegSO_y), force=50, size=4, max.overlaps = 1000, color = "black")   }+
        {if(input$kommun == "Alla") geom_sf(data = sf_kommuner_dalarna, fill = NA, linetype = "solid", size = 1, color = "#333333")  }+
        {if(input$kommun == "Alla") geom_label_repel(data = sf_kommuner_dalarna, aes(label = KOMMUNNAMN,  x = Label_x, y = Label_y), force=50, size=4, max.overlaps = 1000, color = "black") } +
        
        scale_fill_viridis_c(option = "magma", direction=1)+
        theme_minimal()+
        theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
        theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
    })
    
    output$tabellAktuelltLäge <- function(){
      sf_result_regso_filtered()  %>%
        as.data.table() %>%
        mutate(Period =format(Period,"%Y%m%d")) %>%
        pivot_wider(id_cols = RegSO, names_from = Period, values_from = Procent) %>%
        arrange(desc(.[[2]]))%>%
        mutate(across(starts_with("20") , ~round(.x,0))) %>%
        mutate(across(starts_with("20") , ~cell_spec(.x,  bold = T, color = spec_color(.x, end = 0.9, option = "magma", direction = 1)))) %>%
        kable(escape = F, align = "lrrr") %>%
        kable_classic("striped", full_width = F)%>%
        column_spec(1:2, width_min = "5em", bold = FALSE, italic = FALSE)
    }
    
    output$kartaFörändring <- renderPlot({
      sf_result_regso_filtered()  %>%
        ggplot() + 
        geom_sf(aes(fill = Skillnad.fg.månad))+  
        {if(input$kommun != "Alla") geom_label_repel(aes(label = RegSO,  x = RegSO_x, y = RegSO_y), force=50, size=4, max.overlaps = 1000, color = "black")   }+
        {if(input$kommun == "Alla") geom_sf(data = sf_kommuner_dalarna, fill = NA, linetype = "solid", size = 1, color = "#333333")  }+
        {if(input$kommun == "Alla") geom_label_repel(data = sf_kommuner_dalarna, aes(label = KOMMUNNAMN,  x = Label_x, y = Label_y), force=50, size=4, max.overlaps = 1000, color = "black") } +
        
        
        scale_fill_viridis_c(option = "magma", direction=1)+
        theme_minimal()+
        theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
        theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
    })
    
    output$tabellFörändring<- function(){
      sf_result_regso_filtered()  %>%
        as.data.table() %>%
        mutate(Period =format(Period,"%Y%m%d")) %>%
        pivot_wider(id_cols = RegSO, names_from = Period, values_from = Skillnad.fg.månad) %>%
        arrange(desc(.[[2]]))%>%
        mutate(across(starts_with("20") , ~round(.x,0))) %>%
        mutate(across(starts_with("20") , ~cell_spec(.x,  bold = T, color = spec_color(.x, end = 0.9, option = "magma", direction = 1)))) %>%
        kable(escape = F, align = "lrrr") %>%
        kable_classic("striped", full_width = F)%>%
        column_spec(1:2, width_min = "5em", bold = FALSE, italic = FALSE)
    }
    
   

    output$tabellÖverTid <- function(){
      sf_result_regso_filtered_trend()  %>%
        as.data.table() %>%
        pivot_wider(id_cols = RegSO, names_from = Period, values_from = Procent) %>%
        mutate(Skillnad = round(.[[6]]-.[[5]],0))%>%
        #rename(`Skillnad %` = Skillnad) %>%
        mutate(across(starts_with("20") , ~round(.x,0))) %>%
        arrange(desc(Skillnad)) %>%
        #mutate(Skillnad = sprintf("%.1f", Skillnad)) %>%
        mutate(across(starts_with("20") , ~cell_spec(.x,  bold = T, color = spec_color(.x, end = 0.9, option = "magma", direction = 1)))) %>%
        mutate(Skillnad = cell_spec(Skillnad, color = "white", bold = T, background = spec_color(Skillnad, end = 0.9, option = "magma", direction = 1))) %>%
        kable(escape = F, align = "lcccccc", digits = 1) %>%
        kable_classic("striped", full_width = F)%>%
        column_spec(1:5, width_min = "5em", bold = FALSE, italic = FALSE)
    }
    
  }
  
  shinyApp(ui = ui, server = server)
  #?scale_fill_viridis_d
  
  
  
