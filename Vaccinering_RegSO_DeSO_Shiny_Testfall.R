


sf_result_regso_ålder  %>%
  filter(KOMMUNNAMN == "Falun") %>%
  filter(Period == as.POSIXct("2021-09-29")) %>%
  filter(Doser %in% "Minst 1 dos") %>%
  filter(Ålder %in% "18-64") %>%
  mutate(Intervall.hög.min = min(Intervall.hög),
         Intervall.hög.max = max(Intervall.hög)) %>%
  ggplot() + 
  geom_sf(aes(fill = Intervall))+  
  geom_label_repel(aes(label = RegSO,  x = RegSO_x, y = RegSO_y), force=50, size=4, max.overlaps = 1000, color = "black") +
  scale_fill_viridis_d(option = "magma")+
  theme_minimal()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#unique(sf_result_regso$Procent)
str(sf_result_regso)
sf_result_regso  %>% 
  filter(KOMMUNNAMN == "Falun") %>%
  filter(Period == as.POSIXct("2021-09-29")) %>%
  filter(Doser %in% "Minst 1 dos") %>% #head(5) %>%
  ggplot() + 
  geom_sf(aes(fill = Procent))+  
  geom_label_repel(aes(label = RegSO,  x = RegSO_x, y = RegSO_y), force=50, size=4, max.overlaps = 1000, color = "black") +
  scale_fill_viridis_c(option = "magma", direction=1)+
  theme_minimal()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

?scale_fill_viridis

sf_result_regso  %>%
  as.data.table() %>%
  filter(KOMMUNNAMN == "Falun") %>%
  #filter(Period == as.POSIXct("2021-09-29")) %>%
  filter(Doser %in% "Minst 1 dos") %>%
  mutate(Procent = round(Procent,1)) %>%
  select(RegSO, Period, Procent) %>%
  ungroup() %>%
  pivot_wider(id_cols = RegSO, names_from = Period, values_from = Procent) %>%
  #mutate(test = .[[2]])
  mutate(Skillnad = round(.[[3]]-.[[2]],1))%>%
#  rowwise()%>%
  mutate(across(starts_with("20") , ~cell_spec(.x, color = "white", bold = T, background = spec_color(.x, end = 0.9, option = "magma", direction = 1)))) %>%
  #mutate(Skillnad = cell_spec(Skillnad, color = "white", bold = T, background = spec_color(Skillnad, end = 0.9, option = "magma", direction = 1))) %>%
  #kbl(escape = F) %>%
  kable(escape = F, align = "c") %>%
  #kable_paper()
  kable_classic("striped", full_width = F)
