mapa <- fortify(rnaturalearth::ne_states(c("Argentina")))


ggplot() +
  geom_path(data = mapa, aes(long, lat, group = group)) 


plot <- ggplot(mapa_pob)  +
  geom_sf(aes(fill = prop_ponlacional)) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  ggthemes::theme_pander()+
  labs(title = "Porcentaje de titulares por provincia",
       subtitle = "En base a proyecciones de INDEC - Año 2022 ",
       fill = "Proporción") +
  theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14L, face = "italic", hjust = 0.5))

plot + annotate("text", x = -58, y = -50, label = paste("% Total:",prop_titulares,"%"), size = 2, color = "black")



mapa_argentina <- mapa_argentina |> 
  rename("provincia_id"= codprov_censo) |> 
  left_join(poblacion)

mapa_provincia <- titulares_ano |> 
  filter(year(periodo) == 2022) |> 
  group_by(provincia_id) |> 
  summarise(titulares_2022= sum(titulares))

mapa_provincia <- mapa_provincia |> 
  mutate(prop_2022 = titulares_2022/sum(mapa_provincia$titulares_2022))

mapa_argentina <- mapa_argentina |> 
  left_join(mapa_provincia)

total_titulares <- sum(mapa_provincia_pob$titulares_2022)
total_poblacion <- sum(mapa_provincia_pob$poblacion)
prop_titulares <- round((total_titulares/total_poblacion)*100,2)


ggplot(mapa_argentina) +
  geom_sf(aes(fill = prop_2022)) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  ggthemes::theme_pander()+
  labs(title = "Porcentaje de titulares por provincia",
       subtitle = "En base a proyecciones de INDEC - Año 2022 ",
       fill = "Proporción") +
  theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14L, face = "italic", hjust = 0.5))

plot + annotate("text", x = -58, y = -50, label = paste("% Total:",prop_titulares,"%"), size = 2, color = "black")
