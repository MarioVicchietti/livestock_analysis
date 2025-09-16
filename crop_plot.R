library(scales)
vmax <- 0.03
mean_area<-0.5*vmax
breaks_vec <- seq(0, vmax, by = 0.01)
labels_vec <- comma(breaks_vec)
#labels_vec[length(labels_vec)]<-''
myplot<-ggplot() +
  geom_sf(data=cells_bioma, aes(fill = value), color=NA) +
  geom_sf(data = biomes, fill = NA, color = "gray", size = 0.6) +
  scale_fill_gradient2(
    low     = "white",        
    mid     = "orange",       
    high    = "darkred",        
    midpoint= mean_area,
    na.value= "grey",
    name     = "", 
    limits    = c(0, vmax),
    oob    = squish ,
    breaks    = breaks_vec,            
    labels    = labels_vec,            
  ) +
  labs(
    title = "Tropical cereals",
    subtitle = "2010",
    caption  = "Source: MAgPIE/Default version",
  ) +
  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "grey90", colour = NA),
    panel.grid.major  = element_line(colour = "white")
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  guides(fill = guide_colorbar(
    barwidth      = unit(1, "cm"),
    barheight     = unit(5,   "cm"),
  )) +
  theme(
    plot.title        = element_text(size = 26, face = "bold"),
    plot.subtitle     = element_text(size = 24),
    plot.caption      = element_text(size = 22),
    legend.key.height = unit(1.2, "cm"),
    legend.key.width  = unit(0.5, "cm"),
    legend.text       = element_text(size = 20),
    panel.grid.major  = element_line(size = 0.5),
    panel.grid.minor  = element_line(size = 0.25)
  )

pdf(file   = "trce2010magpiebiomes.pdf", 
    width  = 10,      
    height = 8)       

print(myplot)      

dev.off()

df_resumo <- cells_bioma %>%
  group_by(CD_LEGEN1) %>%                             
  summarise(
    total_valor = sum(newvalues, na.rm = TRUE),     
    count        = n()                           
  ) %>%
  ungroup()                                       

print(df_resumo)
print(sum(df_resumo$total_valor)/1e6)