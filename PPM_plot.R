myplot <- ggplot(cells_bioma) +
  geom_sf(aes(fill = v), color = NA, size = 0) +
  geom_sf(data = biomes_border, fill = NA, color = "gray80", linewidth = 0.6) +
  scale_fill_gradient(name = " ", low = "white", high = "#08306b", limits = c(0, 0.5)) +
  labs(
    title    = "Cattle herd",
    subtitle = "2010",
    caption  = "Source: PPM/IBGE"
  ) +
  theme(
    axis.text   = element_blank(),
    axis.ticks  = element_blank(),
    axis.title  = element_blank(),
    plot.caption = element_text(hjust = 1, size = 20),
    plot.title   = element_text(size = 26, face = "bold"),
    plot.subtitle= element_text(size = 24),
    legend.title = element_text(size = 24),   
    legend.text  = element_text(size = 20),  
    legend.key.width  = unit(1, "cm"),     
    legend.key.height = unit(1, "cm")     
  )


pdf(file   = "PPM_fig.pdf", 
    width  = 10,      
    height = 8)       

print(myplot)      
dev.off()