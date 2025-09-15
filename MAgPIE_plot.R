myplot <- ggplot(cells_bioma_ord) +
  geom_sf(aes(fill = v), color = NA, size = 0) +
  geom_sf(data = biomes_border, fill = NA, color = "gray80", linewidth = 1) +
  scale_fill_gradient(name = " ", low = "white", high = "#08306b", limits = c(0, 0.5)) +
  labs(
    title    = "Cattle herd",
    subtitle = "2010",
    caption  = "Source: MAgPIE/Default version"
  ) +
  theme(
    axis.text   = element_blank(),
    axis.ticks  = element_blank(),
    axis.title  = element_blank(),
    plot.caption = element_text(hjust = 1, size = 20),
    plot.title   = element_text(size = 26, face = "bold"),
    plot.subtitle= element_text(size = 24),
    legend.title = element_text(size = 24),   # aumenta título da legenda
    legend.text  = element_text(size = 20),   # aumenta textos da legenda
    legend.key.width  = unit(1, "cm"),      # largura da barra de escala
    legend.key.height = unit(1, "cm")       # altura da barra de escala
  )


pdf(file   = "cattle_fig.pdf", 
    width  = 10,
    height = 8)

print(myplot)      
dev.off()