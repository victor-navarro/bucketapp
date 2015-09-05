theme_shiny = function(base_size = 18, base_family = "Helvetica") {
    require(grid)
    theme(
        line =               element_line(colour = "black", size = 1, linetype = 1,
                                          lineend = "butt"),
        rect =               element_rect(fill = NA, colour = "black", size = 1, linetype = 1),
        text =               element_text(family = base_family, face = "plain",
                                          colour = "black", size = base_size,
                                          hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
        axis.text =          element_text(size = rel(0.8), colour = "black"),
        strip.text =         element_text(size = rel(0.8), colour = "black"),
        
        axis.line =          element_blank(),
        axis.text.x =        element_text(vjust = 1, size = rel(0.8)),
        axis.text.y =        element_text(size = rel(0.8), hjust = 1),
        axis.ticks =         element_line(colour = "black", size = 0.5),
        axis.title =         element_text(colour = "black", size = rel(0.8)),
        axis.title.x =       element_text(vjust= 0),
        axis.title.y =       element_text(angle = 90, vjust=1),
        axis.ticks.length =  unit(-0.25, "lines"),
        axis.ticks.margin =  unit(0.5, "lines"),
        
        legend.background =  element_blank(),
        legend.margin =      unit(0, "cm"),
        legend.key =         element_blank(),
        legend.key.size =    unit(1, "lines"),
        legend.key.height =  NULL,
        legend.key.width =   NULL,
        legend.text =        element_text(size = rel(0.6), colour = "black"),
        legend.text.align =  NULL,
        legend.title =       element_text(size = rel(0.7), hjust = 0, colour = "black"),
        legend.title.align = NULL,
        legend.position =    "right",
        legend.direction =   "vertical",
        legend.justification = "center",
        legend.box =         'horizontal',
        
        panel.background =   element_rect(),
        panel.border =       NULL,
        panel.grid.major =   element_blank(),
        panel.grid.minor =   element_blank(),
        panel.margin =       unit(0.25, "lines"),
        
        strip.background =   element_blank(),
        strip.text.x =       element_text(),
        strip.text.y =       element_text(angle = -90),
        
        plot.background =    element_rect(colour = NA, fill = NA),
        plot.title =         element_text(size = rel(1), vjust = 2),
        plot.margin =        unit(c(1, 1, 0.5, 0.5), "lines"),
        
        complete = TRUE
    )
}