theme_APA <-
function(base_size = 11, base_family = ""){
    require(grid)
    half_line <- base_size/2
    theme(line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
          rect = element_rect(fill = NA, colour = "black", size = 0.5, linetype = 1),
          text = element_text(family = base_family, 
                              face = "plain", 
                              colour = "black", 
                              size = base_size, 
                              lineheight = 0.9, 
                              hjust = 0.5, 
                              vjust = 0.5, 
                              angle = 0, 
                              margin = margin(), 
                              debug = FALSE),          
          
          axis.line = element_blank(),
          axis.text = element_text(size = rel(0.8)),
          axis.text.x = element_text(margin = margin(t = 2 * half_line/2), vjust = 1),
          axis.text.y = element_text(margin = margin(r = 2 * half_line/2), vjust = 0.5),
          axis.ticks = element_line(colour = "black", size = 0.5), 
          axis.ticks.length = unit(-(half_line/2), "pt"), 
          axis.title.x = element_text(margin = margin(t = 0.8*half_line, b = 0.8* half_line/2)), 
          axis.title.y = element_text(angle = 90, margin = margin(r = 0.8 * half_line, l = 0.8 * half_line/2)),
          
          legend.background = element_blank(), 
          legend.margin = unit(0.1, "cm"), 
          legend.key = element_blank(), 
          legend.key.size = unit(0.5, "lines"), 
          legend.key.height = NULL, 
          legend.key.width = NULL, 
          legend.text = element_text(size = rel(0.6)), 
          legend.text.align = NULL, 
          legend.title = element_text(size = rel(0.7), hjust = 0), 
          legend.title.align = NULL, 
          legend.position = "right", 
          legend.direction = "vertical", 
          legend.justification = "center", 
          legend.box = "horizontal", 
          
          panel.background = element_rect(), 
          panel.border = element_rect(size = 1, fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.margin = unit(half_line, "pt"), 
          panel.margin.x = NULL, 
          panel.margin.y = NULL, 
          panel.ontop = FALSE, 
          
          strip.background = element_blank(), 
          strip.text = element_text(), 
          strip.text.x = element_text(margin = margin(t = half_line, b = half_line)), 
          strip.text.y = element_text(angle = -90, margin = margin(l = half_line, r = half_line)), 
          strip.switch.pad.grid = unit(0.1, "cm"), 
          strip.switch.pad.wrap = unit(0.1, "cm"), 
          
          plot.background = element_rect(colour = NA, fill = NA),
          plot.title = element_text(size = rel(1), margin = margin(b = half_line * 1.2)), 
          plot.margin = margin(half_line, half_line, half_line, half_line), 
          
          complete = TRUE)
}
