# Elise's ggplot themes and colour schemes
# elise.r.d.lesage@gmail.com
# Customised ggplot2 themes, color schemes, etc

# Context Habit ggplot theme
theme_sage_contexthabit <- function(base_size = 20, base_family = "Helvetica") {
  theme(
    panel.border = element_blank(), axis.line = element_line(),
    panel.grid.major=element_blank(), 
    panel.grid.minor=element_blank(),
    text = element_text(colour="Black", size=16), 
    axis.title.x = element_blank(),
    axis.text.y= element_text(colour="Black"),
    axis.title.y= element_text(colour="Black"),
    axis.line.y = element_line(colour="Black"),
    axis.line.x = element_line(colour="Black"),
    axis.ticks.y = element_line(colour="Black"),
    axis.ticks.x = element_line(colour="Black"),
    plot.title=element_text(colour="Black"),
    panel.background = element_rect(fill = "white"),
    legend.position = "bottom",
    legend.title = element_blank()
  )
}

# theme_sage_simple <- function() {
#   theme(
#     #base_size = 20,
#     # add border 1)
#     panel.border = element_rect(colour = "Black", fill = NA, linetype = 1),
#     # color background 2)
#     panel.background = element_rect(fill = "white"),
#     # modify grid 3)
#     panel.grid.major.x = element_line(colour = "grey90", linetype = 1, size = 0.5),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y =  element_line(colour = "grey90", linetype = 1, size = 0.5),
#     panel.grid.minor.y = element_blank(),
#     # modify text, axis and colour 4) and 5)
#     axis.text = element_text(colour = "Black"), #, family = "TT Arial", face = "italic"
#     axis.title = element_text(colour = "Black"), #, family = "TT Arial"
#     axis.ticks = element_line(size=1,colour="Black"),
#     # legend at the bottom 6)
#     legend.position = "bottom"
#   )
# }


