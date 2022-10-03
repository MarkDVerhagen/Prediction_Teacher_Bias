# Setup -------------------------------------------------------------------
scatter_size <- 4
alpha <- 0.3
text_size <- 20

# Standard theme
font_family <- "Helvetica"

custom_theme <- function(text_size = text_size, hor = F, ver = F) {
    theme_custom <- theme(
       # strip.placement = "outside",
       #  strip.text.y = element_text(face = "bold", hjust=0.5, vjust=0.5),
       strip.background = element_rect(fill = NA, color = "black", size = 1.5),
       panel.spacing.x = unit(0.08, "lines"),
       panel.spacing.y = unit(0.1, "lines"),
       # panel.border = element_rect(color = "lightgrey", fill = NA, size = 0.5),
       panel.border = element_rect(color = "black"),
       legend.position = "top",
       text = element_text(size = text_size, family = font_family),
       axis.text.x = element_text(size = text_size, family = font_family),
       axis.text.y = element_text(size = text_size)
    )
    if (hor) {
        theme_custom <- theme_custom +
            theme(
                panel.grid.major.y = element_line(
                    size = 0.5, linetype = "dotted",
                    colour = "lightgrey"
                ),
                panel.grid.minor.y = element_line(
                    size = 0.25, linetype = "dotted",
                    colour = "lightgrey"
                )
            )
    }
    if (ver) {
        theme_custom <- theme_custom +
            theme(
                panel.grid.major.x = element_line(
                    size = 0.5, linetype = "dotted",
                    colour = "lightgrey"
                ),
                panel.grid.minor.x = element_line(
                    size = 0.25, linetype = "dotted",
                    colour = "lightgrey"
                )
            )
    }
    return(theme_custom)
}