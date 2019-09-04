#' custom GGPLOT2 theme for insertion
#' adapted from https://jonlefcheck.net/2013/03/11/black-theme-for-ggplot2-2/
#' @export

theme_mk <- function(base_size = 12, base_family = "") {

  theme_minimal(base_size = base_size, base_family = base_family) %+replace%

    theme(
      # Specify axis options
      axis.line = element_blank(),
      axis.text.x = element_text(size = base_size*0.8, color = "grey34", lineheight = 0.9),
      axis.text.y = element_text(size = base_size*0.8, color = "grey34", lineheight = 0.9),
      axis.ticks = element_line(color = "grey24", size  =  0.2),
      axis.title.x = element_text(size = base_size, color = "black", margin = margin(0, 10, 0, 0)),
      axis.title.y = element_text(size = base_size, color = "black", angle = 90, margin = margin(0, 10, 0, 0)),
      axis.ticks.length = unit(0.3, "lines"),
      # Specify legend options
      # legend.background = element_rect(color = NA, fill = "black"),
      # legend.key = element_rect(color = "white",  fill = "black"),
      legend.key.size = unit(1.2, "lines"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.text = element_text(size = base_size*0.8, color = "black"),
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "black"),
      legend.position = "bottom",
      legend.text.align = NULL,
      legend.title.align = NULL,
      # legend.direction = "vertical",
      legend.box = NULL,
      # Specify panel options
      # panel.background = element_rect(fill = "black", color  =  NA),
      panel.border = element_rect(fill = NA, color = "grey24"),
      panel.grid = element_blank(),
      panel.spacing = unit(0.5, "lines"),
      # Specify facetting options
      strip.background = element_rect(fill = "tan2", color = "black"),
      strip.text.x = element_text(size = base_size*0.8, color = "grey24"),
      strip.text.y = element_text(size = base_size*0.8, color = "grey24",angle = -90),
      # Specify plot options
      # plot.background = element_rect(color = "black", fill = "black"),
      plot.title = element_text(size = base_size*1.2, color = "grey24"),
      plot.margin = unit(rep(1, 4), "lines")

    )

}

## not run
# require(ggplot2)
# data(iris)
# ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
#   theme_black()+
#   geom_point()

