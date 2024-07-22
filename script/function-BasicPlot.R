basic_plot <- function(mod, condition, colour = NULL, x, y, dat, xlab, ylab){
  
  
  plot_predictions(mod, condition = condition) + 
    geom_point(aes(x = {{x}}, y = {{y}}, colour = {{colour}}), data = dat) + 
    labs(x = xlab, y = ylab, colour = "", fill = "") + 
    theme_classic() + 
    theme(axis.text = element_text(size = 10),
          legend.position = "top")
  
}