basic_plot <- function(mod, condition, colour = NULL, x, y, dat, xlab, ylab){
  
  
  plot_predictions(mod, condition = condition) + 
    geom_point(aes(x = {{x}}, y = {{y}}, colour = {{colour}}), data = dat) + 
    labs(x = xlab, y = ylab, colour = "", fill = "") + 
    theme_classic() + 
    theme(axis.text = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 13, colour = "black"), 
          legend.text = element_text(size = 12, colour = "black"),
          legend.position = "top")
  
}