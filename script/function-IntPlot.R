int_plot <- function(mod, variables, condition, xlab, ylab){
  
  plot_comparisons(mod, variables = variables, 
                   condition = condition, type = "response") + 
    theme_classic() + 
    labs(x = xlab, y = ylab) + 
    theme(axis.text = element_text(size = 10, colour = "black"))
  
  
}