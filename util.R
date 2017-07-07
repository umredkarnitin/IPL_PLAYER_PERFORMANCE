ggplot_missing  <- function(x){
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_discrete(name = "",
                        labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}
