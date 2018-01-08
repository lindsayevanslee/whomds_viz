ggBarplotDodgeApp <- function(tab, fill_col = "", facet_col = "") {
 
  #initialize ggplot
  gg <- tab %>%
    ggplot(aes(x = q, y = finalprop))
  
  #create basic bar plot, based on whether there is a fill
  if (fill_col=="") {
    gg <- gg +
      geom_col() +
      scale_fill_brewer(palette = "Blues") +
      geom_text(
        aes_string(label = "finalprop"),
        position = position_dodge(width = 1),
        vjust = -0.5,
        size = 5
      )
  } else {
    gg <- gg +
      geom_col(aes_string(fill = fill_col), position = "dodge") +
      scale_fill_brewer(palette = "Blues") +
      geom_text(
        aes_string(label = "finalprop", group = fill_col),
        position = position_dodge(width = 1),
        vjust = -0.5,
        size = 5
      )
  }
  
  #universal formatting
  gg <- gg +
    xlab("Indicators") +
    ylab("Percentage") +
    theme(legend.position = "top",
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  #add facet if desired
  if (facet_col!="") {
    gg <- gg + 
      facet_grid(formula(paste0("~",facet_col)))
  }
    
  return(gg)
  
}



