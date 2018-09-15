mds_barplot <- function(tab, fill_col = "", facet_col = "", indicator_choices) {
 
  #change questions to english labels
  tab$q <- plyr::mapvalues(tab$q,
                           from = unlist(indicator_choices),
                           to = names(indicator_choices), 
                           warn_missing = FALSE)
  
  #initialize ggplot, based on whether there is a fill
  if (fill_col=="") {
    gg <- tab %>%
      ggplot(aes_string(x = "q", y = "finalprop"))
  } else {
    gg <- tab %>%
      ggplot(aes_string(x = "q", y = "finalprop", fill = fill_col)) +
      scale_fill_brewer(palette = "Blues")
  }
  
  #create barplot with universal formatting
  gg <- gg +
    geom_col(position = "dodge") +
    xlab("Indicators") +
    ylab("Percentage") +
    scale_y_continuous(limits = c(0,max(tab[,"finalprop"])+2)) + 
    geom_text(
      aes(label = finalprop),
      position = position_dodge(width = 1),
      vjust = -0.5,
      size = 5
    ) +
    theme(legend.position = "top",
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  #add facet if desired
  if (facet_col!="") {
    gg <- gg + 
      facet_grid(formula(paste0("~",facet_col)))
  }
    
  return(gg)
  
}



