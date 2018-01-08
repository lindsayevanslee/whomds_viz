#unweighted table - percentage responding 4 or 5 per question and disaggregating characteristic
tabDodgeApp <- function(df, cols, disaggs = NULL) {
  
  #deal with only being able to put "" for empty disaggs instead of NULL
  disaggs <- disaggs[!(disaggs=="")]
  if (length(disaggs)==0) disaggs <- NULL
  
  if ("performance_cat" %in% disaggs) {
    df[,"performance_cat"] <- ordered(df[,"performance_cat"], levels = c("No", "Mild", "Moderate", "Severe"))
  }
  
  df <- df %>%
    tbl_df() %>%
    select(c(cols, disaggs)) %>%
    mutate_at(cols, funs(ordered(., levels = 1:5)))
  
  tab <- df %>% 
    gather_(key_col = "q",
            value_col = "resp",
            gather_cols = cols) %>%
    group_by_at(vars(one_of(c("q", disaggs)))) %>%
    count(resp) %>%
    filter(!is.na(resp)) %>%
    mutate(prop=round(prop.table(n)*100,1)) %>%
    filter(resp %in% 4:5) %>%
    summarize(finalprop=sum(prop))
  
  return(tab)
}