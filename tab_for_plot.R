#unweighted table - percentage responding 4 or 5 per question and disaggregating characteristic
tab_for_plot <-
  function(df,
           cols,
           disaggs = NULL,
           weighted = TRUE,
           resp_values = 4:5) {
    
    
    #check that correct df is supplied
    if (weighted) {
      if (!("tbl_svy" %in% class(df))) stop("You say you want weighted results, but you did not provide a survey design object.")
    } else {
      if ("tbl_svy" %in% class(df)) stop ("You say you want unweighted results, but you provided a survey design object.")
    }
    
    #deal with only being able to put "" for empty disaggs instead of NULL
    disaggs <- disaggs[!(disaggs == "")]
    if (length(disaggs) == 0)
      disaggs <- NULL
    
    #make performance_cat ordered (if applicable)
    if ("performance_cat" %in% disaggs) {
      df <- df %>%
        mutate(performance_cat = ordered(
          performance_cat,
          levels = c("No", "Mild", "Moderate", "Severe")
        ))
    }
  
    
    if (weighted) {
      
      tab <- df %>% 
        group_by_at(c(disaggs, "q", "resp")) %>%
        summarize(prop = survey_mean(na.rm=TRUE)) %>%
        dplyr::select(-prop_se) %>%
        mutate(prop = round(prop*100,1)) %>%
        filter(resp %in% resp_values) %>% 
        group_by_at(c(disaggs, "q")) %>%
        summarize(finalprop = sum(prop))
      
    } else {

      tab <- df %>% 
        tbl_df() %>%
        select(c(cols, disaggs)) %>% 
        mutate_at(cols, funs(ordered(., levels = 1:5))) %>% 
        gather(key = "q",
               value = "resp", !!!rlang::syms(cols)) %>% 
        group_by_at(vars(one_of(c("q", disaggs)))) %>%
        count(resp) %>%
        filter(!is.na(resp)) %>%
        mutate(prop = round(prop.table(n) * 100, 1)) %>%
        filter(resp %in% resp_values) %>%
        summarize(finalprop = sum(prop))
      
    }
    
    
    
    return(tab)
  }
