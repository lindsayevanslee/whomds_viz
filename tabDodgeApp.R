tabDodgeApp <- function(df, cols, disaggs = NULL) {
  df <- df %>%
    tbl_df() %>%
    select(c(cols, disaggs)) %>%
    mutate_at(cols, funs(ordered(., levels = 1:5)))
  
  tab <- dat %>% 
    gather_(key_col = "q",
            value_col = "resp",
            gather_cols = cols) %>%
    group_by_at(vars(one_of(c("q", disaggs)))) %>%
    count(resp) %>%
    filter(!is.na(resp)) %>%
    mutate(prop=round(prop.table(n)*100,1)) %>%
    filter(resp %in% 4:5) %>%
    summarize(finalprop=sum(prop))
  
  
  
  
  # if (is.null(disaggs)) {
  #   tab <- dat %>% 
  #     gather_(key_col = "q",
  #             value_col = "resp",
  #             gather_cols = cols) %>%
  #     group_by(q) %>%
  #     count(resp) %>%
  #     filter(!is.na(resp)) %>%
  #     mutate(prop=round(prop.table(n)*100,1)) %>%
  #     filter(resp %in% 4:5) %>%
  #     summarize(finalprop=sum(prop))
  #   
  # } else if (length(disaggs) == 1)  {
  #   tab_raw <- dat %>%
  #     select(cols) %>%
  #     map(function(col) {
  #       colSums(round(prop.table(table(
  #         unlist(col), unlist(dat[, disaggs])
  #       )) * 100, 1)[4:5,])
  #       
  #     })
  #   
  #   tab <- tab_raw %>%
  #     bind_rows() %>%
  #     add_column(sex = names(tab_raw[[1]]))
  #   
  # } else if (length(disaggs) == 2) {
  #   
  # }
  # 
  
  
  
}


cols <- paste0("B", 3001:3002)
disaggs <- NULL
disaggs <- "sex"
disaggs <- c("sex","performance_cat")

dat <- mdstest %>%
  tbl_df() %>%
  select(c(cols, disaggs)) %>%
  mutate_at(cols, funs(ordered(., levels = 1:5)))


#disaggs==NULL
tab <- dat %>% 
  gather_(key_col = "q",
          value_col = "resp",
          gather_cols = cols) %>%
  group_by_at(vars(one_of(c("q", disaggs)))) %>%
  count(resp) %>%
  filter(!is.na(resp)) %>%
  mutate(prop=round(prop.table(n)*100,1)) %>%
  filter(resp %in% 4:5) %>%
  summarize(finalprop=sum(prop))


#disaggs length 1
tab_raw <- dat %>%
  select(cols) %>%
  map(function(col) {
    colSums(round(prop.table(table(
      unlist(col), unlist(dat[, disaggs])
    )) * 100, 1)[4:5,])
    
  })

tab <- tab_raw %>%
  bind_rows() %>%
  add_column(sex = names(tab_raw[[1]]))

#disaggs length 2
dat %>%
  select(cols) %>%
  map(function(col) {
    col %>%
      unlist() %>%
      table(unlist(dat[,disaggs])) %>%
      prop.table()*100 %>%
      round(1)[4:5,] %>%
      colSums()
    
    
    # colSums(round(prop.table(table(
    #   unlist(col), unlist(dat[, disaggs])
    # )) * 100, 1)[4:5,])
    # 
  })

dat[,"B3001"] %>%
  unlist() %>%
  table(unlist(dat[,disaggs[1]])) %>%
  prop.table(2)*100 %>%
  tidy()


  round(.,1) %>%
  slice(4:5) %>%
  colSums()

head(dat)
