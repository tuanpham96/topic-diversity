
calc_entropy <- function(df) {
  total_occ <- sum(df$count)
  
  mutate(df, p = count/total_occ) %>% 
    mutate(p = -p*log(p,base=2)) %>% 
    filter(!is.nan(p)) %>% 
    select(p) %>% sum
}