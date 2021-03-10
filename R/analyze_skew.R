##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param lorenz_analysis
##' @param random_rankings
analyze_skew <- function(lorenz_analysis, random_rankings) {

  ## ++++ Helper functions ++++ ##
  skew_est = function(df, x,y){
    site_name = unique(df$site)
    # boot_id = unique(df$boot_id)
    skew_name = paste0(as.character(y),'_skew')
    y = df %>% dplyr::select(all_of(y)) %>% na.omit %>% unlist
    x = df %>% dplyr::select(all_of(x)) %>% na.omit %>% dplyr::filter(. > 0) %>% unlist
    
    y1 = quantile(y, 0.25, na.rm = TRUE)
    y2 = quantile(y, 0.5, na.rm = TRUE)
    y3 = quantile(y, 0.75, na.rm = TRUE)
    
    y1_min = which.min(abs(y - y1))
    y2_min = which.min(abs(y - y2))
    y3_min = which.min(abs(y - y3))
    
    x1 = unname(x[y1_min])
    x2 = unname(x[y2_min])
    x3 = unname(x[y3_min])
    
    skew = (x3 - 2*x2 + x1)/(x3-x1)
    assign(skew_name, skew)
    z = data.frame(site = site_name, eval(as.symbol(skew_name)))
    colnames(z)[2] <- skew_name 
    return(z)
  }
  
  ## ++++ End Helper functions ++++ ##
   # debugonce(skew_est)
 
  PB_skew_boots = lorenz_analysis[["pb_ranking_boots"]] %>%
    named_group_split(site) %>%
    map(~.x %>% named_group_split(boot_id) %>%
          map(~.x %>% skew_est(., 'rel_flux', 'pb_y')) %>% 
          bind_rows(.id = 'boot_id')) %>%
    bind_rows(.id = 'site')
  
  M_skew_boots = lorenz_analysis[["M_ranking_boots"]] %>%
    named_group_split(site) %>%
    map(~.x %>% named_group_split(boot_id) %>%
          map(~.x %>% skew_est(., 'rel_flux', 'M_mg_ind')) %>%
          bind_rows(.id = 'boot_id')) %>%
    bind_rows(.id = 'site')
  
  return(list(pb_skew_boots = PB_skew_boots, M_skew_boots = M_skew_boots))
}
