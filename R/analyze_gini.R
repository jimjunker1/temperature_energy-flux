##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param spp_rankings
analyze_gini <- function(spp_rankings) {

  spp_ginis = spp_rankings %>%
    map(~.x %>% 
          map(~.x %>% group_by(site_id) %>%
          dplyr::mutate(across(contains('mean'),  ~ .x/sum(.x), .names = "{.col}_relative")) %>%
            dplyr::arrange(across(contains('rank'))) %>%
            dplyr::mutate(across(contains('relative'), ~cumsum(.x), .names = "{.col}_cumulative"))))
  
}
