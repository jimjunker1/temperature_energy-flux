##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param gut_lists
##' @param spp_list
##' @param diet_model
create_diet_df <- function(gut_lists, spp_list =
                           production_summaries[["production_spp_list"]],
                           diet_model = modeled_diets[["diet_model"]]) {

  # period list 
  period_list = list(c('first','second','third'))
  
  spp_list = spp_list %>% rlist::list.subset(names(stream_order_list))
  
  resource_list = gut_lists[["resource_list"]] %>% rlist::list.subset(names(stream_order_list))
  
  stream_diet_dfs = pmap(list(spp_list, resource_list, period_list), ~expand.grid(taxon = unlist(..1),
                                                                                  diet_item = unlist(..2), 
                                                                                  yr_third = unlist(..3),
                                                                                  KEEP.OUT.ATTRS = FALSE,
                                                                                  stringsAsFactors = FALSE) %>%
                           dplyr::mutate(value = NA_real_))
  
  return(stream_diet_dfs = stream_diet_dfs)
  
}
