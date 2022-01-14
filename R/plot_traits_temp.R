#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param spp_rankings_summary
#' @param temperature_stats
plot_traits_temp <- function(production_boots,
                             spp_rankings_summary,
                             temperature_stats) {

  stream_temps = stream_temp_labels %>% data.frame %>%
    rownames_to_column('site_id') %>% setNames(., c('site_id', 'tempC')) %>%
    dplyr::mutate(tempC = as.numeric(tempC))
  
  new_data = data.frame(tempC = seq((min(stream_temps$tempC)-0.5),(max(stream_temps$tempC)+0.5), length.out = 100)) %>%
    dplyr::mutate(tempC_stand = tempC - mean(tempC, na.rm = TRUE))
  
  ann_spp_summ = temperature_stats[["sppBootsDf"]] %>%
    group_by(site_id) %>%
    dplyr::summarise(across(c(pb_y,M_mg_ind), list(mean = ~mean(.x, na.rm = TRUE),
                                                   median = ~median(.x, na.rm = TRUE),
                                                   mode = ~rethinking::chainmode(.x, na.rm = TRUE),
                                                   quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                                   quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                   quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                   quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE))))
  pb_spp_temp_pred = pb_spp_temp_boots %>%
    junkR::named_group_split(n_rep) %>%
    purrr::map(~.x %>% pluck('model'))# %>% predict(unlist(.), newdata = new_data)) %>%
    bind_rows(.id = n_rep)
    purrr::pmap()
 

}
