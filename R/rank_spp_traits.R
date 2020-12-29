##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param production_summaries
rank_spp_traits <- function(production_summaries) {
  ann_spp_summary = production_summaries[["ann_spp_summary"]]
  # create ranked lists for analysis
  # # sum across intervals for each stream
  prod_spp_rank = ann_spp_summary %>%
    junkR::named_group_split(site_id) %>%
    purrr::map(~.x %>%
          dplyr::select(site_id, taxon_id, matches('prod.*_mean')) %>%
          dplyr::mutate(prod_ann_rank = dense_rank(desc(prod_mg_m_y_mean))))
  # biomass
  # # sum across intervals for each stream
  bio_spp_rank = ann_spp_summary %>%
    junkR::named_group_split(site_id) %>%
    purrr::map(~.x %>%
          dplyr::select(site_id, taxon_id, matches('bio.*_mean')) %>%
          dplyr::mutate(bio_ann_rank = dense_rank(desc(bio_mg_m_mean))))
  # abundance
  # # sum across intervals for each stream
  n_spp_rank = ann_spp_summary %>%
    junkR::named_group_split(site_id) %>%
    purrr::map(~.x %>%
          dplyr::select(site_id, taxon_id, matches('n.*_mean')) %>%
          dplyr::mutate(n_ann_rank = dense_rank(desc(n_ind_m_mean))))
  
  # body size, *M*
  M_spp_rank = ann_spp_summary %>%
    junkR::named_group_split(site_id) %>%
    purrr::map(~.x %>%
          dplyr::select(site_id, taxon_id, matches('M.*_mean')) %>%
          dplyr::mutate(M_ann_rank = dense_rank(desc(M_mg_ind_mean))))
  
  # PB ratio
  PB_spp_rank = ann_spp_summary %>%
    junkR::named_group_split(site_id) %>%
    purrr::map(~.x %>%
          dplyr::select(site_id, taxon_id, matches('pb.*_mean')) %>%
          dplyr::mutate(pb_ann_rank = dense_rank(desc(pb_y_mean))))
  
  return(list( prod_spp_rank = prod_spp_rank, bio_spp_rank = bio_spp_rank, n_spp_rank = n_spp_rank, M_spp_rank = M_spp_rank,PB_spp_rank = PB_spp_rank))

}
