##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param ann_comm_boots
##' @param stream_gini_df
##' @param diet_similarity
analyze_temp_stats <- function(ann_comm_boots = production_boots[["ann_comm_boots"]],
                               stream_gini_df = gini_analysis[["stream_gini_df"]],
                               diet_similarity, n_boot = 1e5) {
  
  stream_temps = stream_temp_labels %>% data.frame %>%
    rownames_to_column('site_id') %>% setNames(., c('site_id', 'tempC'))

  pb_temp_boots = ann_comm_boots %>%
    map(~.x %>% slice_sample(n = n_boot, replace = TRUE) %>%
          dplyr::select(site_id, pb_y)) %>%
    bind_rows %>% left_join(stream_temps) #%>%
    apply(.,1,function(x)
    

}
