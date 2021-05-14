##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param ann_comm_boots
##' @param stream_gini_df
##' @param diet_similarity_mat
analyze_temp_stats <- function(ann_comm_boots = production_boots[["ann_comm_boots"]],
                               stream_gini_df = gini_analysis[["stream_gini_df"]],
                               diet_similarity_mat = diet_similarity[['among_modeled_overlap']],
                               n_boot = 1e5) {
  
  stream_temps = stream_temp_labels %>% data.frame %>%
    rownames_to_column('site_id') %>% setNames(., c('site_id', 'tempC')) %>%
    dplyr::mutate(tempC = as.numeric(tempC))
  
  set.seed(123)
  boots_df = ann_comm_boots %>%
     map(~.x %>% slice_sample(n = n_boot, replace = TRUE) %>%
         dplyr::mutate(n_rep = 1:n())) %>% bind_rows %>%
    left_join(stream_temps)
  
  pb_temp_boots = boots_df %>%
    group_by(n_rep) %>%
    do(model = lm(log(pb_y) ~ tempC, data = .))
 
  pb_temp_coefs = pb_temp_boots %>%
    purrr::pmap_dbl(~..2 %>% pluck('coefficients') %>% pluck('tempC'))
  
  M_temp_boots = boots_df %>%
    group_by(n_rep) %>%
    do(model = lm(log(M_mg_ind) ~ tempC, data = .))
  
  M_temp_coefs = M_temp_boots %>%
    purrr::pmap_dbl(~..2 %>% pluck('coefficients') %>% pluck('tempC'))
  
  #create df of diet_similarity
  diet_similarity_df= diet_similarity_mat %>%
    future_map(~.x %>% pluck('overlapmatrix') %>% as.data.frame %>%
          setNames(., nm = names(stream_order_list)) %>%
          dplyr::mutate(site = names(stream_order_list)) %>%
        column_to_rownames("site")) %>%
          future_map(function(x){ 
            y = as.matrix(x)
            y[lower.tri(y)]<- NA
            y %>% data.frame %>%
              rownames_to_column("site1") %>%
              pivot_longer(-site1, names_to = 'site2', values_to = 'overlap') %>%
              na.omit}) %>%
    bind_rows(.id = "boot_n")
  
   #create temp distance df
  site_combns = expand.grid(site1 = stream_order, site2 = stream_order) %>%
    left_join(stream_temps %>% dplyr::rename(site1 = 'site_id')) %>%
    left_join(stream_temps %>% dplyr::rename(site2 = 'site_id'), by = 'site2') %>%
    setNames(.,nm = c('site1','site2','tempC_1','tempC_2')) %>%
    dplyr::filter(tempC_1 > tempC_2) %>%
    dplyr::mutate(temp_diff = tempC_1 - tempC_2)

  # join temp distance and diet overlap
  distance_similarity = diet_similarity_df %>%
    left_join(site_combns %>% dplyr::select(site1,site2,temp_diff), by = c("site1","site2"))
  

  return(list(pb_temp_coefs = pb_temp_coefs, M_temp_coefs = M_temp_coefs, n_boot = n_boot, distance_similarity = distance_similarity))
}
