# data scrub for wyatt
prod_full_df = join(prod_df %>% select(site, taxon, prod_mg_m_y_mean), bio_df %>% select(site, taxon, bio_mg_m_mean), by = c("site","taxon"))
 prod_full_df = join(prod_full_df, pb_df %>% select(site, taxon, pb_y_mean))
 prod_full_df = join(prod_full_df, M_df %>% select(site, taxon, M_mg_ind_mean))
 
 prod_full_df = prod_full_df %>% dplyr::filter(!is.na(taxon)) %>% dplyr::mutate(mean_tempC = NA, mean_tempC = recode(site, !!!stream_temp_labels))

 write.csv(prod_full_df, "./prod_full_for-wyatt.csv", row.names = FALSE) 
 