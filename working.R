source("packages.R")
drake::loadd("production_boots")
st9_prod = readRDS("./ignore/st9_out_full_mod.rds")
prod_full_fin = readRDS("./data/derived-data/prod_full_fin.rds")

rowSums(st9_prod$Pintboots.cob, na.rm = T) %>% round(2) %>% sum
rowSums(st9_prod$Bintboots.cob, na.rm = TRUE) %>% round(2) %>% sum

st9_dates = readRDS("C:/Users/jrjunker/Desktop/st9_temps.rds") %>% dplyr::select(DATE) %>% purrr::reduce(c)

st9_production_boots = list()

st9_int_comm_prod = st9_prod[["Pintboots.full"]] %>%
  rlist::list.subset(lengths(.) > 1) %>%
  purrr::map(~.x %>% data.frame %>% setNames(., nm = st9_dates) %>%
  dplyr::mutate(n = 1:n(),
                boot_id = paste0("X",n)) %>%
    dplyr::select(-n)) %>% bind_rows(.id = 'taxon_id') %>%
  ungroup %>% group_by(boot_id) %>% 
  pivot_longer(-c(boot_id,taxon_id), names_to = 'date_id', values_to = 'prod_mg_m_int') %>%
  dplyr::mutate(site_id = "st9") %>%
  dplyr::select(taxon_id, date_id, boot_id, prod_mg_m_int, site_id)

st9_int_comm_bio = st9_prod[["Bintboots.full"]] %>%
  rlist::list.subset(lengths(.) > 1) %>%
  purrr::map(~.x %>% data.frame %>% setNames(., nm = st9_dates) %>%
               dplyr::mutate(n = 1:n(),
                             boot_id = paste0("X",n)) %>%
               dplyr::select(-n)) %>% bind_rows(.id = 'taxon_id') %>%
  ungroup %>% group_by(boot_id) %>% 
  pivot_longer(-c(boot_id,taxon_id), names_to = 'date_id', values_to = 'bio_mg_m')  %>%
  dplyr::mutate(site_id = "st9") %>%
  dplyr::select(taxon_id, date_id, boot_id, bio_mg_m, site_id)

add_date = function(x,...){
  vec1 = x[,1]
  vec2 = x[,dim(x)[2]]
  addVec = purrr::map2_dbl(vec1,vec2, ~mean(c(.x,.y)))
  data.frame(bind_cols(x, addVec))
}

st9_int_comm_n = st9_prod[["Nintboots.full"]] %>%
  rlist::list.subset(lengths(.) > 1) %>%
  purrr::map(~.x %>% data.frame %>% add_date(.) %>%
               setNames(., nm = st9_dates) %>%
               dplyr::mutate(n = 1:n(),
                             boot_id = paste0("X",n)) %>%
               dplyr::select(-n)) %>% bind_rows(.id = 'taxon_id') %>%
  ungroup %>% group_by(boot_id) %>% 
  pivot_longer(-c(boot_id,taxon_id), names_to = 'date_id', values_to = 'n_ind_m')  %>%
  dplyr::mutate(site_id = "st9") %>%
  dplyr::select(taxon_id, date_id, boot_id, n_ind_m, site_id)


st9 = list("Pintboots.full" = st9_int_comm_prod,
           "Bintboots.full" = st9_int_comm_bio,
           "Nintboots.full" = st9_int_comm_n)


prod_full_fin[["st9"]] <- st9

saveRDS(prod_full_fin, "./data/derived-data/prod_full_fin.rds")


# st9_int_comm_boots = st9_int_comm_prod %>%
#   dplyr::mutate(site_id = 'st9') %>%
#   dplyr::select(site_id, date_id, everything()) %>%
#   left_join(st9_int_comm_bio, by = c('date_id','boot_id')) %>%
#   left_join(st9_int_comm_n, by = c('date_id', 'boot_id')) %>%
#   dplyr::mutate(pb_int = prod_mg_m_int/bio_mg_m,
#                 M_mg_ind = bio_mg_m/n_ind_m)
# 
# 
# st9_ann_comm_prod = st9_int_comm_prod %>%
#   ungroup %>% group_by(boot_id) %>%
#   dplyr::summarise(prod_mg_m_y = sum(prod_mg_m_int))
# 
# st9_ann_comm_bio = st9_int_comm_bio %>%
#   ungroup %>% group_by(boot_id) %>%
#   dplyr::summarise(bio_mg_m = sum(bio_mg_m))
# 
# st9_ann_comm_n = st9_int_comm_n %>%
#   ungroup %>% group_by(boot_id) %>%
#   dplyr::summarise(n_ind_m = sum(n_ind_m))
# 
# st9_ann_comm_boots = st9_ann_comm_prod %>%
#   dplyr::mutate(site_id = 'st9') %>%
#   dplyr::select(site_id, everything()) %>%
#   left_join(st9_ann_comm_bio) %>%
#   left_join(st9_ann_comm_n) %>%
#   dplyr::mutate(pb_y = prod_mg_m_y/bio_mg_m,
#                 M_mg_ind = bio_mg_m/n_ind_m)
# 
# 
# st9_production_boots = setNames(st9_production_boots, nm = c("int_comm_boots",
#                                                              ))