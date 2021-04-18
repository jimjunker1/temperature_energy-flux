##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param spp_rankings
plot_trait_histogram <- function(spp_rankings = spp_rankings_summary) {
  
  prod_df = spp_rankings[['prod_spp_rank']] %>% dplyr::filter(prod_mg_m_y_mean > 0 | is.na(prod_mg_m_y_mean)) %>% dplyr::mutate(site = factor(site, levels = rev(stream_order)))
  pb_df = spp_rankings[['PB_spp_rank']] %>%  dplyr::filter(pb_y_mean >0 | is.na(pb_y_mean)) %>% dplyr::mutate(site = factor(site, levels = rev(stream_order)))
  bio_df = spp_rankings[['bio_spp_rank']] %>% dplyr::filter(bio_mg_m_mean >0 | is.na(bio_mg_m_mean)) %>% dplyr::mutate(site = factor(site, levels = rev(stream_order)))
  M_df = spp_rankings[['M_spp_rank']] %>% dplyr::filter(M_mg_ind_mean >0 | is.na(M_mg_ind_mean)) %>% dplyr::mutate(site = factor(site, levels = rev(stream_order)))
  
  pb_boxplot = ggplotGrob(ggplot(pb_df,aes(x = site, y = log10(pb_y_mean))) + geom_violin(aes(fill = site), alpha = 0.5) +
    geom_jitter(aes( fill = site),color = 'black', size = 2, shape = 21, alpha = 0.5) +
    scale_fill_manual(values = rev(ocecolors[['temperature']][oce_temp_pos]), labels = rev(stream_temp_labels)) +
    theme(axis.title.x = element_blank(), legend.title = element_blank(), legend.position = c(0,0), legend.justification = c(0,0),
          legend.background = element_rect(fill = 'transparent', colour = NA)))
  # pb_boxplot
  
  M_boxplot = ggplotGrob(ggplot(M_df,aes(x = site, y = log10(M_mg_ind_mean))) + geom_violin(aes(fill = site), alpha = 0.5) +
    geom_jitter(aes( fill = site),color = 'black', size = 2, shape = 21, alpha = 0.5) +
    scale_fill_manual(values = rev(ocecolors[['temperature']][oce_temp_pos]), labels = rev(stream_temp_labels)) +
    theme(axis.title.x = element_blank(), legend.title = element_blank(), legend.position = "none"))
  
  bio_boxplot = ggplotGrob(ggplot(bio_df,aes(x = site, y = log10(bio_mg_m_mean))) + geom_violin(aes(fill = site), alpha = 0.5) +
    geom_jitter(aes( fill = site),color = 'black', size = 2, shape = 21, alpha = 0.5) +
    scale_fill_manual(values = rev(ocecolors[['temperature']][oce_temp_pos]), labels = rev(stream_temp_labels)) +
    theme(axis.title.x = element_blank(), legend.title = element_blank(),legend.position = "none"))
  
  return(grid.arrange(pb_boxplot, M_boxplot, bio_boxplot, ncol = 3))

}
