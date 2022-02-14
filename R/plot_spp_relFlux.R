#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nameme1
plot_spp_relFlux <- function(ann_spp_flux =
                             flux_summaries[["annual_spp_flux_summary"]],
                             taxaDf = taxonomic_info) {
  
  namesDf = taxaDf %>%
    ungroup %>%
    dplyr::mutate(taxaGroup = recode(taxaGroup, Oligochaeta = 'Lumbricidae')) %>%
     dplyr::select(taxon, cleanGroup, taxaGroup) %>% unique
  
  taxaDf %>%
    dplyr::select(site, taxon, cleanGroup, taxaGroup, flux_mg_m_y_mean) %>%
    dplyr::mutate(site = factor(site, levels = stream_order)) %>%
    group_by(site) %>%
    dplyr::mutate(rel_flux_mean = flux_mg_m_y_mean/sum(flux_mg_m_y_mean, na.rm = TRUE)) %>%#,
                  # rel_flux_mean = case_when(rel_flux_mean <0.01 ~ 0.0101,
                  #                           TRUE ~ rel_flux_mean)) %>%
    junkR::named_group_split(site) -> ann_spp_fluxList
  
  
   ann_spp_fluxList = purrr::map(ann_spp_fluxList, function(x){
     # browser()
     df = x
     lowFluxN = x %>% dplyr::filter(rel_flux_mean <= 0.0101) %>% nrow
     other_label = paste0("Other taxa (n = ",lowFluxN,")")
     x %>%
       dplyr::mutate(taxon = case_when(rel_flux_mean > 0.0101 ~ taxon,
                                       TRUE ~ other_label)) %>%
       group_by(site, taxon) %>%
       dplyr::summarise(rel_flux_mean = sum(rel_flux_mean, na.rm = TRUE)) %>%
       left_join(namesDf, by = "taxon") %>%
       dplyr::mutate(rel_flux_mean = case_when(rel_flux_mean < 0.01 ~ 0.0101,
                                               TRUE ~ rel_flux_mean),
                     taxon = factor(taxon,
                                    levels = df %>%
                                      dplyr::filter(taxon != other_label) %>%
                                      dplyr::arrange(rel_flux_mean) %>%
                                      dplyr::select(taxon) %>% flatten %>% unlist %>% c(other_label,.)))

   } )

plotTaxa = ann_spp_fluxList %>%
  purrr::map(~.x %>% ungroup %>%
               dplyr::filter(!grepl('Other',taxon, .)) %>%
               dplyr::select(taxaGroup) %>%
               unique) %>% unlist %>% unique

plotTaxaColors = setNames(c(viridis(12, option = 'D', begin = 1, end = 0),"#D3D3D3"), nm = c(plotTaxa, NA))

 plot_relFlux = function(flux_list = NULL, colors = plotTaxaColors, plot_annotation = FALSE, plot_axes_text = FALSE,...){
   other_label = flux_list$taxon[grepl("Other", unlist(flux_list$taxon))] %>% as.character %>% unique
   names(colors)[length(colors)] = other_label
   
   plotDf = flux_list %>% ungroup %>%
   dplyr::mutate(taxaGroup = case_when(is.na(taxaGroup) ~ as.character(taxon),
                                       TRUE ~ taxaGroup),
                 cleanGroup = case_when(is.na(cleanGroup)~ as.character(taxon),
                                        TRUE ~ cleanGroup),
                 taxon = factor(taxon, levels = flux_list %>%
                                  ungroup %>% dplyr::filter(taxon != other_label) %>%
                                  dplyr::arrange(rel_flux_mean) %>%
                                  dplyr::select(taxon) %>% unlist %>%
                                  as.character %>% c(other_label,.)))
   plot = plotDf %>%
     ggplot() +
     geom_col(aes(x = log10(rel_flux_mean*100), y = taxon, fill = taxaGroup), color = 'black')+
     scale_fill_manual(values = colors[names(colors) %in% unlist(flux_list$taxaGroup)])+
     # scale_color_manual(values = .y)+
     scale_x_continuous(limits = c(0,2), expand = c(0.001,0.001))+
     scale_y_discrete(position = 'right', breaks = plotDf$taxon, labels = plotDf$cleanGroup)+
     theme_tufte()+
     theme(legend.position = 'none',
           axis.title.y = element_blank(),
           axis.title.x = element_blank())
   
   if(plot_annotation){
     plot = plot + annotate('text', label = 'A', x = Inf, y = Inf, family = 'serif',
                     vjust = 1, hjust = 0, size = 4)
   }
   if(!plot_axes_text){
     plot = plot + theme(axis.text.x = element_blank())
   }
   
   return(plot)
 }

 annotateVec = c(T, F, F, F, F, F)
 axesVec = c(F, F, F, F, F, T)
 # debugonce(plot_relFlux)
 ann_spp_plotList = purrr::pmap(list(ann_spp_fluxList,annotateVec,axesVec), 
                                ~plot_relFlux(flux_list = ..1,
                                              plot_annotation = ..2,
                                              plot_axes_text = ..3)) %>%
   purrr::map(~ggplotGrob(.x))
 
 widths = max(ann_spp_plotList[[1]]$widths,
                ann_spp_plotList[[2]]$widths,
                ann_spp_plotList[[3]]$widths,
                ann_spp_plotList[[4]]$widths,
                ann_spp_plotList[[5]]$widths,
                ann_spp_plotList[[6]]$widths
              )
 
 ann_spp_plotList[[1]]$widths -> ann_spp_plotList[[2]]$widths -> ann_spp_plotList[[3]]$widths -> ann_spp_plotList[[4]]$widths -> ann_spp_plotList[[5]]$widths -> ann_spp_plotList[[6]]$widths -> widths
 
 full_plot = gridExtra::grid.arrange(ann_spp_plotList[[1]],
                                     ann_spp_plotList[[2]],
                                     ann_spp_plotList[[3]],
                                     ann_spp_plotList[[4]],
                                     ann_spp_plotList[[5]],
                                     ann_spp_plotList[[6]], ncol = 1,
                                     layout_matrix = rbind(1,
                                                           1,
                                                           2,
                                                           2,
                                                           2,
                                                           3,
                                                           3,
                                                           3,
                                                           4,
                                                           4,
                                                           4,
                                                           4,
                                                           5,
                                                           5,
                                                           5,
                                                           5,
                                                           6,
                                                           6,
                                                           6,
                                                           6),
                                                  bottom = textGrob(expression(""*log[10]*"Relative Organic Matter flux (%)"), 
                                                                               gp = gpar(fontfamily = 'serif')))
 
 return(full_plot)
 
}

# ann_spp_plotList = purrr::map2(ann_spp_fluxList,list(plotTaxaColors),
#                                function(x,y){
#                                  # browser()
#                                  # alter the dataframe 
#                                  other_label = x$taxon[grepl("Other",unlist(x$taxon))] %>% as.character %>% unique
#                                  names(y)[length(y)] <- other_label
#                                  
#                                  plotDf = x %>% ungroup %>%
#                                    dplyr::mutate(taxaGroup = case_when(is.na(taxaGroup) ~ as.character(taxon),
#                                                                        TRUE ~ taxaGroup),
#                                                  cleanGroup = case_when(is.na(cleanGroup) ~ as.character(taxon),
#                                                                         TRUE ~ cleanGroup),
#                                                  taxon = factor(taxon,
#                                                                 levels = x %>% ungroup %>%
#                                                                   dplyr::filter(taxon != other_label) %>%
#                                                                   dplyr::arrange(rel_flux_mean) %>%
#                                                                   dplyr::select(taxon) %>% unlist %>% as.character  %>% c(other_label,.)))
#                                  
#                                  plotDf %>%
#                                    ggplot() +
#                                    geom_col(aes(x = log10(rel_flux_mean*100), y = taxon, fill = taxaGroup), color = 'black')+
#                                    scale_fill_manual(values = y[names(y) %in% unlist(x$taxaGroup)])+
#                                    # scale_color_manual(values = .y)+
#                                    scale_x_reverse(limits = c(2,0), expand = c(0.001,0.001))+
#                                    scale_y_discrete(position = 'right', breaks = plotDf$taxon, labels = plotDf$cleanGroup)+
#                                    theme_tufte()+
#                                    theme(legend.position = 'none',
#                                          axis.title.y = element_blank(),
#                                          axis.title.x = element_blank())
#                                }) %>%
#   purrr::map(~ggplotGrob(.x))