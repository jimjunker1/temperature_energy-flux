##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param skew_analysis
##' @param random_rankings
plot_random_skew <- function(skew_analysis, random_rankings) {
  
  random_summ_pb = random_rankings %>% group_by(site) %>%
    dplyr::summarise(across(.fns = list(mean = ~mean(.x, na.rm = TRUE),
                                             quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                             quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                             quant50 = ~quantile(.x, 0.50, na.rm = TRUE),
                                             quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                             quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE)))) %>%
    dplyr::select(site, matches("mean|quant2.5|quant50|quant97.5")) %>%
    pivot_longer(-site,names_to = 'variable', values_to = 'value') %>%
    dplyr::mutate(stat = stringr::str_extract_all(variable, '(?<=skew_)(.*)','\\1'),
                  group = "random", 
                  measure = "pb" )
  random_summ_M = random_summ_pb %>% dplyr::mutate(measure = "M")
  random_summ = random_summ_M %>% bind_rows(random_summ_pb)

  skew_long = skew_analysis %>% rlist::list.subset(grepl("summ",names(.), ignore.case = TRUE)) %>% 
    map(~.x %>% select(site, matches("mean|quant2.5|quant50|quant97.5")) %>% select(-matches("prob"))) %>%
    bind_rows %>% pivot_longer(-site, names_to = 'variable', values_to = 'value') %>%
    na.omit %>%
    dplyr::mutate(stat = stringr::str_extract_all(variable, '(?<=skew_)(.*)','\\1'),
                  group = "empirical",
                  # stat = stringr::str_remove_all(stat, 'prob_'),
                  measure = stringr::str_extract_all(variable, '(^[:alpha:]{1,2})'),
                  measure = unlist(measure)) %>%
    bind_rows(random_summ) %>%
    select(site, measure, group, stat, value) %>% 
    pivot_wider(names_from = 'stat', values_from = 'value') %>%
    dplyr::mutate(site = factor(site, levels = rev(names(stream_order_list))))
    
skew_long %>%
  ggplot(aes(y = mean, x = site)) +
  geom_errorbar(aes(ymin = quant2.5, ymax = quant97.5, color = site), width= 0, size = 1, position = position_dodge()) +
  geom_point(aes(color = site, shape = group), size = 3, position = position_dodge(), ) +
  scale_color_manual(values = rev(ocecolors[["temperature"]][oce_temp_pos]), labels = rev(stream_temp_labels))+
  facet_grid(~measure)
  
}
