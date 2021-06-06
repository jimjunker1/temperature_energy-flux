##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param temperature_stats
plot_skew_temperature <- function(temperature_stats, n_id = 2e3) {
  
  pb_probs_df = temperature_stats[["pb_probs_df"]]
  pb_probs_summ = pb_probs_df %>%
    group_by(site) %>%
    dplyr::summarise(tempC= unique(tempC),
                     across(c(pb_skew_prob), list(mean = ~mean(.x, na.rm = TRUE),
                                                        quant2.5 = ~quantile(.x, 0.025, na.rm = TRUE),
                                                        quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                        quant50 = ~quantile(.x, 0.50, na.rm = TRUE),
                                                        quant75 = ~quantile(.x, 0.75, na.rm = TRUE),
                                                        quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE)))) %>%
    dplyr::mutate(site = factor(site, levels = names(stream_order_list)))

  pb_probs_concave_pred = temperature_stats[["pb_probs_concave_pred"]]
  pb_probs_concave_med = temperature_stats[["pb_probs_concave_med"]]

  set.seed(123)
  pb_probs_plot =
    pb_probs_concave_pred %>% 
    .[sample(1:length(.),n_id, replace = TRUE)] %>%
    setNames(.,nm = seq(1:n_id)) %>%
    bind_rows(.id = "n_id") %>% 
    # dplyr::mutate(site = factor(site, levels = names(stream_order_list))) %>%
  ggplot(aes(x = tempC, y = .fitted, group = n_id)) +
    geom_line(alpha = 0.1, color = 'lightgrey') +
    # geom_line(data = pb_probs_concave_med, aes(x= tempC, y = `.fitted`), color = 'black', size = 3, inherit.aes = FALSE)+
    geom_errorbar(data = pb_probs_summ, aes(x = tempC, ymin = pb_skew_prob_quant2.5, ymax = pb_skew_prob_quant97.5, color = site), size = 1, width = 0, inherit.aes = FALSE)+
    geom_errorbar(data = pb_probs_summ, aes(x = tempC, ymin = pb_skew_prob_quant25, ymax = pb_skew_prob_quant75, color = site), size = 1.5, width = 0, inherit.aes = FALSE)+ 
    geom_point(data = pb_probs_summ, aes(x = tempC, y = pb_skew_prob_mean, fill = site), size = 2, color = 'black', inherit.aes = FALSE) +
    scale_x_continuous(name = expression("Temperature ("~degree*C~")"), expand = c(0,0.004))+
    scale_y_continuous(name = expression("Pr("~italic(Sk[flux])~">="~italic(x)*")"), expand = c(0,0.004)) +
    coord_cartesian(xlim = c(0,30), ylim = c(0,0.5))+
    scale_fill_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
    scale_colour_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
    theme_tufte(ticks = TRUE) +
    geom_rangeframe(sides = "lb")+
    annotate("text", x = 28, hjust = 1, y = 0.15, vjust = 1, angle = 90, label = "Non-random")+
    annotate("segment", x = 29, xend = 29, y = 0.25, yend = 0, size = 1, 
             arrow = arrow(length = unit(0.4,"cm"), type = 'open'))+
    annotate("text", x = 28, hjust = 1, y = 0.4, vjust = 1, angle = 90, label = "Random" )+
    annotate('segment', x = 29, xend = 29, y = 0.27, yend = 0.5, size = 1,
             arrow = arrow(length = unit(0.4, "cm"), type = 'open'))+
    theme(legend.title = element_blank(),
          legend.position = c(0, 0),
          legend.justification = c(0,0))
  
 return(pb_probs_plot)
}
