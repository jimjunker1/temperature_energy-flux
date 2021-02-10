##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param lorenz_analysis
plot_trait_lorenz <- function(lorenz_analysis, spp_rankings_summary) {
    
 # spp_rankings_summary = map(spp_rankings_summary, ~.x %>% bind_rows)

 lorenz_analysis[["relative_flux_df"]] %>%
    ggplot(aes(x = rel_spp, y = rel_flux)) +
    geom_abline(color = "black")+
    geom_abline(intercept = 1, slope = -1, color = "black", linetype = "dotted")+
    geom_line(aes(group = boot_id), size = 1, color = 'grey', alpha = 0.5) +
    geom_line(data = lorenz_analysis[["relative_flux_summary"]], aes(x = rel_spp, y = rel_flux, color = site), size = 1.5)+
    scale_x_continuous(name = "Cumulative species", limits = c(0,1), expand = c(0,0.03))+
    scale_y_continuous(name = "Cumulative flux", limits = c(0,1), expand = c(0,0.03))+
    scale_colour_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
    theme_tufte(ticks = TRUE) +
    geom_rangeframe(sides = "lb")+
    theme(legend.position = c(0,1),
          legend.justification = c("left", "top"), 
          legend.title = element_blank(),
          legend.spacing.y = unit(0.0003,'cm'),
          legend.key.size = unit(0.4,'cm'),
          strip.text = element_blank()) +
    facet_wrap(~site, ncol = 2) -> lorenz_flux_plot
    
    lorenz_analysis[["pb_ranking_boots"]] %>%
        ggplot(aes(x = rel_spp, y = rel_flux)) +
        geom_abline(color = "black")+
        geom_abline(intercept = 1, slope = -1, color = "black", linetype = "dotted")+
        geom_line(aes(group = boot_id), color = "grey", alpha = 0.5) +
        geom_line(data = spp_rankings_summary[["PB_spp_rank"]], aes(x = rel_spp, y = rel_flux, color = site), size = 1.5) +
        scale_x_continuous(name = "Cumulative species", limits = c(0,1), expand = c(0,0.03))+
        scale_y_continuous(name = "Cumulative flux", limits = c(0,1), expand = c(0,0.03))+
        scale_colour_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
        theme_tufte(ticks = TRUE) +
        geom_rangeframe(sides = "lb")+
        theme(legend.position = c(0,1),
              legend.justification = c("left", "top"), 
              legend.title = element_blank(),
              legend.spacing.y = unit(0.0003,'cm'),
              legend.key.size = unit(0.4,'cm'),
              strip.text = element_blank()) +
        facet_wrap(~site, ncol = 2) -> pb_lorenze_flux_plot
    
    lorenz_analysis[["M_ranking_boots"]] %>%
        ggplot(aes(x = rel_spp, y = rel_flux)) +
        geom_abline(color = "black")+
        geom_abline(intercept = 1, slope = -1, color = "black", linetype = "dotted")+
        geom_line(aes(group = boot_id), color = "grey", alpha = 0.5) +
        geom_line(data = spp_rankings_summary[["M_spp_rank"]], aes(x = rel_spp, y = rel_flux, color = site), size = 1.5) +
        scale_x_continuous(name = "Cumulative species", limits = c(0,1), expand = c(0,0.03))+
        scale_y_continuous(name = "Cumulative flux", limits = c(0,1), expand = c(0,0.03))+
        scale_colour_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
        theme_tufte(ticks = TRUE) +
        geom_rangeframe(sides = "lb")+
        theme(legend.position = c(0,1),
              legend.justification = c("left", "top"), 
              legend.title = element_blank(),
              legend.spacing.y = unit(0.0003,'cm'),
              legend.key.size = unit(0.4,'cm'),
              strip.text = element_blank()) +
        facet_wrap(~site, ncol = 2) -> M_lorenze_flux_plot
    
    lorenz_analysis[["bio_ranking_boots"]] %>%
        ggplot(aes(x = rel_bio, y = rel_flux)) +
        geom_abline(color = "black")+
        geom_abline(intercept = 1, slope = -1, color = "black", linetype = "dotted")+
        geom_line(aes(group = boot_id), color = "grey", alpha = 0.5) +
        geom_line(data = spp_rankings_summary[["bio_spp_rank"]], aes(x = rel_bio, y = rel_flux, color = site), size = 1.5) +
        scale_x_continuous(name = "Cumulative Biomass", limits = c(0,1), expand = c(0,0.03))+
        scale_y_continuous(name = "Cumulative flux", limits = c(0,1), expand = c(0,0.03))+
        scale_colour_manual(values = ocecolors[['temperature']][oce_temp_pos], labels = stream_temp_labels)+
        theme_tufte(ticks = TRUE) +
        geom_rangeframe(sides = "lb")+
        theme(legend.position = c(0,1),
              legend.justification = c("left", "top"), 
              legend.title = element_blank(),
              legend.spacing.y = unit(0.0003,'cm'),
              legend.key.size = unit(0.4,'cm'),
              strip.text = element_blank()) +
        facet_wrap(~site, ncol = 2) -> bio_lorenze_flux_plot
        
    
    return(list(lorenz_flux_plot = lorenz_flux_plot, pb_lorenze_flux_plot = pb_lorenze_flux_plot, M_lorenze_flux_plot = M_lorenze_flux_plot, bio_lorenze_flux_plot = bio_lorenze_flux_plot))
    
}
