x = x %>% dplyr::filter(site == 'hver')
y = spp_rankings_summary[[5]] %>% dplyr::filter(site == 'hver')
z = spp_rankings_summary[[5]] %>% dplyr::filter(site == "st14")

mu = mean(y$pb_y_mean, na.rm = TRUE)

sum(na.omit(y$pb_y_mean < mu))

mu_z = mean(z$pb_y_mean, na.rm = TRUE)

sum(na.omit(z$pb_y_mean < mu_z))
21/27
22/27

(mu_z- 0.7778)/(0.786-0.7778)
21+