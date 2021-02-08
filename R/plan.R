the_plan <-
  drake_plan(
   ## Plan targets in here.
   ### environmental data
    
  environment_data = read_clean_temperature(),
  
   ### production data
  #
  production_boots = read_production(),
  #
  production_summaries = summarise_production(production_boots),
  #
  seasonal_production = summarise_seasonal_production(production_boots[["int_spp_boots"]]),
  #
  seasonal_boot_split = split_seasonal_production(seasonal_production[["season_spp_boots_split"]]),
  
   ### diet data
  #
  gut_lists = read_clean_guts(),
  #create full data frame of site x yr_third x taxon for diet proportions
  full_diet_df = create_diet_df(gut_lists, production_summaries[["production_spp_list"]]),
  # model diets with missing data included.
  modeled_diets = model_diet_props(full_diet_df),
  # re-establish conflicts
  conflicted:::conflicts_register(),
  #create blank diet matrices to fill in  
  diet_matrices = create_diet_matrices(modeled_diets[['diet_seasonal_boot_split']], seasonal_boot_split),
  
   ### flux estimates, summaries, and figures
  # estimate the fluxes, YEAH!!
  flux_estimates = estimate_flux(diet_matrices, seasonal_boot_split),
  #
  flux_summaries = summarise_fluxes(flux_estimates, production_boots[["int_spp_meta"]]),
  
   ### trait analyses
  #
  spp_rankings_boots = rank_spp_boots(production_boots[['ann_spp_boots']]),
  #
  spp_rankings_summary = rank_spp_traits(production_summaries[["ann_spp_summary"]], flux_summaries[["annual_spp_flux_summary"]]),
  
  gini_analysis = analyze_gini(flux_summaries[["annual_spp_flux_boots"]]),
  #
  lorenz_analysis = analyze_lorenz(flux_summaries[["annual_spp_flux_boots"]], spp_rankings_boots),
  #lorenz asymmetry analysis
  lorenz_asym_analysis = analyze_lorenz_asymmetry(lorenz_analysis),
  
   
   ### figures
  #
  annual_spp_flux_fig = plot_spp_flux(flux_summaries[["annual_spp_flux_summary"]], environment_data[["stream_temp_labels"]]),
  #
  spp_trait_histograms = plot_trait_histogram(spp_rankings_summary),
  #
  lorenz_trait_fig = plot_trait_lorenz(lorenz_analysis, spp_rankings_summary),
  
  # target_name = target(
  #   command = {
  #     rmarkdown::render(knitr_in("docs/prelim-doc.Rmd"))
  #     file_out("docs/prelim-doc.html")
  #   }
  # )  
)
