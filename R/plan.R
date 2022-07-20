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
  flux_summaries = summarise_fluxes(flux_estimates[["flux_full"]], production_boots[["int_spp_meta"]]),
  #
  taxonomic_info = get_taxa_info(ann_spp_flux =
                                   flux_summaries[["annual_spp_flux_summary"]]), 
   ### trait analyses
  #
  spp_rankings_boots = rank_spp_boots(production_boots[['ann_spp_boots']]),
  #
  spp_rankings_summary = rank_spp_traits(production_summaries[["ann_spp_summary"]], flux_summaries[["annual_spp_flux_summary"]]),
  #
  random_rankings = create_random_ranks(flux_summaries[["annual_spp_flux_boots"]], n = 1e5),
  #
  gini_analysis = analyze_gini(flux_summaries[["annual_spp_flux_boots"]]),
  #
  lorenz_analysis = analyze_lorenz(flux_summaries[["annual_spp_flux_boots"]],
                                   spp_rankings_boots),
  #lorenz asymmetry analysis
  # lorenz_asym_analysis = analyze_lorenz_asymmetry(lorenz_analysis),
  #skew
  skew_analysis = analyze_skew(lorenz_analysis, random_rankings),
  # diversity analyses
  hill_diversity_analysis = analyze_hill_diversity(flux_summaries[["annual_spp_flux_summary"]]),
  # community diversity change through time
  diversity_analysis = analyze_diversity(production_boots),
  #partial dominance
  partial_dominance_analysis = analyze_dominance(flux_summaries[["annual_spp_flux_boots"]]),
  #
  #diet overlap
  diet_similarity = analyze_diet_similarity(gut_lists[["diet_list"]],
                                            modeled_diets[["diet_predictions"]]),
  # trait interrelationships
  trait_stats = analyze_trait_stats(skew_analysis),
  # temperature trait relationships
  temperature_stats = analyze_temp_stats(production_boots[['ann_comm_boots']],
                                         production_boots[['ann_spp_boots']],
                                         gini_analysis[['stream_gini_df']],
                                         diet_similarity[['among_modeled_overlap']],
                                         skew_analysis, n_boot = 1e3),
  conflicted:::conflicts_register(),
  
  ### figures
  #
  prelim_diet_figures = plot_stream_diets(modeled_diets[["diet_predictions"]]),#figure s1
  #
  diet_similarity_analysis = plot_diet_similarity(diet_similarity),
  #
  annual_spp_flux_fig = plot_spp_flux(flux_summaries[["annual_spp_flux_summary"]], 
                                      environment_data[["stream_temp_labels"]]),
  #
  # evenness_profile_fig = plot_evenness_profile(hill_diversity_analysis),
  #
  spp_trait_histograms = plot_trait_histogram(spp_rankings_summary),
  #
  spp_traitsVtemp = plot_traits_temp(production_boots, spp_rankings_summary, temperature_stats),
  #
  spp_flux_dist = plot_spp_relFlux(flux_summaries[["annual_spp_flux_summary"]],taxonomic_info),
  #
  lorenz_trait_fig = plot_trait_lorenz(lorenz_analysis, spp_rankings_summary),
  #
  random_skew_fig = plot_random_skew(skew_analysis, random_rankings),
  # 
  temperature_skew_fig = plot_skew_temperature(temperature_stats, n_id = 2e3),
  #
  # diversity_fig = plot_comm_diversity(diversity_analysis),
  # pbM_fig = plot_spp_ann_trait(production_summaries[['ann_spp_summary']]),
  
  # target_name = target(
    # command = {
      # rmarkdown::render(knitr_in("doc/prelim-doc.Rmd"))
      # file_out("doc/prelim-doc.html")
    # }
  # ),
  # 
  # ms_file = target(
  #   command = {
  #     rmarkdown::render(knitr_in("doc/./ms/ms.Rmd"))
  #     file_out("doc/./ms/ms.docx")
  #   }
  # )
  # 
  # figure_file = target(
  #   command = {
  #     rmarkdown::render(knitr_in("doc/./ms/tables-figures.Rmd"))
  #     file_out("doc/./ms/tables-figures.docx")
  #   }
  # )
  
  # target_name = target(
  #   command = {
  #     rmarkdown::render(knitr_in("doc/./coherence_ms/coherence_ms.Rmd"))
  #     file_out("doc/./coherence_ms/coherence_ms.docx")
  #   }
  # )
)
