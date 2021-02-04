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
  
  spp_rankings = rank_spp_traits(production_summaries),
  
  # gini_analysis = analyze_gini(spp_rankings),

  # target_name = target(
  #   command = {
  #     rmarkdown::render(knitr_in("docs/prelim-doc.Rmd"))
  #     file_out("docs/prelim-doc.html")
  #   }
  # )  
)
