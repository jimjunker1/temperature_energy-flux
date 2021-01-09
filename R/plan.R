the_plan <-
  drake_plan(
   ## Plan targets in here.
   ### environmental data
  environment_data = read_clean_temperature(),
  
   ### production data
  production_boots = read_production(),
  
  production_summaries = summarise_production(production_boots),
  
  seasonal_fluxes = summarise_seasonal_fluxes(production_boots[["int_spp_boots"]]),
  
  seasonal_boot_split = split_seasonal_fluxes(seasonal_fluxes[["season_spp_boots_split"]]),
  
   ### diet data
  gut_lists = read_clean_guts(),
  
  # gut_df = create_gut_dists(gut_lists),
  
  # gut_summaries = summarise_guts(gut_df),
  # 
  modeled_diets = model_diet_props(gut_lists),
  
  # re-establish conflicts
  conflicted:::conflicts_register(),
  
  #create full data frame of site x yr_third x taxon for diet proportions
  full_diet_df = create_diet_df(gut_lists, production_summaries[["production_spp_list"]]),
  # predict diets 
  predicted_diets = predict_diets(full_diet_df, modeled_diets),
  #create blank diet matrices to fill in  
  blank_diet_matrices = create_diet_matrices(gut_lists, production_summaries[["production_spp_list"]]),
  
  seasonal_diet_matrices = create_seasonal_matrices(blank_diet_matrices, gut_lists),
  
  filled_diet_matrices = fill_predator_diets(seasonal_diet_matrices),
  
  # flux_estimates = estimate_flux(filled_diet_matrices, seasonal_fluxes),
  
  # spp_rankings = rank_spp_traits(flux_estimates, production_summaries),
  
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
