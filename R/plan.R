the_plan <-
  drake_plan(
   ## Plan targets in here.
   ### environmental data
  environment_data = read_clean_temperature(),
  
   ### production data
  production_boots = read_production(),
  
  production_summaries = summarise_production(production_boots),
  
  seasonal_fluxes = summarise_seasonal_fluxes(production_boots[["int_spp_boots"]]),
  
   ### diet data
  gut_lists = read_clean_guts(),
  
  # gut_df = create_gut_df(gut_lists),
  
  # gut_summaries = summarise_guts(gut_df),
  
  diet_matrices = create_diet_matrices(gut_lists, production_summaries[["production_spp_list"]]),
  
  # flux_estimates = estimate_flux(diet_matrices, seasonal_fluxes),
  
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
