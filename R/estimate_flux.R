##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param diet_matrices
##' @param seasonal_fluxes
estimate_flux <- function(diet_matrices, seasonal_fluxes) {
 
  # resource date frame with efficiences
  diet_item = list("amorphous_detritus",
                "cyanobacteria",
                "diatom",
                "filamentous",
                "green_algae",
                "plant_material",
                "animal")
 
   efficiencies = list(
     c(0.08, 0.1, 0.12),
     c(0.08, 0.1, 0.12),
     c(0.24, 0.3, 0.36),
     c(0.24, 0.3, 0.36),
     c(0.24, 0.3, 0.36),
     c(0.08, 0.1, 0.12),
     c(0.56, 0.7, 0.84)
  )
  resource_keyval = setNames(efficiencies, nm = unlist(diet_item))

 # estimate the beta distributions to draw diet AEs from
   set.seed(123)
   beta_dist.pars = map(efficiencies, ~rriskDistributions::get.beta.par(p = c(0.025,0.5,0.975), q = unlist(.x), plot = FALSE, show.output = FALSE))
   res_effs = map(beta_dist.pars, ~rbeta(1e3, shape1 = .x[1], shape2 = .x[2])) %>% setNames(., nm = unlist(diet_item))

 #NPE quantile 0.025,0.5,0.975
   NPE_dist.pars = rriskDistributions::get.beta.par(p = c(0.025,0.5,0.975),q = c(0.4,0.45,0.5), show.output = FALSE, plot = FALSE)
   NPE = rbeta(1e3, shape1 = NPE_dist.pars[1], shape2 = NPE_dist.pars[2])
 # combined the production from Jan-Apr, May-Aug, Sep-Dec
  boot_flux_function = function(mat_list, losses,resources_mat, efficiences_vct, NPE,...){
     
     
    colnames(mat_list) %>% tibble %>%
      setNames(., nm = 'matrix_name') %>%
      dplyr::mutate(efficiencies = dplyr::recode(matrix_name, !!!resource_keyval),
                    efficiencies = tidyr::replace_na(efficiencies, 0.7)) -> matrix_effiencies
    boots = mat_list
    
  }

}