##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param season_spp_split
split_seasonal_fluxes <- function(season_spp_split =
                                  seasonal_fluxes[["season_spp_boots_split"]]) {

 flux_boot_split = map(season_spp_split, ~.x %>% map(~.x %>% group_split(boot_id)))
 
 return(flux_boot_split)
 
}
