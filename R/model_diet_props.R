##' This code builds a multilevel model for diet proportions across sites. It allows for species- and period-level
##' modifications. To allow for uncertainty to be propagated through to OM flux estimates, it employs a hierarchical 
##' bayesian model in brms
##'
##' .. content for \details{} ..
##'
##' @title
##' @param gut_lists
model_diet_props <- function(gut_lists) {
  
  ## ++++ Helper functions ++++ ##
  convert_month_num <- function(x){
    x %>% dplyr::mutate(month_id = match(month_id, month.abb),
                        yr_third = case_when(month_id %in% 1:4 ~ 'first',
                                             month_id %in% 5:8 ~ 'second',
                                             month_id %in% 9:12 ~ 'third'))
  }

  ## ++++ End Helper functions ++++ ##
  
    # convert gut lists into wide format for each site
  wide_gut_lists = gut_lists[["diet_list"]] %>% 
    map(~convert_month_num(.x)) %>%
    map(~.x %>% ungroup %>% 
          select(site, yr_third, taxon, id, diet_item, rel_area) %>%
          group_by(site, yr_third, id, taxon) %>% 
          pivot_wider(names_from = 'diet_item', values_from = 'rel_area', values_fn = max)) %>%
    rlist::list.subset(names(stream_order_list))
  
  wide_guts_lists_nopred = wide_gut_lists %>%
    map(~.x %>%
          dplyr::filter(animal <= 0.99)) %>%
    # map( ~ if(length(.x) == 10){.x %>% dplyr::mutate(green_algae = NA_real_)} else{.x = .x}) %>% 
    bind_rows %>%
    ungroup %>% select(-id) %>% dplyr::filter(site %ni% c('st7','oh2'))
  
  diet_meta = wide_guts_lists_nopred %>% ungroup %>% select(site, yr_third, taxon)
  diet_props = wide_guts_lists_nopred %>% ungroup %>% select(-site:-taxon) %>% as.matrix
  apply(diet_props,1,sum)
  
  diet_df = diet_meta;diet_df$props = diet_props
  
  #### full brm diet model ###
  model_formula <- brms::bf(props ~ site + yr_third + taxon)
  model_prior = brms::get_prior(model_formula, 
                    data = diet_df,
                    family = dirichlet())
  conflicted:::conflicts_reset()
    set.seed(123)
  diet_model <- brm(model_formula,
                    data = diet_df, 
                    family = dirichlet(),
                    prior = model_prior)
  return(list(diet_model = diet_model, diet_df = diet_df))
}
