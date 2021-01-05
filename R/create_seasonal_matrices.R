##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param blank_diet_matrices
##' @param gut_lists
create_seasonal_matrices <- function(blank_diet_matrices, gut_lists) {
  ## ++++ Helper functions ++++ ##
  
  split_diets <- function(x){
    
    if(any(is.na(x$date))){
      diet_list = vector('list',3L)
      for(i in 1:3){
        diet_list[[i]] <- x
      }
      return(diet_list)
    } else {
      diet_list = x %>%
        dplyr::mutate(month_id = match(month_id, month.abb),
                      yr_third = case_when(month_id %in% 1:4 ~ "first",
                                           month_id %in% 5:8 ~ "second",
                                           month_id %in% 9:12 ~ "third")) %>%
        ungroup %>% group_split(yr_third)
      
      if(length(diet_list) < 3){
        list.length = length(diet_list)
        to_add = 3-list.length
        for(i in 1:to_add){
          diet_list[[(i+list.length)]] <- x %>% 
            group_by(site, date, taxon, diet_item) %>% 
            dplyr::summarise(rel_area = mean(rel_area))
        }
      }
      
      return(diet_list)
    }
  }
  
  merge_matrices = function(diet,mat,...){
    tax_diet = diet %>%
      group_by(taxon, diet_item) %>%
      dplyr::summarise(rel_area = mean(rel_area))
    
    for(i in 1:nrow(tax_diet)){
      taxon = tax_diet$taxon[i]
      taxa_list = agrep(taxon, colnames(mat))
      diet_item = tax_diet$diet_item[i]
      mat[rownames(mat) %in% diet_item, taxa_list] <- tax_diet$rel_area[i]
    }
    return(mat)
  }
  
  ## ++++ End Helper functions ++++ ##
  diets = gut_lists[['diet_list']]
  diet_splits = diets %>% map(~split_diets(.x))
  
  ## combine the blank matrices and the seasonal matrices
  filled_matrices = vector('list', length = length(diet_splits))
  filled_matrices = filled_matrices %>% setNames(., nm = levels(stream_order)) %>% rlist::list.subset(names(stream_order_list))
  for(i in 1:length(filled_matrices)){
    x = diet_splits[[i]]
    y = blank_diet_matrices[[i]]
    
    filled_matrices[[i]] <- map(x, ~merge_matrices(.x,y))
  }
  return(filled_matrices = filled_matrices)

}
