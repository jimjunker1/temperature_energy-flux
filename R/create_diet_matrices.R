##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param gut_lists
##' @param spp_list
create_diet_matrices <- function(gut_lists, spp_list = production_summaries[["production_spp_list"]]) {
## ++++ Helper functions ++++ ##
  create_matrix = function(spp_list, resource_list){
    # create three matrices for 1:4. 5:8, 9:12 months
    spp_matrix = matrix(0, nrow = nrow(spp_list), ncol = nrow(spp_list))
    rownames(spp_matrix) <- colnames(spp_matrix) <- unlist(spp_list)
    res_matrix = matrix(0, nrow = length(resource_list), ncol = length(resource_list))
    rownames(res_matrix) <- colnames(res_matrix) <- unlist(resource_list)
    
    # create combined lists
    full_matrix = as.matrix(gtools::smartbind(spp_matrix, res_matrix, fill = 0))
    rownames(full_matrix)<- colnames(full_matrix)
    return(full_matrix)
  }
  
  
## ++++ End Helper functions ++++ ##
  
  spp_list = spp_list %>% rlist::list.subset(names(stream_order_list))
  
  resource_list = gut_lists[["resource_list"]] %>% rlist::list.subset(names(stream_order_list))
  
  #create blank stream matrices
  # debugonce(create_matrix)
  stream_matrices = map2(spp_list, resource_list, ~create_matrix(.x,.y)) %>% rlist::list.subset(names(stream_order_list))
  return(stream_matrices = stream_matrices)
  
}
