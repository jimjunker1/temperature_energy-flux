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
    res_matrix = matrix(0, nrow = nrow(resource_list), ncol = nrow(resource_list))
    rownames(res_matrix) <- colnames(res_matrix) <- unlist(resource_list)
    
    # create combined lists
    full_matrix = as.matrix(gtools::smartbind(spp_matrix, res_matrix, fill = 0))
    rownames(full_matrix)<- colnames(full_matrix)
    return(full_matrix)
  }
  
  
## ++++ End Helper functions ++++ ##
  
  spp_list = spp_list %>% rlist::list.subset(names(stream_order_list))
  
  resource_list = gut_lists[["resource_list"]] %>% rlist::list.subset(names(stream_order_list))
  
  predator_lists = list(hver = c("Limnophora.riparia","Sperchon.glandulosus"),
                        st6 = c("Limnophora.riparia","Sperchon.glandulosus","Macropelopia", "Antocha.sp.","Ephydridae.sp."),
                        st9 = c("Limnophora.riparia","Sperchon.glandulosus","Macropelopia", "Antocha.sp.","Ephydridae.sp.", "Clinocera.stagnalis"),
                        st7 = c("Limnophora.riparia","Sperchon.glandulosus"),
                        oh2 = c("Limnophora.riparia","Sperchon.glandulosus", "Macropelopia"),
                        st14 = c("Limnophora.riparia","Sperchon.glandulosus", "Antocha.sp.")) %>% 
    rlist::list.subset(names(stream_order_list))
  
  predator_diet_lists = list(hver = list(c("Simulium.vittatum","Cricotopus.sylvestris","Nais.spp.", "Naididae 1"),c("Nais.spp.","Naididae 1")),
                             st6 = list(c("Nais.spp.","Simulium.vittatum","Simulium.spp.", "Micropsectra.sp."),
                                        c("Nais.spp.","Naididae.1"),
                                        c("Chaetocladius.dentiforceps","Eukiefferiella","Micropsectra.sp.","Orthocladius","Orthocladius.oblidens","Simulium.vittatum","Thienemanniella.sp."),
                                        c("Nais.spp.","Simulium.vittatum","Simulium.spp.", "Micropsectra.sp."),
                                        c("Nais.spp.","Simulium.vittatum","Simulium.spp.", "Micropsectra.sp.")),
                             st9 = list(c("Nais.spp.","Simulium.vittatum","Simulium.spp."),
                                        c("Nais.spp.","Tubificid.1"),
                                        c("Eukiefferiella.sp.","Micropsectra.sp.","Orthocladius.fridgidus","Simulium.vittatum","Thienemanniella.sp."),
                                        c("Nais.spp.","Simulium.vittatum","Simulium.spp."),
                                        c("Nais.spp.","Simulium.vittatum","Simulium.spp."),
                                        c("Nais.spp.","Simulium.vittatum","Simulium.spp.")),
                             st7 = list(c()),
                             oh2 = list(c()),
                             st14 = list(c())) %>% 
    rlist::list.subset(names(stream_order_list))
  predator_diet_props = list(hver = list(c(0.25,0.25,0.25,0.25), c(0.5, 0.5)),
                             st6 = list(c(0.1,0.3,0.3,0.3),
                                        c(0.5,0.5),
                                        c(rep(1/7,7)),
                                        c(0.1,0.3,0.3,0.3),
                                        c(0.1,0.3,0.3,0.3)),
                             st9 = list(c(0.1,0.7,0.2),
                                        c(0.5,0.5),
                                        c(rep(1/5),5),
                                        c(0.1,0.7,0.2),
                                        c(0.1,0.7,0.2),
                                        c(0.1,0.7,0.2)),
                             st7 = list(),
                             oh2 = list(),
                             st14 = list())
  
  # debugonce(create_matrix)
 stream_matrices = map2(spp_list, resource_list, ~create_matrix(.x,.y))
 


}
