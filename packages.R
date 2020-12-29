## library() calls go here
###load packages and functions
  if(!require("pacman")) install.packages("pacman")
  library(pacman)
  package.list <- c("conflicted", "dotenv", "drake","data.table","gtools",
                    "RCurl","plyr","tidyverse","furrr", "fnmate", "rowr",
                    "dflow","rmarkdown","tictoc","chron","lubridate","httr","TTR", 
                    "grid","gridExtra", "ggridges", "fluxweb", "rmarkdown",
                    "viridis", "broom","bbmle","ggthemes", "ggeffects",
                    "fluxweb","igraph","ggraph","magick","cowplot","rriskDistributions")
  p_load(char = package.list, install = TRUE, character.only = TRUE)
  p_load_gh("jimjunker1/junkR")
  p_load_gh("milesmcbain/dflow")
  rm("package.list" )
  # ins_julian_path = getURL("https://raw.githubusercontent.com/jimjunker1/secprod_workflow/master/len_freq/ins_julian_function.txt",ssl.verifypeer = FALSE)
  # ins_julian <<- eval(parse(text = ins_julian_path))
  #  len_freq_path = getURL("https://raw.githubusercontent.com/jimjunker1/secprod_workflow/master/len_freq/len_freq_function.R",ssl.verifypeer = FALSE)
  #  len_freq <<- eval(parse(text = len_freq_path))
  cbbPalette <<- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  # load("./object_files/ocecolors.rda")
  theme_mod = function(){theme_bw() %+replace% theme(panel.grid = element_blank())}
  theme_black = function() {theme_bw() %+replace% theme(panel.background = element_rect(fill = 'transparent', colour = NA),panel.grid = element_blank(), axis.ticks = element_line(color = 'white'),
                                                          axis.title = element_text(color = 'white'), axis.text = element_text(color = 'white'), plot.background =element_rect(fill = 'transparent', colour = NA),
                                                          panel.border = element_rect(fill = NA, colour = 'white'))}
  
  # C_to_overkt <<- function(a){1/(8.61733*10^-5*(a+273.15))}#overkt function
  # C_to_overkt_J <<- function(a){1/(1.380*10^-23*(a+273.15))}#overkt function
  # overkt_to_C <<- function(a){1/(a*(8.61733*10^-5)) - 273.15}
  # overktJ_to_C <<- function(a){1/(a*(1.380*10^-23)) - 273.15}
  # C_to_overkt_stand15 <<- function(a){(1/(8.61733e-5*(15+273.15)) - (1/(8.61733e-5*(a+273.15))))}
  # 
  # Mode <<- function(x) {
  #   ux <- unique(x)
  #   ux[which.max(tabulate(match(x, ux)))]
  # }
  # na.rm_mean <<- function(...,na.rm=TRUE){mean(c(...),na.rm=na.rm)}
  # options(stringsAsFactors = F)
  # options(max.print = 1000000)
  # 
  # mass_corrPB <<- function(a,b){a*(b^-0.25)}
  # mass_corrBIO <<- function(a,b){a*(b^0.25)}
  # 
  # temp_corrPB <<- function(a,b){a*(b^0.65)}
  # temp_corrBIO <<- function(a,b){a*(b^-0.65)}

  multiply_prod <<- function(x, NPE,...) x/NPE
  daily_prod <<- function(x) x/as.numeric(.$days)
  
  '%ni%' <<- Negate('%in%')
  
  # # ! ++++ Plotting aesthetics ++++ ! #
  # oce_temp_disc = c("#E5FA6A","#CF696C","#544685","#072C44","#082A40","#0B222E")#color codes
  oce_temp_pos <<- c(256,212,168,124,80,1)#color positions in 'temperature' list of ocecolors
  stream_order <<- factor(c("hver", "st6","st9", "st7","oh2","st14"))#stream ordering
  stream_order_list <<- stream_order %>% as.list() %>% setNames(.,stream_order) #stream ordering for lists
  stream_temp_labels <<- c("27.2","17.6","11.2","5.8","5.5","5.0")#stream annual temperature labels
  names(stream_temp_labels) = stream_order #setting named character vector of stream labels
  
source("./R/fluxweb_mod_function.R")
source("./R/Lorenz.R")
theme_set(theme_mod())
