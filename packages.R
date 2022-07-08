## library() calls go here
###load packages and functions
  if(!require("pacman")) install.packages("pacman")
  library(pacman)
  package.list <- c("conflicted", "dotenv", "drake","data.table","gtools",
                    "RCurl","plyr","tidyverse","furrr", "fnmate", "moments",
                    "dflow","rmarkdown","tictoc","chron","lubridate","httr","TTR", 
                    "grid","gridExtra", "ggridges", "fluxweb", "rmarkdown", "MuMIn","zoib",
                    "viridis", "broom","bbmle","ggthemes", "ggeffects", "ggpubr","betareg",
                    "fluxweb","igraph","ggraph","magick","cowplot","rriskDistributions",
                    "rstan", "brms", "tidybayes", "parallel", "hillR", "RInSp", "rsample",
                    "emmeans")
  p_load(char = package.list, install = TRUE, character.only = TRUE)
  # p_load_gh("jimjunker1/junkR")
  # remotes::install_github("jimjunker1/junkR")
  library(junkR)
  conflict_prefer('count', 'dplyr')
  conflict_prefer('mutate', 'dplyr')
  conflict_prefer('traceplot', 'coda')
  remotes::install_github("milesmcbain/dflow")
  # remotes::install_github("MilesMcBain/breakerofchains")
  library(dflow)
  rm("package.list" )
  
  source("https://gist.githubusercontent.com/jimjunker1/0ec857c43b1e3a781363c1b1ea7e12ad/raw/4dd2d1078a00500963822d29b2e58ebf39831fb3/geom_flat_violin.R")
  # len <<- function(data)
  # {
  #   result<<-ifelse(is.null(nrow(data)),length(data),nrow(data))
  #   return(result)
  # }
  # vert<<-function(object)
  # {
  #   #result<-as.data.frame(cbind(as.matrix(object)))
  #   if(is.list(object))
  #     object<-cbind(object)
  #   object<-data.frame(object)
  # 
  #   return(object)
  # }
  # cbind.fill<<-function(...,fill=NULL)
  # {
  #   inputs<-list(...)
  #   inputs<-lapply(inputs,vert)
  #   maxlength<-max(unlist(lapply(inputs,len)))
  #   bufferedInputs<-lapply(inputs,buffer,length.out=maxlength,fill,preserveClass=FALSE)
  #   return(Reduce(cbind.data.frame,bufferedInputs))
  # }
  # cbind.fill<<-function(...){
  #   nm <- list(...)
  #   nm<-lapply(nm, as.matrix)
  #   n <- max(sapply(nm, nrow))
  #   do.call(cbind, lapply(nm, function (x)
  #     rbind(x, matrix(, n-nrow(x), ncol(x)))))
  # }

  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  # load("./object_files/ocecolors.rda")
  overkt_to_C <- function(a){1/(a*(8.61733*10^-5)) - 273.15}
  theme_mod = function(){theme_bw() %+replace% theme(panel.grid = element_blank())}
  theme_black = function() {theme_bw() %+replace% theme(panel.background = element_rect(fill = 'transparent', colour = NA),panel.grid = element_blank(), axis.ticks = element_line(color = 'white'),
                                                          axis.title = element_text(color = 'white'), axis.text = element_text(color = 'white'), plot.background =element_rect(fill = 'transparent', colour = NA),
                                                          panel.border = element_rect(fill = NA, colour = 'white'))}
  
  multiply_prod <- function(x, NPE,...) x/NPE
  daily_prod <- function(x) x/as.numeric(.$days)
  
  '%ni%' <- Negate('%in%')
  
  # # ! ++++ Plotting aesthetics ++++ ! #
  # oce_temp_disc = c("#E5FA6A","#CF696C","#544685","#072C44","#082A40","#0B222E")#color codes
  oce_temp_pos <- c(256,212,168,124,80,1)#color positions in 'temperature' list of ocecolors
  stream_order <- factor(c("hver", "st6","st9", "st7","oh2","st14"))#stream ordering
  stream_order_list <- stream_order %>% as.list() %>% setNames(.,stream_order) #stream ordering for lists
  stream_temp_labels <- c("27.2","17.6","11.2","5.8","5.5","5.0")#stream annual temperature labels
  names(stream_temp_labels) = stream_order #setting named character vector of stream labels
  
source("./R/fluxweb_mod_function.R")
source("./R/Lorenz.R")
source("./R/Evenness.R")
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 
nboot = 1e3
theme_set(theme_mod())
options(mc.cores = parallel::detectCores()-1)
rstan_options(auto_write = TRUE)
library(rmarkdown)
