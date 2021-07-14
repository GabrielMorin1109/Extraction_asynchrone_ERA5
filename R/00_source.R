###############################################################################~
start_package <- function(){
  list.of.packages <- c("future","measurements","lubridate","parallel", "tidyverse", "data.table", "raster", "sf",
                        "exactextractr", "stars", "ncdf4", 
                        "svDialogs" # pour ecrire des messages dans une fenetre "pop-up" pour l'utilisateur
                        )
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages) > 0) {install.packages(new.packages, dependencies = T, quiet =T, repos='https://cran.rstudio.com/')}
  for(package_name in list.of.packages) {library(package_name,character.only=TRUE, quietly = TRUE)}
  
  future::plan(multicore) 
  numCores <<- parallel::detectCores()-18
  
  home <<- "/home/gabriel/Documents/"
  setwd(home)
  database <<- "/media/gabriel/HDD/Database/"
  # clean plots
  while (dev.cur()>1) dev.off()
}
start_package()


