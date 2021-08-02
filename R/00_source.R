###############################################################################~
start_package <- function(numCores = 1){
  list.of.packages <- c("future","measurements","lubridate","parallel", "tidyverse", "data.table", "raster", "sf",
                        "exactextractr", "stars", "ncdf4", "units"
                        )
  packages.necessaire <- c("svDialogs", # pour ecrire des messages dans une fenetre "pop-up" pour l'utilisateur
                           "unix", # pour set la limite de memoire vive utilisable par R (Pas necessaire sous une machine non Unix)
                           "parallel") # pour set en parallel
  
  list.of.packages <- c(list.of.packages, packages.necessaire)
  
  
  # @> fonction qui permet d'installer les packages manquant de la list.of.packages ----
  install.missing.package <- function(list.of.packages){
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages) > 0) {
      
      message(paste0(
        "\n>> Packages : \n- ",
        paste(new.packages, collapse = " \n- ")
        ,
        "\nwill be install. \nDo you want to proceed? (1/0)"
      ))
      ANSWER <- as.integer(readline())
      
      # test si ANSWER est 0 ou 1, sinon on repose la meme question
      if(!grepl("^[0-1]+$", ANSWER)) {
        return(install.missing.package(list.of.packages))
      } else if (ANSWER == 1) {
        message("Installing packages...")
        install.packages(
          new.packages,
          dependencies = T,
          quiet = T,
          repos = 'https://cran.rstudio.com/'
        )
      } else if (ANSWER == 0) {
        stop("Le code ne peut s'effectuer sans ces packages.")
      } 
    }
  }
  # @< -----
  
  install.missing.package(list.of.packages)
  for (package_name in list.of.packages) {
    library(package_name,
            character.only = TRUE,
            quietly = TRUE)
  }
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # mes paths personnels, a modifier selon votre machine. Dans un futur proche, home devrait etre retirer.
  home <<- "/home/gabriel/Documents/"
  setwd(home)
  # path pour lire les bases de donnees
  database <<- "/media/gabriel/HDD/Database/"
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  cat(">>>> Function message : \n")
  # ask user if they want to set parallel computing >>>>>>>
  numcore_out_of_range.B <- !(numCores > 0 & numCores <= parallel::detectCores())
    if (numcore_out_of_range.B) {
      # test recursif pour poser la question du nombre de coeurs choisit pour parallel
      Question.user <- function(){
        user.input <- as.integer(
          svDialogs::dlg_input(paste0("Enter the number of cores you want to use. \nThe number must be between 1 and ", parallel::detectCores()),
                               default = 1)$res
        )
        # si on a pas un nombre de coeur possible a selectionner, on repose la question
        if (!length(user.input)) { # The user clicked the 'cancel' button
          user.input <- 1
        } else if (is.na(user.input)) {
          user.input <- Question.user()  
        } else if (!(user.input > 0 & user.input <= parallel::detectCores())){
          user.input <- Question.user()
        }
        return(user.input)
      }
      numCores <- suppressWarnings(Question.user())
    }
    # inform users how many cores will be used
    message(paste0("Parrallel computing will use ", numCores, " core(s).\n"))
    
    ### parallel and RAM limit for Unix user ###
    B.windows <<- Sys.info()["sysname"] != "Linux" # permet de tester si le systeme peut parrallelise en "fork"
    if(!B.windows) { 
      # Set a memory limit that R can use.
      if (Sys.info()["sysname"] == "Linux") {
        message(c("Maximum RAM usage is set to 60Gb. Limit can be changed by modifying the code."))
        unix::rlimit_as(60e12, 60e12)
      }
      # set parallel structure
      future::plan(multicore)
    } else {
      message(paste0("Since you have Windows, parallel plan will not work and only one core will be used"))
    }
  
  # clean plots
  while (dev.cur()>1) dev.off()
  # clean garbage memory
  gc()
}



