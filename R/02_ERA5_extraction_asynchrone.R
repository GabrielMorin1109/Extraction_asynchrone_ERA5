"
Extraction asynchrone, historique et combinaison des differents codes.
"
###############################################################################~
# import package
home.script <- dirname(rstudioapi::getSourceEditorContext()$path)
source(paste0(home.script, "/00_source.R"))
start_package(parallel_B = FALSE)
source(paste0(home.script, "/01_IBTrACs.R"))

###############################################################################~
# variables a modifier en fonction de la structure des fichier ------

# path pour allez lire les files .nc (premier)
path.to.data.nc <- paste0(database, "ERA5/Data_merge/")

# ==============================================================================
# Le script va creer un doccuments appelez "extractions". 
# Ce fichier contient l'information contenue dans chaque extraction.
# Les extractions sont écrit de maniere iterative sur les fichiers ".nc". 
# Chaque itération va écrire un fichier ".csv" dans le doccument "extraction".

# A la fin de toutes les itérations (<=> extractions), l'ensenmbles des fichiers ".csv" 
# contenue dans "extraction" vont etre concatener dans un fichier unique nommer "merge_all_time.csv".
# Ce fichier est enregistrer dans le doccument "merge_all_time".

# Les extractions sont enregistrer sur un hard drive different.
path_to_hdd2 <- "/media/gabriel/HDD_2/"

# creation du directory ou les fichiers vont etre enregistrer
if(exists("path_to_hdd2")){
  dir.create(paste0(path_to_hdd2, "ERA5"))
} else {
  path_to_hdd2 <- svDialogs::dlg_dir(default = getwd(),
                                     title = "Vous devez specifier le repertoire ou les extractions vont etre enregistrer")$res
}

       
# ==============================================================================

###############################################################################~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(exists("dt_sf")){
  # on choisit les dates qui sont dans l'intersection des dates d'ERA5 et 
  # d'IBTrack moins l'intervalles de temps
  dt.async <- dt_sf[ISO_TIME>=(as.POSIXct("1979-01-01 00:00:00",tz = "UTC") + t.asynchrone),
                    seq(ISO_TIME - t.asynchrone, ISO_TIME, by="3 hours"), by = c("SID","ISO_TIME")]
  setnames(dt.async, c("SID", "ISO_TIME","ISO_TIME_async"))
  # on identifit les groupes
  dt.async[, ISO_TIME_a_u:=.GRP, by=ISO_TIME_async]
  dt.async[,poly_id:=.GRP, by=c("SID","ISO_TIME")]
  
  time_async <- dt.async
  rm("dt.async");gc()
  
  # pour identifier les id! -----
  time_async[,id:=.I]
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXTRACTION ASYNCHRONE ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

nc.files.full <- list.files(path.to.data.nc, full.names = T)
nc.files <-
  path.to.data.nc %>% list.files() %>% tools::file_path_sans_ext()

# lectures des dates des files .nc
data.nc.time <- lapply(nc.files.full, function(nc.files.full.i){
  nc.files.full.i %>% 
    ncdf4::nc_open() %>% 
    {ncdf4::ncvar_get(., "time")*(60*60)} %>% 
    as.POSIXct(origin='1900-01-01 00:00:00', tz= 'UTC') %>% 
    {.[. %in% time_async$ISO_TIME_async]}
}) %>% do.call(c, .) # from : https://stackoverflow.com/a/31472036/13205929

data.nc.time.dt <- data.table::data.table(ISO_TIME = data.nc.time, year = lubridate::year(data.nc.time))
data.nc.time.dt[,time.id:=seq_len(.N), by = year]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXTRACTION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# if directory exist, restart where the algorithm left
path_to_hdd2.extraction <- paste0(path_to_hdd2, "ERA5/extraction/")
dir.create(path_to_hdd2.extraction) # erreur si le directory existe

if(dir.exists(path_to_hdd2.extraction)){
  completed_iteration <- list.files(path_to_hdd2.extraction) %>% tools::file_path_sans_ext() %>% as.integer()
  iteration.extract <<- seq_along(data.nc.time)[!seq_along(data.nc.time) %in% completed_iteration]
} else{
  iteration.extract <<- seq_along(data.nc.time)
}
dt_sf <- dt_sf %>% st_as_sf()
# CODE FINAL*******
{
  for(time_index in iteration.extract){
    # on selectionne la date corresponddante de data.nc.time
    time_select <- data.nc.time[time_index] == time_async$ISO_TIME_async
    poly.id.select <- time_async[time_select, poly_id]
    
    # stocker temporairement le Data Table d'IBTrAC
    dt_sf.tmp <- dt_sf[dt_sf$poly.id %in% poly.id.select,]
    dt_sf.tmp$asynchrone_id <- time_async[time_select, id]
    
    
    # test si le temps de la loop est dans IBTrACS
    if(sum(time_select) == 0){
      cat("ERROR time is not in dt_sf\n")
    } else {
      
      # grep()
      date.in.ibtrack.like.file_ncName <- paste0(
        lubridate::year(data.nc.time[time_index]),
        lubridate::month(data.nc.time[time_index])
      )
      
      
      select.path.to.file.in.database.dir <- nc.files.full[str_detect(date.in.ibtrack.like.file_ncName, nc.files)]
      
      
      # lecture des fichiers .nc 
      suppressMessages(
        tmp.nc <- stars::read_ncdf(
          select.path.to.file.in.database.dir, 
          # var = "msl",
          ncsub = cbind(
            start = c(1, 1, data.table::first(data.nc.time.dt[time_index,time.id])),
            count = c(
              1440,
              721,
              1 # on fait la lecture par 3 heures (faire un saut de 1)
            )
          )
        )
      )
      
      ################################################################################
      # @> extactextract -------------------------------------------------------------
      # chaque ligne represente un polygone! Ainsi, le poly.id est lier a la selection dans le dataframe
      extract.map.by.poly <- lapply(attributes(tmp.nc)$names, function(v.names){
        tmp.f.r <- as(tmp.nc[v.names], "Raster")
        out.s <- exactextractr::exact_extract(x = tmp.f.r, 
                                              y = dt_sf.tmp,
                                              fun = "mean",
                                              progress = FALSE,
                                              # include_cols = c("poly.id", "asynchrone_id")
                                              append_cols = c("poly.id", "asynchrone_id")
        )
        out.s[,var_ERA:=v.names]
        # out.dt <- rbindlist(out.s)[,var_ERA:=v.names][]
        return(out.s)
      }) %>% rbindlist()
      
      
      # write dans le HDD #2, question d'espace
      fwrite(extract.map.by.poly, 
             paste0(
               path_to_hdd2.extraction,
               time_index,
               ".csv"
             )
      )
      # print ou est rendu la boucle >>>>>>
      cat(
        as.character(data.nc.time[time_index]),
        fill = 2,
        labels = paste("(", round(time_index / length(data.nc.time) * 100, 2), "%", "):")
      )
      # <<<<<<
      
      # clear memory
      gc()
    }
  }
}

##############################

# merge of all csv into one unique file
extraction_MergeAll_csv <- list.files(path=path_to_hdd2.extraction, full.names = TRUE) %>%
  lapply(read.csv) %>% 
  dplyr::bind_rows()

# create merge_all_time file ----
paste0(path_to_hdd2, "ERA5/merge_all_time/") %>% 
  dir.create()
# write concatenante files
extraction_MergeAll_csv %>%
  data.table::fwrite(file = paste0(path_to_hdd2, "ERA5/merge_all_time/merge_all_time.csv"))

