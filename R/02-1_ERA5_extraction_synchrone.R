"
Extraction synchrone, historique et combinaison des differents codes.
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

# Les extractions sont enregistrer sur un hard drive different.
path_to_hdd2 <- "/media/gabriel/HDD_2/"

# creation du directory ou les fichiers vont etre enregistrer
if(exists("path_to_hdd2")){
  dir.create(paste0(path_to_hdd2, "ERA5"))
} else {
  path_to_hdd2 <- svDialogs::dlg_dir(default = getwd(),
                                     title = "Vous devez specifier le repertoire ou les extractions vont etre enregistrer")$res
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXTRACTION SYNCHRONE ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nc.files.full <- list.files(path.to.data.nc, full.names = T)
nc.files <-
  path.to.data.nc %>% list.files() %>% tools::file_path_sans_ext()

# lectures des dates des files .nc
data.nc.time <- lapply(nc.files.full, function(nc.files.full.i){
  nc.files.full.i %>% 
    ncdf4::nc_open() %>% 
    {ncdf4::ncvar_get(., "time")*(60*60)} %>% 
    as.POSIXct(origin='1900-01-01 00:00:00', tz= 'UTC')
}) %>% do.call(c, .) # from : https://stackoverflow.com/a/31472036/13205929

data.nc.time.dt <- data.table::data.table(ISO_TIME = data.nc.time, year = lubridate::year(data.nc.time))
data.nc.time.dt[,time.id:=seq_len(.N), by = year]

                                                                                # 
                                                                                # data.nc.time.dt <- data.table::data.table(ISO_TIME = data.nc.time, year.month = paste(
                                                                                #   lubridate::year(data.nc.time),
                                                                                #   lubridate::day(data.nc.time),
                                                                                #   sep = "_"
                                                                                # ))
                                                                                # data.nc.time.dt[,time.id:=seq_len(.N), by = year.month]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXTRACTION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# if directory exist, restart where the algorithm left
path_to_hdd2.extraction <- paste0(path_to_hdd2, "ERA5/extraction_synchrone/")
dir.create(path_to_hdd2.extraction) # erreur si le directory existe


if(!is_empty(list.files(path_to_hdd2.extraction))){
  completed_iteration <- list.files(path_to_hdd2.extraction) %>% tools::file_path_sans_ext() %>% as.integer() %>% sort()
  seq_time <- seq_along(data.nc.time)[data.nc.time %in% dt_sf$ISO_TIME]
  iteration.extract <<- seq_time[!seq_time %in% completed_iteration]
} else {
  iteration.extract <<- seq_along(data.nc.time)[data.nc.time %in% dt_sf$ISO_TIME] # selectionne les fichier dans IBTrACS
}

dt_sf <- dt_sf %>% st_as_sf()


# CODE FINAL*******
{
  for(time_index in iteration.extract){
    # on selectionne la date corresponddante de data.nc.time
    
    time_select <- data.nc.time[time_index]
    
    # stocker temporairement le Data Table d'IBTrAC
    dt_sf.tmp <- dt_sf[dt_sf$ISO_TIME %in% data.nc.time[time_index],]
    
    
    # test si le temps de la loop est dans IBTrACS
    {
      # grep()
      date.in.ibtrack.like.file_ncName <- paste0(lubridate::year(data.nc.time[time_index]))
      
      
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
                                              append_cols = c("poly.id")
        )
        out.s[,var_ERA:=v.names]
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
      # print ou est rendu la boucle >>>>>> (ne fonctionne plus)
      cat(
        as.character(data.nc.time[time_index]),
        fill = 2,
        labels = paste("(", round(time_index / length(
          seq_along(data.nc.time)[data.nc.time %in% dt_sf$ISO_TIME]
        ) * 100, 2), "%", "):")
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
paste0(path_to_hdd2, "ERA5/merge_all_time_synchrone/") %>% 
  dir.create()

# write concatenante files
extraction_MergeAll_csv %>%
  data.table::fwrite(file = paste0(path_to_hdd2, "ERA5/merge_all_time_synchrone/merge_all_time_synchrone.csv"))