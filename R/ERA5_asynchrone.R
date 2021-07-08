"
Extraction asynchrone, historique et combinaison des differents codes.
"
###############################################################################~
start_package <- function(){
  list.of.packages <- c("future","measurements","lubridate","parallel", "tidyverse", "data.table", "raster", "sf",
                        "exactextractr", "stars", "ncdf4")
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
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# importation DataBase
{
  # importation de IBTrack
  header <- paste0(database, "IBTrack/ibtracs.ALL.list.v04r00(2020).csv") %>% scan(nlines = 1, what = character(), sep = ",")
  df_csv <- paste0(database, "IBTrack/ibtracs.ALL.list.v04r00(2020).csv") %>% read.csv(skip = 2, header = F)
  names(df_csv) <- header
  
  df_csv$BASIN <- as.character(df_csv$BASIN)
  df_csv$BASIN[which(is.na(df_csv$BASIN))] <- as.character("N_A")
  df_csv$USA_ROCI <- measurements::conv_unit(df_csv$USA_ROCI, from = "mi", to = "km")
  df_dt <- as.data.table(df_csv)
  rm(df_csv)
  # correction des longitude (IBTrack va de 180 a 266.9)
  df_dt[LON>180,LON:=LON-360]
  
  # on prend 500 de rayon 
  dt_sf <- df_dt[,geometry:= st_as_sf(df_dt[,.(LON,LAT)], coords = c("LON","LAT"), crs= 4326) %>% st_set_crs(4326)]
  rm(df_dt)
  # Correction des dates au modulo 3 heures le plus pres, il y a des date du genre : "2003-01-05 09:50:00 UTC"
  dt_sf[,ISO_TIME := ymd_hms(as.character(ISO_TIME)) %>% 
          round_date("3 hour")
  ]
  
  # On convertie les unites des rayons d oeils d ouragans. On etait en miles et on les rend en kilometres
  dt_sf[,USA_ROCI := conv_unit(USA_ROCI, from = "mi", to = "km")]
  
  dt_sf[,time.d:= date(ISO_TIME)]
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # on compte le nombre de seconde a aller chercher pour l'extraction asynchrone
  {
    nsec_3h <- (3*60*60)
    nsec_24h <-nsec_3h*24/3
    nsec_1m <- nsec_24h*30
    nsec_3m <- 3*nsec_1m
  }
  
  # on choisit le temps a aller chercher asynchrone
  t.asynchrone <- nsec_3m
  
  # SELECTION ASYNCHRONE
  dt_sf[,SID:=as.character(SID)]
  dt_sf[ISO_TIME>=(as.POSIXct("1979-01-01 00:00:00",tz = "UTC") + t.asynchrone),][,.N, by=SID][,cut:=N][,N:=NULL][]
  dt_sf.cut <- dt_sf[ISO_TIME>=(as.POSIXct("1979-01-01 00:00:00",tz = "UTC") + t.asynchrone),][,.N, by=SID][,cut:=N][,N:=NULL][]
  dt_sf.raw <- dt_sf[SID %in% dt_sf.cut$SID][,.N,by=SID][,raw:=N][,N:=NULL][]
  SID.select <<- sapply(as.character(dt_sf.raw$SID), function(SID.i){
    dt_sf.raw[SID==SID.i,raw] == dt_sf.cut[SID == SID.i,cut]
  })
  dt_sf <- dt_sf[SID %in% names(SID.select)[SID.select], ]
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  dt_sf[,time.id:= .GRP, by = .(SID, time.d)
  ][,SID.id:= .GRP, by = SID
  ][,poly.id:= .GRP, by = .(SID, ISO_TIME)
  ][,ISO_TIME.id:= .GRP, by = time.d]
  dt_sf <- dt_sf[!duplicated(dt_sf$poly.id),]
}
###### create_circle START ######
my.buffer <- function(center_df, radius.m) {
  # set du cadrage
  my.bbox <- st_as_sfc(
    st_bbox(c(xmin=-180.0000,ymin= -90.0000,xmax= 180.0000,ymax= 90.0000))
  ) %>% st_set_crs(4326) %>% st_transform(4088) %>% st_set_crs(NA)
  
  poly.left <- st_as_sfc(
    st_bbox(c(xmin=-30000000,ymin= -10007550,xmax= 0,ymax= 10007550))
  )
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  {
    # create_buffer
    cent_buffer <- st_as_sf(center_df, coords = c("LON", "LAT"), crs = 4326) %>% st_geometry() %>% st_sf %>%
      st_transform(4088) %>% st_set_crs(NA) # transform to an equal area projection so that the circle stays the same regardless of the coordinates
    
    cent_buffer <- st_buffer(cent_buffer, radius.m) %>% st_geometry() %>% st_sf
    
    # # merge la colonne boolean
    cent_buffer$in.bbox.B <- st_covered_by(cent_buffer, my.bbox, sparse = F) %>% c()
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  {
    circle.in <- st_intersection(cent_buffer[cent_buffer$in.bbox.B==FALSE,]$geometry, my.bbox) %>% st_sf()
    
    circle.out <- st_difference(cent_buffer[cent_buffer$in.bbox.B==FALSE,]$geometry,
                                my.bbox) %>% st_sf() %>% st_geometry()
    
    circle <- cbind(circle.out, circle.in)
    names(circle) <- c("geometry.out", "geometry")
  }
  if(nrow(circle)!=0){
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # testing if out polygone is in the left or right corner ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    circle$out.left.B <- st_covered_by(circle$geometry.out, poly.left, sparse = F) %>% c()
    
    #  on bouge les polygones qui sont en dehors du cadre
    circle$geometry.out <- mclapply(1:nrow(circle), mc.cores = numCores, function(i){
      x <- circle[i,]
      as.matrix(
        cbind(
          x$geometry.out[[1]][[1]][,1] + (if(x$out.left.B==T){1} else{-1}) * 20015109*2, # 20015109 est l'equivalent de 180 degree
          x$geometry.out[[1]][[1]][,2]
        )
      ) %>% list() %>% st_polygon() %>% st_sfc() %>% st_sf
    }) %>% data.table::rbindlist() %>% sf::st_as_sf() %>% st_intersection(my.bbox) %>% st_geometry() # on selectionne les points qui sont dans le bbox pour ne pas cause d'erreur
    
    circle <- apply(circle, 1, function(x){
      st_multipolygon(x["geometry.out"], x["geometry"]) %>% st_sfc() %>% st_sf()
    }) %>% data.table::rbindlist() %>% sf::st_as_sf()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cent_buffer[cent_buffer$in.bbox.B==FALSE,][,"geometry"] <- circle
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  }
  center_df$geometry <- cent_buffer$geometry %>% st_set_crs(4088) %>% st_transform(4326)
  
  return(center_df)
}
###### create_circle END ######
dt_sf <- my.buffer(dt_sf,radius.m = 500*1000) 
dt_sf
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXTRACTION ASYNCHRONE ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(FALSE){
  # path pour allez lire les files .nc
  path.choosen <- "ERA5/Data_merge/"
  
  nc.files.full <- list.files(paste0(database, path.choosen), full.names = T)
  nc.files <- list.files(paste0(database, path.choosen)) %>% tools::file_path_sans_ext()
  
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
  path_to_hdd2 <- "/media/gabriel/HDD_2/ERA5/extraction/"
  if(dir.exists(path_to_hdd2)){
    completed_iteration <- list.files(path_to_hdd2) %>% tools::file_path_sans_ext() %>% as.integer()
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
        if(time_index == 1){
          cat("A changer en fonction de l'allure du noms des fichiers \n")
        }
        
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
                 path_to_hdd2,
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
  
  # merge of all csv into one unique file
  extraction_MergeAll_csv <- list.files(path=path_to_hdd2, full.names = TRUE) %>%
    lapply(read.csv) %>% 
    bind_rows
  
  
  dir.create("/media/gabriel/HDD_2/ERA5/merge_all_time/")
  extraction_MergeAll_csv %>% data.table::fwrite("/media/gabriel/HDD_2/ERA5/merge_all_time/merge_all_time.csv")
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXTRACTION SYNCHRONE ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# path pour allez lire les files .nc
path.choosen <- "/MSWEP/Data/"

nc.files.full <- list.files(paste0(database, path.choosen), full.names = T)
nc.files <- list.files(paste0(database, path.choosen)) %>% tools::file_path_sans_ext()

# lectures des dates des files .nc
data.nc.time <- lapply(nc.files.full, function(nc.files.full.i){
  nc.files.full.i %>% 
    ncdf4::nc_open() %>% 
    {ncdf4::ncvar_get(.,"time")*(24 * 60 * 60)} %>% # car les units sont: days since 1899-12-31 00:00:00
    as.POSIXct(origin='1899-12-31 00:00:00', tz= 'UTC') #%>% 
}) %>% do.call(c, .) # from : https://stackoverflow.com/a/31472036/13205929

data.nc.time.dt <- data.table::data.table(ISO_TIME = data.nc.time, year.month = paste(
  lubridate::year(data.nc.time),
  lubridate::day(data.nc.time),
  sep = "_"
  ))
data.nc.time.dt[,time.id:=seq_len(.N), by = year.month]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXTRACTION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# if directory exist, restart where the algorithm left
path_to_hdd2 <- "/media/gabriel/HDD_2/MSWEP/"
dir.create(path_to_hdd2) # OK, car erreur si existe
if(!is_empty(list.files(path_to_hdd2))){
  completed_iteration <- list.files(path_to_hdd2) %>% tools::file_path_sans_ext() %>% as.integer() %>% sort()
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
    # poly.id.select <- time_async[time_select, poly_id]
    
    # stocker temporairement le Data Table d'IBTrAC
    dt_sf.tmp <- dt_sf[dt_sf$ISO_TIME %in% data.nc.time[time_index],]# [dt_sf$poly.id %in% poly.id.select,] # plus besoin, car synchrone
    # dt_sf.tmp$asynchrone_id <- time_async[time_select, id]
    
    
    # test si le temps de la loop est dans IBTrACS
    {
      # grep()
      if(lubridate::month(data.nc.time[time_index])>=10){
        date.in.ibtrack.like.file_ncName <- paste0(
          lubridate::year(data.nc.time[time_index]),
          lubridate::month(data.nc.time[time_index])
        )
      } else { # necessaire, pour fit la structure des noms de fichier
        date.in.ibtrack.like.file_ncName <- paste0(
          lubridate::year(data.nc.time[time_index]),
          "0",
          lubridate::month(data.nc.time[time_index])
        )
      }
      
      
      select.path.to.file.in.database.dir <- nc.files.full[str_detect(date.in.ibtrack.like.file_ncName, nc.files)]
      
      
      # lecture des fichiers .nc 
      suppressMessages(
        tmp.nc <- stars::read_ncdf(
          select.path.to.file.in.database.dir, 
          # var = "msl",
          ncsub = cbind(
            start = c(1, 1, data.table::first(data.nc.time.dt[time_index,time.id])),
            count = c(
              3600,
              1800,
              1 # on fait la lecture par 3 heures (faire un saut de 1)
            )
          )
        )
      )
      
      ################################################################################
      # @> extactextract -------------------------------------------------------------
      # chaque ligne represente un polygone! Ainsi, le poly.id est lier a la selection dans le dataframe
      extract.map.by.poly <- as(tmp.nc, "Raster") %>% 
          exactextractr::exact_extract(y = dt_sf.tmp,
                                       fun = "mean",
                                       progress = FALSE,
                                       append_cols = c("poly.id")
        )
      
      
      # write dans le HDD #2, question d'espace
      fwrite(extract.map.by.poly, 
             paste0(
               path_to_hdd2,
               time_index,
               ".csv"
             )
      )
      # print ou est rendu la boucle >>>>>>
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
# fwrite(time_async,"/home/gabriel/Desktop/extraction_back_up/time_async.csv")
# ERA5 MEAN
start_package()
# time_async <- fread("/home/gabriel/Desktop/extraction_back_up/time_async.csv")
# setkey(time_async, poly_id)
era5.DT <- fread("/home/gabriel/Desktop/extraction_back_up/merge_all_time.csv")
setkey(era5.DT, poly.id)

# merge by synchrone data
era5.DT.s <- era5.DT[,mean(mean), by=c("var_ERA", "poly.id")][]
setnames(era5.DT.s, "V1", "mean_asynchrone")
era5.DT.s <- dcast(era5.DT.s, poly.id~var_ERA, value.var = "mean_asynchrone")
if(!file.exists("/home/gabriel/Desktop/extraction_back_up/era5_mean_asynchrone.csv")){
  fwrite(era5.DT.s, "/home/gabriel/Desktop/extraction_back_up/era5_mean_asynchrone.csv")
}

era5.DT.s <- fread("/home/gabriel/Desktop/extraction_back_up/era5_mean_asynchrone.csv")


setkey(dt_sf, poly.id)
dt_sf[era5.DT.s, on = "poly.id", `:=`(msl=msl, slhf=slhf, sst=sst, tcwv=tcwv)]

