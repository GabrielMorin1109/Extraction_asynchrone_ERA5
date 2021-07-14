# Importation et arrangement des donnees d'IBTrACs

# importation DataBase
{
  # path pour lire la database
  destfile <- paste0(database, "IBTrack/ibtracs.ALL.list.v04r00(2020).csv")
  # si data n'est pas enregistrer, alors on l'enregistre dans le HDD
  if(!file.exists(destfile)){
    url <- "https://ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/csv/ibtracs.ALL.list.v04r00.csv"
    download.file(url, destfile)
  }
  
  # importation de IBTrack dans l'environnement R
  header <- destfile %>% scan(nlines = 1, what = character(), sep = ",")
  df_csv <- destfile %>% read.csv(skip = 2, header = F)
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
# fonction qui permet de creer un buffer circulaire meme si 
# le cercle est au dela des extremite (-180;180) [CRS: 4326]
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