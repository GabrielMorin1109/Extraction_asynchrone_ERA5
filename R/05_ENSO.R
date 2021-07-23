###############################################################################~
# import package
home.script <- dirname(rstudioapi::getSourceEditorContext()$path)
source(paste0(home.script, "/00_source.R"))
if(FALSE){ # a rouler si on doit modifier le present document (note perso: il existe probablement un moyens plus efficace que de faire un if FALSE (a cause des boites de dialogues))
  start_package(parallel_B = TRUE) 
} 
source(paste0(home.script, "/04_merge_extraction_to_IBTrACS.R"))
###############################################################################~
# Les donnees d'enso doivent etre telecharger et netoyer manuellement!
###############################################################################~
# ENSO -----
{
  # donnees sur ftp://ftp.coaps.fsu.edu/pub/JMA_SST_Index/ ou sur https://psl.noaa.gov/enso/mei/
  enso <- data.table::fread(paste0(database, "ENSO/jmasst1868-today.filter-5"))
  names(enso) <-
    c("YEAR",
      "JAN",
      "FEB",
      "MAR",
      "APR",
      "MAY",
      "JUN",
      "JUL",
      "AUG",
      "SEP",
      "OCT",
      "NOV",
      "DEC")
  YEAR <- 1970:2019
  enso <- enso[which(enso$YEAR %in% c(1970:2019)), ]
  if(enso[YEAR == 2019, DEC] == 999){
    enso[YEAR == 2019, DEC := enso[YEAR == 2019, NOV]]
  }
  north <- c("MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV") #IL Y A DES RANGES QUI OVERLAPS
  south <- c("OCT", "NOV", "DEC", "JAN", "FEB", "MAR", "APR", "MAY")
  enso34.m.n <- enso[, apply(.SD, 1, mean, na.rm=T)/10, .SDcols = north]
  # Basin north
  enso34.m.n <- data.table(
    YEAR = YEAR,
    enso = enso34.m.n
  )
  BASIN.north <- data.table(BASIN = c("WP", "NI", "EP", "N_A"))
  BASIN.north[,tmp:=1]
  enso34.m.n[,tmp:=1]
  enso34.m.n <- left_join(enso34.m.n, BASIN.north)
  
  # Basin south
  enso34.m.s <- 
    enso[, apply(.SD, 1, mean), .SDcols = south]
  
  enso34.m.s <- data.table(
    YEAR = YEAR,
    enso = enso34.m.s
  )
  
  BASIN.south <- data.table(BASIN = c("SP", "SI"))
  BASIN.south[,tmp:=2]
  enso34.m.s[,tmp:=2]
  enso34.m.s <- left_join(enso34.m.s, BASIN.south)
  
  # merge des enso
  enso34 <- bind_rows(enso34.m.n, enso34.m.s)
  
  # merge avec dt
  dt_sf[,YEAR:=year(ISO_TIME)]
  enso.names <- colnames(enso34)
  
  dt_sf[enso34, on=c("YEAR", "BASIN"), (enso.names):=  mget(paste0("i.", enso.names))]
}