###############################################################################~
# import package
home.script <- dirname(rstudioapi::getSourceEditorContext()$path)
source(paste0(home.script, "/00_source.R"))
if(FALSE){ # a rouler si on doit modifier le present document (note perso: il existe probablement un moyens plus efficace que de faire un if FALSE (a cause des boites de dialogues))
  start_package(parallel_B = TRUE) 
}
if(!exists("dt_sf")){
  source(paste0(home.script, "/01_IBTrACs.R"))
}

if(FALSE){ # mettre condition pour rouler l'extraction
  source(paste0(home.script, "/02_ERA5_extraction_asynchrone.R"))
  source(paste0(home.script, "/03_ERA5_extraction_synchrone.R"))
}
###############################################################################~
# Les bases de donnees sont stockees dans deux hard drives different:
# - premier HDD: 
database
# - Les extractions de mswep sont enregistres sur un hard drive different.
path_to_hdd2 <- "/media/gabriel/HDD_2/"

###############################################################################~
# ERA5 MEAN asynchrone par polygone: ----
path_to_data_era5 <-
  paste0(path_to_hdd2,
         "ERA5/merge_all_time/era5_mean_asynchrone.csv")
test.era5.B <- path_to_data_era5 %>%
  file.exists()

if (isFALSE(test.era5.B)) { # MOYENNE DES DONNEES ASYNCHRONES
  era5.DT <- paste0(path_to_hdd2,
                    "ERA5/merge_all_time/merge_all_time.csv") %>%
    data.table::fread()
  setkey(era5.DT, poly.id)
  
  # merge asynchrone by its corresponding synchrone data
  era5.DT.s <-
    era5.DT[, mean(mean), by = c("var_ERA", "poly.id")][] %>%
    data.table::setnames("V1", "mean_asynchrone") %>%
    data.table::dcast(poly.id ~ var_ERA, value.var = "mean_asynchrone")
  rm(era5.DT)
  gc()
  
  # write era5 modification to disk to optiomise RAM usage
  path_to_data_era5 %>%
    data.table::fwrite(era5.DT.s, .)
} else if (isTRUE(test.era5.B)) {
  era5.DT.s <- path_to_data_era5 %>%
    data.table::fread()
}

## merge ERA5 asynchrone avec IBTrACS ----
setkey(dt_sf, poly.id)
dt_sf[era5.DT.s, on = "poly.id", `:=`(
  msl_mean.a_7.13 = msl,
  slhf_mean.a_7.13 = slhf,
  sst_mean.a_7.13 = sst,
  tcwv_mean.a_7.13 = tcwv
)]

###############################################################################~
## merge ERA5 synchrone avec IBTrACS ----
era5.DT.synchrone <- paste0(path_to_hdd2,
                    "ERA5/merge_all_time_synchrone/merge_all_time_synchrone.csv") %>% 
  data.table::fread()
setkey(era5.DT.synchrone, poly.id)
era5.DT.synchrone <- era5.DT.synchrone %>% data.table::dcast(poly.id ~ var_ERA, value.var = "mean_asynchrone")
# merge synchrone data to IBTrACS
dt_sf[era5.DT.synchrone, on = "poly.id", `:=`(
  msl = msl,
  slhf = slhf,
  sst = sst,
  tcwv = tcwv
)]
###############################################################################~
# MSWEP -----
mswep <- data.table::fread(file = paste0(path_to_hdd2, "MSWEP/merge_all_time/merge_all_time.csv"))

## merge MSWEP avec IBTrACS ----
dt_sf[mswep, on = "poly.id", `:=`(precip_mean=mean)]


