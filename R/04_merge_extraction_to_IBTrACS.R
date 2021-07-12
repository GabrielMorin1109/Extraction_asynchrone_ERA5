###############################################################################~
# import package
home.script <- dirname(rstudioapi::getSourceEditorContext()$path)
source(paste0(home.script, "/00_source.R"))
source(paste0(home.script, "/01_IBTrACs.R"))
###############################################################################~
# A TERMINER

# fwrite(time_async,"/home/gabriel/Desktop/extraction_back_up/time_async.csv")
# ERA5 MEAN

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
