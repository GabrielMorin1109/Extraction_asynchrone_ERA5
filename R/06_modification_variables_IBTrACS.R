###############################################################################~
# import package
home.script <- dirname(rstudioapi::getSourceEditorContext()$path)
source(paste0(home.script, "/00_source.R"))
if(FALSE){ # a rouler si on doit modifier le present document (note perso: il existe probablement un moyens plus efficace que de faire un if FALSE (a cause des boites de dialogues))
  start_package(parallel_B = TRUE) 
} 
source(paste0(home.script, "/05_ENSO.R"))
###############################################################################~
# Ce code transforme des variables observees dans IBTrACS pour qu'elle soit plus 
# interessante dans le modele.

###############################################################################~
# prp.m2 ----
# on calcul la proportion du cercle qui est au dessu de la terre
{
  border.w.u.4088 <- paste0(database, "WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp") %>% 
    sf::read_sf() %>%
    sf::st_geometry() %>% 
    sf::st_transform(4088) %>% 
    sf::st_simplify(dTolerance = 5000) %>% 
    sf::st_combine() %>% 
    sf::st_buffer(0) %>% 
    sf::st_make_valid()
  
  sf.b.ok <- dt_sf$geometry %>% sf::st_transform(4088) %>% sf::st_buffer(0) %>% sf::st_make_valid() # pour regler le probleme de "Evaluation error: TopologyException: Input geom 0 is invalid"
  
  # iteration en fonction de la parrallelisation
  iteration <- seq_along(sf.b.ok) %>% 
    {split(., . %% numCores)} # split sur le nombre de coeur pour optimiser la computation parallel
  
  # A VERIFIER!!!!
  sf_IntB <- iteration %>%
    mclapply(mc.cores = numCores, function(i) {
      sf::st_intersection(border.w.u.4088, sf.b.ok[i])
    }) %>% lapply(sf::st_sf) %>% dplyr::bind_rows()
  
  # on rearange les donnees ensembles
  buf_area <- sf_IntB[unlist(iteration),] %>% st_area()
  
  buffer_full <- sf::st_area(sf.b.ok)
  if(!any(sapply(buf_area, is_empty))){
    prop_m2 <- {buf_area/buffer_full} %>% units::drop_units()
  } else {
    prop_m2 <- map2(buf_area, buffer_full, ~
                      if(!is_empty(.x)){
                        .x/.y
                      } else {
                        set_units(0,"m2")
                      }) %>% map(units::drop_units) %>% unlist()
  }
  
  dt_sf <- cbind(dt_sf, data.table::data.table(prp.m2= prop_m2))
}
###############################################################################~
# t.sst.27p et pp.cum.sst.27p ----
{
  # temps passer au dessus de l'eau (count lorsque sst>=27 degree celsius en moyenne (<=> a X + 273.15 kelvin afin de convertir les kelvin en celsius))
  # on fait t.sst.27p-1, car on veut avoir 1 quand TRUE, et -1 quand FALSE
  dt_sf[!is.na(sst),t.sst.27p:=(sst>=(27 + 273.15))*1, by=SID 
  ][t.sst.27p==0,t.sst.27p:=t.sst.27p-1]
  
  # On fait la proportion cumule avec la prp.m2 [comme prp.m2 mesure la proportion au dessus de la terre, on fait 1-prp.m2]
  dt_sf[!is.na(sst), pp.cum.sst.27p:=cumsum(t.sst.27p*(1-prp.m2)), by = SID]
  
  dt_sf[!is.na(sst),t.sst.27p:=cumsum(t.sst.27p), by = SID]
}
# t.sst.26p et pp.cum.sst.26p ----
{ 
  # temps passer au dessus de l'eau (count lorsque sst>=26 degree celsius en moyenne (<=> a X + 273.15 kelvin afin de convertir les kelvin en celsius))
  dt_sf[!is.na(sst),t.sst.26p:=(sst>=(26 + 273.15))*1, by=SID 
  ][t.sst.26p==0,t.sst.26p:=t.sst.26p-1]
  
  # On fait la proportion cumule avec la prp.m2 [comme prp.m2 mesure la proportion au dessus de la terre, on fait 1-prp.m2]
  dt_sf[!is.na(sst),pp.cum.sst.26p:=cumsum(t.sst.26p*(1-prp.m2)), by = SID]
  
  dt_sf[!is.na(sst),t.sst.26p:=cumsum(t.sst.26p), by = SID]
}
###############################################################################~
# delta_USA_PRES et delta_USA_WIND ----
{
  # on va chercher les donnees asynchrones pour certaines mesures
  nsec_36h <- (36*60*60)
  dt_sf[,TIME.m36.t:=.I
  ][dt_sf[,.I[(ISO_TIME-nsec_36h) %in% ISO_TIME],by=SID]$V1,
    TIME.m36.t:=dt_sf[,.I[ISO_TIME %in% (ISO_TIME-nsec_36h)],by=SID]$V1]
  
  # variation de PRES dans 36 heures
  dt_sf[, delta_USA_PRES := USA_PRES - dt_sf[TIME.m36.t, USA_PRES]
  ][is.na(delta_USA_PRES), delta_USA_PRES := 0]
  
  # variation de WIND dans 36 heures
  dt_sf[, delta_USA_WIND := USA_WIND - dt_sf[TIME.m36.t, USA_WIND]
  ][is.na(delta_USA_WIND), delta_USA_WIND := 0]
}
###############################################################################~
# modification de certaines variables d'IBTrACS ----
{
  # WIND^2 ----
  dt_sf[,WIND.2:=USA_WIND^2, by=SID.id]
  # WIND^3 ----
  dt_sf[,WIND.3:=USA_WIND^3, by=SID.id]
  # ACE to date ----
  dt_sf[!is.na(dt_sf$USA_WIND),
        ACE.to.date:=USA_WIND^2 %>% cumsum(),
        by=SID.id]
  # PDI to date ----
  dt_sf[!is.na(dt_sf$USA_WIND),
        PDI.to.date:=USA_WIND^3 %>% cumsum(),
        by=SID.id]
  # max wind speed to date ----
  dt_sf[!is.na(dt_sf$USA_WIND),
        m.w.speed.to.date:=USA_WIND %>% cummax(),
        by=SID.id]
}
###############################################################################~



