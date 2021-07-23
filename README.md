# Extraction asynchrone ERA5 et extraction synchrone de MSWEP
Historique des scripts pour procéder à l'extraction asynchrone des données d'ERA5.

```diff
- Projet en progression. 
```



Les cartes de réanalyse sont issues de la base de données d'<a href = "https://cds.climate.copernicus.eu/cdsapp#!/home#!%2Fdataset%2Freanalysis-era5-single-levels%3Ftab=overview">**ERA5** </a>. La base de données de réanalyse correspondante se nomme **ERA5 hourly data on single levels from 1979 to present**.

Les variables d'ERA5 choisies sont:
- msl: mean sea level pressure;
- sst : Sea Surface temperature;
- slhf : surface latent heat flux;
- tcwv : total column water vapour.

----

Les cartes de précipitations sont issues de la base de données <a href = "http://www.gloh2o.org/mswep/">**Multi-Source Weighted-Ensemble Precipitation**</a> (*MSWEP*). Les données de précipitations sont fournies aux trois heures.

----

Les données <a href = "https://psl.noaa.gov/enso/mei/">**El Niño–Southern Oscillation**</a> (*ENSO*) ont été utilisé comme variable covariate dans le modèle.

----

```diff
- Si vous êtes sous Windows, il est possible qu'il y ait des erreurs d'exécution.
- Le script R utilise le *fork parallel processing* qui fonctionne seulement sur les machines Unix. (Normalement, il n'y a pas d'erreur de ce côté)
- Je crée un modèle de GBM avec un package qui fonctionne seulement avec Unix.
- Ne portez pas attention aux fautes de français dans mes scripts, je vais les corrige vers la fin.
```
Si vous avez des commentaires, veuillez m'écrire à mon adresse <a href="gabriel.morin1109@outlook.com"> gabriel.morin1109@outlook.com</a>.
