#!/bin/sh

######   ERA5   ######
# RUN >>>>>>>>>>>>>>>>>>>>>>>>>>>>
# find script in computer
ERA5_scrip_API_path=$(find . -name ERA5_scrip_API.py) # le script download par variables, il faut merge les fichiers.nc par "merge_variables.sh"
merge_variables_path=$(find . -name merge_variables.sh) # pour merges les variabels d'ERA5

python3 $ERA5_scrip_API_path
bash $merge_variables_path
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

######   MSWEP   ######
# RUN >>>>>>>>>>>>>>>>>>>>>>>>>>>>
mswep_download=$(find . -name Download_folder_from_a_server.py)

python3 $mswep_download
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

######   R SCRIPT   ######
# RUN >>>>>>>>>>>>>>>>>>>>>>>>>>>>
ERA5_asynchrone_path=$(find . -name 02_ERA5_extraction_asynchrone.R)
MSWEP_synchrone_path=$(find . -name 03_MSWEP_extraction_synchrone.R)
merge_extraction_to_IBTrACS_path=$(find . -name 04_merge_extraction_to_IBTrACS.R)

Rscript $ERA5_asynchrone_path
Rscript $MSWEP_synchrone_path
Rscript $merge_extraction_to_IBTrACS_path
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<