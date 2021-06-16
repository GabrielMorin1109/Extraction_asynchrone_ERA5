#!/bin/sh

# find script in computer
ERA5_scrip_API_path=$(find . -name ERA5_scrip_API.sh)
merge_variables_path=$(find . -name merge_variables.sh)
ERA5_asynchrone_path=$(find . -name merge_variables.sh)
#run script
python3 $ERA5_scrip_API_path
bash $merge_variables_path
Rscript $ERA5_asynchrone_path
