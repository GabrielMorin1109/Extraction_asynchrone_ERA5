#!/bin/sh
path="/media/gabriel/HDD/Database/ERA5/"
cd $path

# ----------------------------------------------------
# TESTING IF DIRECTORY EXIST
# ----------------------------------------------------
### START MKDIR >
#!/bin/bash

# path du dossier que je veux creer
file="Data_merge/"

# merge des deux string
long_path="$path$file"
echo "test if ${long_path} exist, if not, creating one"

# test si le folder exist, sinon on le cree
if [[ -d "$long_path" ]]; then 
	# -d pour folder
	# -f pour file
	echo "This file exists on your filesystem."
else
	echo "creating directory ${long_path}"
	mkdir $long_path
fi
### END MKDIR <


# ----------------------------------------------------
# MERGE DES DIFFERENTES VARIABLE DANS UNE MEME ANNEE >
# ----------------------------------------------------
echo "merge in a for loop all four variables of the same given year"

for i in $(seq 1979 2020); do 
    echo "now processing year ${i}"
    cdo merge Data/"$i"_*.nc Data_merge/"$i".nc 
done
# END MERGE <

# MERGE DES DIFFERENT LAPS DE TEMPS
cd $file # aka on va dans le Data_merge/

# test if there is enough available space on disk >>
content_space=$(du "${path}Data" -m | awk '{ print $1 }') #awk pour seulement retenur la premiere entree

# tail -n1 pour enlever le header du output
disk_left=$(df ${path} -m | tail -n1 | awk '{ print $4 }') # Structure de l'output: Filesystem 1M-blocks Used Available Use% Mounted on

# -lt veut dire <=
if [[ $disk_left -lt $content_space ]]; then 
	echo "not enouth space"
else
	echo "merging time of all file. Filename output: ERA5.nc"
	cdo mergetime *.nc ERA5.nc
fi
# <<

