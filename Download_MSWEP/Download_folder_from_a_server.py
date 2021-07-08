# -*- coding: utf-8 -*-
"""
Telechargement d'un folder stocker dans un serveur externe
"""
# import modules
import glob, os, sys, re, bs4, requests

# lieu ou le telechargement va se faire
database = "/media/gabriel/HDD/Database/MSWEP/"
path = database + "Data"

# DEF >>>
def download_files(path_os, url, extention):
    """
    Fonction qui permet le download de files stocker sur un serveur externe.
    """
    # on change le working directory pour l'enregistrement des fichers ~~~~~~~~~
    os.chdir(path_os)
    
    # demande de download
    r = requests.get(url)
    # liste des documents a telecharger
    data = bs4.BeautifulSoup(r.text, "html.parser")
    data_file_extention = data.findAll('a', href=re.compile(extention))
    
    # print the % downloaded file
    def check_file_status(filepath, filesize):
        sys.stdout.write('\r') 
        sys.stdout.flush()
        size = int(os.stat(filepath).st_size)
        percent_complete = (size/filesize)*100
        sys.stdout.write('%.3f %s' % (percent_complete, '% Completed'))
        sys.stdout.flush()
    
    
    # download funtion
    for file in data_file_extention:
        filename = url + file["href"]
        file_base = os.path.basename(file["href"])
        print('Downloading',file_base)
        req = requests.get(filename)
        filesize = int(req.headers['Content-length'])
        with open(file_base, 'wb') as outfile:
            chunk_size=1048576 # on download par chunk, permet de reprendre un telechargement s'il y a probleme
            for chunk in req.iter_content(chunk_size=chunk_size):
                outfile.write(chunk)
                if chunk_size < filesize:
                    check_file_status(file_base, filesize)
        check_file_status(file_base, filesize)
        print()
# END DEF <<<<

download_files( 
    path_os = path,
    # nom du dossier dans le serveur a telecharger ~~~~~~~~~
    url = "http://hydrology.princeton.edu/data/hylkeb/MSWEP_V220/global_3hourly_010deg/",
    extention = "\.nc$", # modifier seulement le nc. E.g. pour un csv il faut ecrire : "\.csv$"
)

