""" 
website : https://cds.climate.copernicus.eu/cdsapp#!/home#!%2Fdataset%2Freanalysis-era5-single-levels%3Ftab=overview
database: ERA5 hourly data on single levels from 1979 to present

script pour Download les files d'ERA5 et les importer dans un espace de stockage local
"""

""" 
Changement du directory
"""
import os
os.getcwd()
database = "/media/gabriel/HDD/Database/ERA5/"
path = database + "Data"
path

if(not os.path.isdir(path)):
	os.makedirs(path)
os.chdir(path)

"""
Importation des données
"""
import cdsapi

c = cdsapi.Client(
	url = "https://cds.climate.copernicus.eu/api/v2",
	key = "45576:735bebae-47ca-42a5-aca6-cf3bb5a41525")

DATE_A_TELECHARGER =  range(1979, 2022, 1) # 2022 car la boucle n'inclue pas 2022

varnames = ['mean_sea_level_pressure', 'sea_surface_temperature', 'surface_latent_heat_flux', 'total_column_water_vapour']

for year in DATE_A_TELECHARGER :
    for varname in varnames :
        c.retrieve(
            'reanalysis-era5-single-levels',
            {
                'product_type': 'reanalysis',
                'variable': str(varname),
                'year': str(year),
                'month': [
                    '01', '02', '03',
                    '04', '05', '06',
                    '07', '08', '09',
                    '10', '11', '12',
                ],
                'day': [
                    '01', '02', '03',
                    '04', '05', '06',
                    '07', '08', '09',
                    '10', '11', '12',
                    '13', '14', '15',
                    '16', '17', '18',
                    '19', '20', '21',
                    '22', '23', '24',
                    '25', '26', '27',
                    '28', '29', '30',
                    '31',
                ],
                'time': [
                    '00:00', 
                    '03:00',
                    '06:00',
                    '09:00',
                    '12:00', 
                    '15:00',
                    '18:00', 
                    '21:00',
                ],
                'format': 'netcdf',
                'area': [
                90, -180, -90,
                180,
                ],
            },
            (str(year) + '_' + str(varname) + '_' + '.nc'))

##
