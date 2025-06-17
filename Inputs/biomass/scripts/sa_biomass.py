# -*- coding: utf-8 -*-
"""
Created on Tue Mar 28 22:53:06 2023

@author: medin
"""
# List of regions
# https://www.globalfiredata.org/data.html

# dataset
# https://www.geo.vu.nl/~gwerf/GFED/GFED4/
# Modified from GFED script: get_GFED4s_CO_emissions.py
# reminder, python starts indexing at 0
# REMOVE agricultural waste burning from EDGAR

import numpy as np
import h5py # if this creates an error please make sure you have the h5py library
import os
import pandas as pd
# =============================================================================
# import geopandas as gpd
# import matplotlib.pyplot as plt
# =============================================================================

months       = '01','02','03','04','05','06','07','08','09','10','11','12'
sources      = 'SAVA','BORF','TEMF','DEFO','PEAT','AGRI'

# in this example we will calculate annual CO emissions for the 14 GFED 
# basisregions over 1997-2014. Please adjust the code to calculate emissions
# for your own specie, region, and time period of interest. Please
# first download the GFED4.1s files and the GFED4_Emission_Factors.txt
# to your computer and adjust the directory where you placed them below
year = 2015


proj_dir = "G:/My Drive/Project/SA_region/"
input_dir = "biomass/inputs"
export_dir = "G:/My Drive/Project/SA_region/biomass/files/biomass_"+str(year)+"/emis_raw/"

directory    = os.path.join(proj_dir, input_dir)


"""
Read in emission factors
"""
species = [] # names of the different gas and aerosol species
EFs     = np.zeros((41, 6)) # 41 species, 6 sources

# NMHC - only include species that have only H and C atoms, e.g pentane..
species_interest = np.array(['PM2.5', 'NOx', 'NH3', 'OC', 'BC', 'SO2',
                             'NMHC', 'CH3OH', 'C2H5OH', 'CH2O', 'C2H4O', 'C3H6O',
                             'HCOOH', 'CH3COOH', 'MEK', 'CH3COCHO', 'HOCH2CHO'])



k = 0
f = open(directory+'/GFED4_Emission_Factors.txt')
while 1:
    line = f.readline()
    if line == "":
        break
        
    if line[0] != '#':
        contents = line.split()
        species.append(contents[0])
        EFs[k,:] = contents[1:]
        k += 1
                
f.close()


species = np.array(species)



"""
make table with summed DM emissions for each region, year, and source
"""
if year >= 2017:
    string = directory+'/GFED4.1s_'+str(year)+'_beta.hdf5'
else:
    string = directory+'/GFED4.1s_'+str(year)+'.hdf5'

f = h5py.File(string, 'r')

    
basis_regions = f['/ancill/basis_regions'][:] #' regions 1-15 (15-globe) are labeled 0-14 (python index starts at 0)
grid_area     = f['/ancill/grid_cell_area'][:]
lon = f['/lon'][:]
lat = f['/lat/'][:]
new_lon = lon.flatten()
new_lat = lat.flatten()
all_regions = basis_regions.flatten()
grid_area_f = grid_area.flatten()

region = 8 #Southern Africa, SHAF region 


for n in species_interest:
   poll_ind = np.where(n == species)   
   poll_ind = int(poll_ind[0])
   
   EF_poll = EFs[poll_ind,:]

   poll_emissions = np.zeros((720, 1440))
   
   for month in range(12):
        # read in DM emissions
        string = '/emissions/'+months[month]+'/DM'
        DM_emissions = f[string][:]
        for source in range(6):
            # read in the fractional contribution of each source
            string = '/emissions/'+months[month]+'/partitioning/DM_'+sources[source]
            contribution = f[string][:]
            # calculate poll emissions as the product of DM emissions (kg DM per 
            # m2 per month), the fraction the specific source contributes to 
            # this (unitless), and the emission factor (g poll per kg DM burned)
            poll_emissions += DM_emissions * contribution * EF_poll[source]
    
    
    # fill table with total values for the globe (row 15) or basisregion (1-14)
   mask = basis_regions == (region + 1) #' 1 is added to get region number, python - 0 based indexing
            #' logical array, filters out each region
    
   emis_table = grid_area * mask * poll_emissions / 1E6 # 1E6 (g to mton)
   
   new_emis = emis_table.flatten()

   grid_array = pd.DataFrame(data = [new_lon, new_lat, new_emis, all_regions, grid_area_f]).T
   grid_array.columns = ['lon', 'lat', 'emissions', 'regions', 'grid_area']


   sa_emis = grid_array[(grid_array['regions'] == region + 1)] 

   del sa_emis['regions']
   
   sa_emis.to_csv(export_dir + n + '.csv', index=False)

   #print(n + ': ' +str(np.sum(emis_table)/1E10))     

   




# =============================================================================
# sa_emis.plot(x="lon", y="lat", kind="scatter", c="emissions",
#         colormap="YlOrRd")
# 
# 
# countries = gpd.read_file(
#                gpd.datasets.get_path("naturalearth_lowres"))
# countries.head()
# 
# fig, ax = plt.subplots(figsize=(8,6))
# 
# countries[countries["continent"] == "Africa"].plot(color="lightgrey", 
#                                                    ax = ax,
#                                                    edgecolor="black")
# 
# 
# sa_emis.plot(x="lon", y="lat", kind="scatter", c="emissions",
#         colormap="YlOrRd", ax=ax)
# =============================================================================


