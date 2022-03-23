"""
Get the distance between zipcodes in MA with zipcodes in neighboring/bordering
states (RI, NH, NY, CT, VT).

1 deg of latitude is ~=69 miles. So break if more than > 1 deg lat.
"""

import pandas as pd
import re
import pgeocode
import mpu
from os import listdir
from zipfile import ZipFile

import matplotlib.pyplot as plt

import utils



nom = pgeocode.Nominatim('us')

# ==========================================================================
#           * DIRECTORIES *
# ==========================================================================

ROOTDIR = '/home/econ87/Research/Papers/Right_to_repair/'
DATADIR = ROOTDIR + 'Note/Data/'


# ==========================================================================
#
#           * LOAD DATA *
#
# ==========================================================================


# ====================
# Zip codes and States
# ====================

zip_states_df = pd.read_csv(f'{DATADIR}/Zip_States/ZipCodesOrg2.csv')

zip_states_df['State_Code'] = zip_states_df.County.str.split(' ').str[-1]

zip_states_df.rename(columns = {'Zip': 'zip'}, inplace = True)



# ============================================================================
#
# FIND THE DISTANCE BETWEEN ZIPCODES IN MA AND ZIPCODES IN BORDERING STATES
#
# ============================================================================


# Zipcodes in MA
ma_zip = zip_states_df.loc[zip_states_df.State_Code == 'MA']

ma_zip['Lat_Long'] = ma_zip['zip'].apply(lambda x: utils.get_lat_long(nom, x))


ma_zip['Lat'] = ma_zip.Lat_Long.astype(str).str.extract(r'(\d\d)\.\d{1,4},')


# Zipcodes in bordering states
border_states_zip = zip_states_df.loc[
        zip_states_df.State_Code.str.contains(r'(RI|NH|NY|CT|VT)')
        ]

# Latitude and Longitude for zipcodes in bordering states
border_states_zip['Lat_Long'] = border_states_zip['zip'].apply(
        lambda x: utils.get_lat_long(nom, x))

border_states_zip['Lat'] = border_states_zip.Lat_Long.astype(str).str.extract(
        r'(\d\d)\.\d{1,4},')


# Drop if missing (lat, long)
ma_zip = ma_zip.loc[pd.isna(ma_zip.Lat) == False]
border_states_zip = border_states_zip.loc[pd.isna(border_states_zip.Lat) == False]


# Sort dataframes by LAT
ma_zip.sort_values(by = ['Lat'], inplace = True)
border_states_zip.sort_values(by = ['Lat'], inplace = True)

ma_zip.reset_index(drop = True, inplace = True)
border_states_zip.reset_index(drop = True, inplace = True)


# =========================
#
# Distance from MA
#
# =========================

border_states_zip['Short_Dist_MA'] = pd.np.nan

shortest_distance(border_states_zip, ma_zip, var = 'Short_Dist_MA')


# ============================
#
#   * SAVE *
#
# ============================
border_states_zip.to_csv(f'{DATADIR}/Distance_MA_BorderingStates.csv')
