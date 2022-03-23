"""
1 deg of latitude is ~=69 miles. So break if more than > 1 deg lat.
"""

import pandas as pd
import re
import pgeocode
import mpu
from os import listdir
from zipfile import ZipFile

import matplotlib.pyplot as plt

nom = pgeocode.Nominatim('us')


# ==========================================================================
#           * DIRECTORIES *
# ==========================================================================

ROOTDIR = '/home/econ87/Research/Papers/Right_to_repair/'
DATADIR = ROOTDIR + 'Note/Data/'

def get_lat_long(nom, x):
    """
    Return: (latitude, longitude) of zipcode x.
    Must predefine nom = pgeocode.Nominatim('us')
    """
    x_str = str(x) if len(str(x)) == 5 else f'0{str(x)}'
    query = nom.query_postal_code(x_str)
    return query.latitude, query.longitude


def shortest_distance(df1: pd.core.frame.DataFrame,
        df2: pd.core.frame.DataFrame,
        var: str):
    """
    For each zipcode in df1 find the closest zipcode in df2.
    """
    for j in df1.index:
        brd_lat = df1.loc[j, 'Lat']
        brd_point = df1.loc[j, 'Lat_Long']
        distances = []
        for i in df2.index:
            ma_lat = df2.loc[i, 'Lat']
            ma_point = df2.loc[i, 'Lat_Long']
            # if abs(float(ma_lat) - float(brd_lat)) < 3:
                # distances.append(mpu.haversine_distance(ma_point, brd_point))
            # else:
                # break
            distances.append(mpu.haversine_distance(ma_point, brd_point))
        if len(distances) == 0:
            print(j)
        else:
            df1.loc[j, var] = min(distances)


def case_when_array(array, *args):
    for x in array:
        for i in sorted(args):
            if x <= i:
                print(f'{x} is less than {i}')
                break

def case_when(x, *args):
    cutoff = pd.np.nan
    for i in sorted(args):
        if x <= i:
            cutoff = i
            break
    return cutoff


# ==========================================================================
#           * LOAD DATA *
# ==========================================================================


# ====================
# Zip codes and States
# ====================

zip_states_df = pd.read_csv(f'{DATADIR}/Zip_States/ZipCodesOrg2.csv')

zip_states_df['State_Code'] = zip_states_df.County.str.split(' ').str[-1]

zip_states_df.rename(columns = {'Zip': 'zip'}, inplace = True)


# ============
# Census data
# ============

file = 'zbp00detail.zip'

df = pd.read_csv(
        ZipFile(f'{DATADIR}/Census/{file}').open(re.sub('zip', 'txt', file))
        )

# KEEP ONLY 'GENERAL AUTOMOTIVE REPAIR'
df = df.loc[df.naics == '811111']

df.reset_index(drop = True, inplace = True)

# Create year variable from file name
year = re.search(r'(\d\d)', file).group(1)

if int(year) >= 90:
    year_int = f'19{year}'
else:
    year_int = f'20{year}'
df['Year'] = year_int


df=pd.merge(df, zip_states_df,
        on='zip',
        how='inner',
        indicator=True)

df = df.loc[df._merge == 'both']
df.reset_index(drop = True, inplace = True)


ma_zip = df.loc[df.State_Code == 'MA', ['zip', 'City', 'County']]
ma_zip['Lat_Long'] = ma_zip['zip'].apply(lambda x: get_lat_long(nom, x))
ma_zip['Lat'] = ma_zip.Lat_Long.astype(str).str.extract(r'(\d\d)\.\d{1,4},')


brd_st_zip = {
        'RI': df.loc[df.State_Code == 'RI', ['zip', 'City', 'County']],
        'NH': df.loc[df.State_Code == 'NH', ['zip', 'City', 'County']],
        'NY': df.loc[df.State_Code == 'NY', ['zip', 'City', 'County']],
        'CT': df.loc[df.State_Code == 'CT', ['zip', 'City', 'County']],
        'VT': df.loc[df.State_Code == 'VT', ['zip', 'City', 'County']]
        }


for i in brd_st_zip.keys():
    brd_st_zip[i]['shortest_distance'] = pd.np.nan
    brd_st_zip[i]['Lat_Long'] = brd_st_zip[i]['zip'].apply(
            lambda x: get_lat_long(nom, x)
            )
    brd_st_zip[i]['Lat'] = brd_st_zip[i].Lat_Long.astype(str).str.extract(
            r'(\d\d)\.\d{1,4},')







# ============================================================================
# For each year
#
# =============================================================

for file in listdir(f'{DATADIR}/Census/'):
    # Load file
    df = pd.read_csv(
            ZipFile(f'{DATADIR}/Census/{file}').open(re.sub('zip', 'txt', file))
            )

    # KEEP ONLY 'GENERAL AUTOMOTIVE REPAIR'
    df = df.loc[df.naics == '811111']
    df.reset_index(drop = True, inplace = True)

    # Year of data from file name
    year = re.search(r'(\d\d)', file).group(1)
    if int(year) >= 90:
        year_int = f'19{year}'
    else:
        year_int = f'20{year}'

    # Merge with zip code data to get information
    df=pd.merge(df, zip_states_df,
                on='zip',
                how='inner',
                indicator=True)

    # MA Zip Codes and latitude/longitude
    ma_zip = df.loc[df.State_Code == 'MA', ['zip', 'City', 'County']]
    ma_zip['Lat_Long'] = ma_zip['zip'].apply(lambda x: get_lat_long(nom, x))
    ma_zip['Lat'] = ma_zip.Lat_Long.astype(str).str.extract(r'(\d\d)\.\d{1,4},')
    ma_zip.sort_values(by = ['Lat'], inplace = True)
    ma_zip.reset_index(drop = True, inplace = True)

    # Bordering States Zip Codes
    brd_st_zip = {
            'RI': df.loc[df.State_Code == 'RI', ['zip', 'City', 'County']],
            'NH': df.loc[df.State_Code == 'NH', ['zip', 'City', 'County']],
            'NY': df.loc[df.State_Code == 'NY', ['zip', 'City', 'County']],
            'CT': df.loc[df.State_Code == 'CT', ['zip', 'City', 'County']],
            'VT': df.loc[df.State_Code == 'VT', ['zip', 'City', 'County']]
            }

    for i in brd_st_zip.keys():
        brd_st_zip[i]['shortest_distance'] = pd.np.nan
        brd_st_zip[i]['LAT_Long'] = brd_st_zip[i]['zip'].apply(
                lambda x: get_lat_long(nom, x)
                )
        brd_st_zip[i]['Lat'] = brd_st_zip[i].Lat_Long.astype(str).str.extract(
                r'(\d\d)\.\d{1,4}')
        brd_st_zip[i].sort_values(by = ['Lat'], inplace = True)
        brd_st_zip[i].reset_index(drop = True, inplace = True)
        # Check difference in latitude degrees. If more than 2 then break
        shortest_distance(brd_st_zip[i], ma_zip)




