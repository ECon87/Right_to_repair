import pandas as pd
import re
from os import listdir
from zipfile import ZipFile
import utils
from pgeocode import Nominatim
import matplotlib.pyplot as plt

# ==========================================================================
#           * DIRECTORIES *
# ==========================================================================

ROOTDIR = '/home/econ87/Research/Papers/Right_to_repair/'
DATADIR = ROOTDIR + 'Note/Data/'

# ==========================================================================
#           * LOAD DATA *
# ==========================================================================

zip_dist_df = pd.read_csv(f'{DATADIR}/Distance_MA_BorderingStates.csv')

# =================================================
#
# Create Zone Band
#
# =================================================

### Case when
zip_dist_df['Zone_Band'] = zip_dist_df.Short_Dist_MA.apply(
    lambda x: utils.case_when(x, 5, 15, 25, 35, 50, 75, 100, 200))

# ============================================================================
#
# LOAD CENSUS DATA

# For now keep only:
# CT
# MA
# NH
# NY
# RI
# VT
listdir(f'{DATADIR}/Census')
# =============================================================

# states = r'(CT|MA|NH|NY|RI|VT)'

census_datasets = []

for file in listdir(f'{DATADIR}/Census/Business/'):
    # Load file
    print(file)
    file_open = re.sub('zip', 'txt', file)
    zipfile = ZipFile(f'{DATADIR}/Census/Business/{file}')
    try:
        df = pd.read_csv(zipfile.open(file_open), encoding="ISO-8859-1")
    except KeyError:
        file_open = re.sub('zbp', 'Zbp', file_open)
        df = pd.read_csv(zipfile.open(file_open), encoding="utf-8")

    df.rename(columns={
        'ZIP': 'zip',
        'NAICS': 'naics',
        'EST': 'est',
        'N1_4': 'n1_4',
        'N5_9': 'n5_9',
        'N10_19': 'n10_19',
        'N20_49': 'n20_49',
        'N50_99': 'n50_99',
        'N100_249': 'n100_249',
        'N250_499': 'n250_499',
        'N500_999': 'n500_999',
        'N1000': 'n1000'
    },
              inplace=True)

    df.rename(columns={'n<5': 'n1_4'}, inplace=True)
    # KEEP ONLY 'GENERAL AUTOMOTIVE REPAIR'
    try:
        df = df.loc[df.naics == '811111']
    except AttributeError:
        df = df.loc[df.sic == '7538']

    df.reset_index(drop=True, inplace=True)

    # Year of data from file name
    year = re.search(r'(\d\d)', file).group(1)
    if int(year) >= 90:
        year_int = f'19{year}'
    else:
        year_int = f'20{year}'

    df['year'] = year_int

    print(file, year_int)
    census_datasets.append(df)

# Concat all the yearly files
census_df = pd.concat([f for f in census_datasets])
census_df.reset_index(drop=True, inplace=True)

census_df.year.value_counts()

# ============================================================================
#
# Merge
#
# ============================================================================

# Merge
data = pd.merge(zip_dist_df,
                census_df,
                on='zip',
                validate='one_to_many',
                how='outer',
                indicator=True)

tab1 = pd.DataFrame(data._merge.value_counts()).reset_index()
tab2 = pd.DataFrame(data._merge.value_counts(normalize=True)).reset_index()
tab1.rename(columns={'_merge': 'N'}, inplace=True)
tab2.rename(columns={'_merge': 'Perc'}, inplace=True)
tab = pd.merge(tab1, tab2)

for i in tab.values:
    print(i[0], i[1], i[2])

formatted_text = '{:>16}' * (1 + 1)
print('Distribtion of merge indicator:\n')
print('Value', formatted_text.format('N', 'Perc'))
print('=' * 50)
# print(data._merge.value_counts(), data._merge.value_counts(normalize=True))
for i in tab.values:
    print(i[0], i[1], i[2])

# Keep only matched observations
data = data.loc[data._merge != 'left_only']
data.reset_index(drop=True, inplace=True)

# Drop vars
data.drop(columns=[
    'sic', 'name', 'city', 'stabbr', 'cty_name', '_merge', 'Unnamed: 0'
],
          inplace=True)

# ==============================================
#
# Find the coordinates again for the zip codes in the census data!!!
#
# Define geocoder
nom = Nominatim('us')
# ==============================================

all_zips_redux = data[['zip']].drop_duplicates().reset_index(drop=True)

all_zips_redux[[
    'Lat', 'Long', 'County', 'State'
]] = all_zips_redux.zip.apply(lambda x: pd.Series(utils.get_lat_long(nom, x)))

all_zips_redux.rename(columns={
    'Lat': 'Lat_nom',
    'Long': 'Long_nom',
    'County': 'County_nom',
    'State': 'State_nom'
    },  inplace=True)



# Merge back
data = pd.merge(data, all_zips_redux, on='zip')

# ============================================================================
#       * SAVE *
# ============================================================================

data.to_csv(f'{DATADIR}/Distance_MA_BorderingStates_Census_Merged.csv')
