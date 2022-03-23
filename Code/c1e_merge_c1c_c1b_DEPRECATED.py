"""
Merge distance of zip codes from MA with census weights of counties.

"""

import pandas as pd
import utils

from os import listdir

# ====================================================
#   DIRECTORIES
# ====================================================

DATADIR = '/home/econ87/Research/Papers/Right_to_repair/Note/Data/'

print(listdir(f'{DATADIR}/FHA/'))

# ====================================================
#   LOAD DATABASES
# ====================================================

zipdist_df = pd.read_csv(
    f'{DATADIR}/Distance_MA_BorderingStates_Census_Merged.csv')

censuswg_df = pd.read_csv(f'{DATADIR}/Census_weights.csv')

fha_drivers_df = pd.read_csv(f'{DATADIR}/FHA/dl201_drivers.csv')
fha_vehicles_df = pd.read_excel(f'{DATADIR}/FHA/mv1_vehicles_reg.xlsx')
fha_miles_df = pd.read_excel(f'{DATADIR}/FHA/vm3m_miles_functional.xls')


# =========

zipdist_df.drop(columns=['Unnamed: 0'], inplace=True)
censuswg_df.drop(columns=['Unnamed: 0'], inplace=True)

# =========

# =========
# Create State abbreviations and drop original state variable
# =========

state_abb = {
    'Alabama': 'AL',
    'Alaska': 'AK',
    'Arizona': 'AZ',
    'Arkansas': 'AR',
    'California': 'CA',
    'Colorado': 'CO',
    'Connecticut': 'CT',
    'Delaware': 'DE',
    'District of Columbia': 'DC',
    'Florida': 'FL',
    'Georgia': 'GA',
    'Hawaii': 'HI',
    'Idaho': 'ID',
    'Illinois': 'IL',
    'Indiana': 'IN',
    'Iowa': 'IA',
    'Kansas': 'KS',
    'Kentucky': 'KY',
    'Louisiana': 'LA',
    'Maine': 'ME',
    'Maryland': 'MD',
    'Massachusetts': 'MA',
    'Michigan': 'MI',
    'Minnesota': 'MN',
    'Mississippi': 'MS',
    'Missouri': 'MO',
    'Montana': 'MT',
    'Nebraska': 'NE',
    'Nevada': 'NV',
    'New Hampshire': 'NH',
    'New Jersey': 'NJ',
    'New Mexico': 'NM',
    'New York': 'NY',
    'North Carolina': 'NC',
    'North Dakota': 'ND',
    'Ohio': 'OH',
    'Oklahoma': 'OK',
    'Oregon': 'OR',
    'Pennsylvania': 'PA',
    'Puerto Rico': 'PR',
    'Rhode Island': 'RI',
    'South Carolina': 'SC',
    'South Dakota': 'SD',
    'Tennessee': 'TN',
    'Texas': 'TX',
    'Utah': 'UT',
    'Vermont': 'VT',
    'Virginia': 'VA',
    'Washington': 'WA',
    'West Virginia': 'WV',
    'Wisconsin': 'WI',
    'Wyoming': 'WY',
}

censuswg_df['State_nom'] = censuswg_df['State']

censuswg_df.replace({'State_nom': state_abb}, inplace=True)

censuswg_df.drop(columns=['State'], inplace=True)

# =========
# Rename County name to match with zip code data in order to merge
# =========

censuswg_df.rename(columns={'County': 'County_nom'}, inplace=True)

# =================
# Cleaning County names
# =================

censuswg_df.County_nom.replace(
    {
        r' Parish$': '',
        r' city$': ' (city)',
        r'LaSalle': 'La Salle',
        r'^DeBaca$': 'De Baca',
        r' Census Area$': ' (CA)',
        r' Borough$': ' Borough',
        r'(Carson City)': r'\1 (city)'
    },
    regex=True,
    inplace=True)

zipdist_df.County_nom.replace(
    {
        r' Parish$': '',
        r'City of Alexandria': 'Alexandria (city)',
        r'City of Harrisonburg': 'Harrisonburg (city)',
        r'City of Richmond': 'Richmond (city)',
        r'City of Suffolk': 'Suffolk (city)',
        r'LaSalle': 'La Salle',
        r'^DeBaca$': 'De Baca',
        r'Doña Ana': 'Dona Ana',
        r'Saint ': 'St. ',
        r' \(city$': ' (city)',
        r'City and County of ': ''
    },
    regex=True,
    inplace=True)

# ====================================================
#   Census Years
#
#  The census weight database contains a year dimension
# with one year for each census (2000 and 2010).
# Therefore, I need to create a census year variable in
# the zip code database in which the year runs from 1994 to 2019!

# Drop years before 2000!
# ====================================================

zipdist_df['Census_Year'] = 2000

zipdist_df.loc[zipdist_df.year >= 2007, 'Census_Year'] = 2010

# ====================================================
#   MERGE
# ====================================================

data = pd.merge(zipdist_df,
                censuswg_df,
                on=['County_nom', 'State_nom', 'Census_Year'],
                how='outer',
                indicator=True)

# Drop AK, PR counties
data = data.loc[data.State_nom != 'AK'].reset_index(drop=True)
data = data.loc[data.State_nom != 'PR'].reset_index(drop=True)

# The only observations that are not matched are in the censuswg_df.
# Unmatched observations in zipdist_df are only missing values.

df = data.loc[data._merge != 'both']
df['County'] = df.State_nom + " - " + df.County_nom

pd.crosstab(df.County, df._merge)

df.loc[df._merge == "left_only", 'County_nom'].drop_duplicates()

del (df)

# ====================================================
#
# Keep only matched observations
#
# ====================================================

data = data.loc[data._merge == 'both'].reset_index(drop=True)

# Clean by dropping some variables

data.drop(columns=['County', 'Lat_Long', 'Lat', 'State_Code', '_merge'],
          inplace=True)



# ====================================================
#
# Get Minimum, and Maximum distance to MA
# for different counties in bordering states!
#
# ====================================================

MAX_DIST_MA = data.Short_Dist_MA.max() + 1

# replace missing short distance to MA with the max
data.loc[pd.isna(data.Short_Dist_MA), 'Short_Dist_MA'] = MAX_DIST_MA

