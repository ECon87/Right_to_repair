"""
In this file I apportion State Federal Highway Administration data to the
constituent counties based on census weights derived in c1c
"""

import pandas as pd
from os import listdir
import utils

# ===============================
#   Directories
# ===============================

DATADIR = '/home/econ87/Research/Papers/Right_to_repair/Note/Data/'

# State Abbreviations dictionary
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

# ===============================
# Load the dataframes
# ===============================

fha_df = pd.read_csv(f'{DATADIR}/fha_2000_2020.csv')
censuswgts_df = pd.read_csv(f'{DATADIR}/Census_weights.csv')

zip_df = pd.read_csv(f'{DATADIR}/Distance_MA_BorderingStates_Census_Merged.csv')

# Drop useless column
fha_df.drop(columns=['Unnamed: 0'], inplace=True)
censuswgts_df.drop(columns=['Unnamed: 0'], inplace=True)
zip_df.drop(columns=['Unnamed: 0'], inplace=True)

# Create State_Code variable to merge with FHA data
censuswgts_df['State_Code'] = censuswgts_df.State.replace(state_abb)

# Rename Dist. of Col. -> District of Columbia
fha_df['State'].replace({'Dist. of Col.': 'District of Columbia'},
                        inplace=True)

# Create census year in order to merge
fha_df['Census_Year'] = fha_df.Year.apply(lambda x: utils.get_censusyear(x))

# =====================
# MERGE
# =====================

data = pd.merge(censuswgts_df,
                fha_df,
                on=['State', 'State_Code', 'Census_Year'],
                how='outer',
                validate='many_to_many',
                indicator=True)

# =====================
# Drop PR and _merge var
# =====================

data = data.loc[data['State_Code'] != 'PR'].reset_index(drop=True)

data.drop(columns=['_merge'], inplace=True)

# ====================
# Zip code data
# ====================

zip_df.drop(columns=['naics', 'County', 'State_Code', 'Lat'], inplace=True)

zip_df.rename(columns={
    'est': 'n_total',
    'year': 'Year',
    'County_nom': 'County',
    'State_nom': 'State_Code'
},
              inplace=True)

# ============================
# Merge
# ============================

data.County.replace(
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

zip_df.County.replace(
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


df = pd.merge(zip_df, data,
        on = ['Year', 'County', 'State_Code'],
        validate = 'many_to_one',
        how = 'outer',
        indicator=True)

df = df.loc[pd.isna(df['State_Code']) == False].reset_index()


# Drop Alaska
df = df.loc[df['State_Code'] != 'AK']

# Drop before year < 2000 (since fha is after 2000)
df = df.loc[df['Year'] >= 2000]




# =====================
# SAVE
# =====================

df.to_csv(f'{DATADIR}/fha_censuswgts_2000_2020.csv')


