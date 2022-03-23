"""
Import specific ranges of the Federal Highway Administration (FHA)
"""

import pandas as pd
import re
from os import listdir

# =========================
# Directories
# ========================

DATADIR = '/home/econ87/Research/Papers/Right_to_repair/Note/Data/'
FHADATADIR = f'{DATADIR}/FHA/'

state_abb = {
    'Alabama': 'AL',
    'Alaska': 'AK',
    'Arizona': 'AZ',
    'Arkansas': 'AR',
    'California': 'CA',
    'Colorado': 'CO',
    'Connecticut': 'CT',
    'Delaware': 'DE',
    'Dist. of Col.': 'DC',
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

# =========================
# Load and compine mv data
# ========================

fha_mv = []
for f in sorted(listdir(FHADATADIR)):
    if re.search(r'mv1_vehicles', f):
        print(f'{f}\n')
        year = int(re.search(r'(\d\d\d\d)', f).group(1))
        if year <= 2010:
            fha_ex = pd.read_excel(
                f'{FHADATADIR}/{f}',
                names=['State', 'allmv_pvt', 'allmv_pub', 'allmv_total'],
                # sheet_name='MV1',
                usecols='A,L:N',
                skiprows=range(1, 13))
        elif 2010 < year <= 2018:
            fha_ex = pd.read_excel(
                f'{FHADATADIR}/{f}',
                names=['State', 'allmv_pvt', 'allmv_pub', 'allmv_total'],
                # sheet_name='MV-1',
                usecols='A,N:P',
                skiprows=range(1, 12))
        elif 2018 < year <= 2019:
            fha_ex = pd.read_excel(
                f'{FHADATADIR}/{f}',
                names=['State', 'allmv_pvt', 'allmv_pub', 'allmv_total'],
                # sheet_name='MV-1',
                usecols='A,N:P',
                skiprows=range(1, 10))
        elif 2019 < year:
            fha_ex = pd.read_excel(
                f'{FHADATADIR}/{f}',
                names=['State', 'allmv_pvt', 'allmv_pub', 'allmv_total'],
                # sheet_name='MV-1',
                usecols='A,N:P',
                skiprows=range(1, 9))
        fha_ex['Year'] = year
        fha_ex = fha_ex.loc[:50]
        for c in fha_ex.columns[1:]:
            fha_ex[c] = fha_ex[c].astype(int)
        fha_mv.append(fha_ex)

# Concat
fha_mv_df = pd.concat([f for f in fha_mv])
fha_mv_df.reset_index(drop=True, inplace=True)

# Clean State variable
fha_mv_df['State'] = fha_mv_df.State.replace({
    r'\s\(\d+\)': '',
    r'\d/': ''
},
                                             regex=True)

fha_mv_df['State'] = fha_mv_df.State.str.strip()

# ----------------------
# State abbreviations
# --------------------

fha_mv_df['State_Code'] = fha_mv_df.State.replace(state_abb)

# =========================
# Load and compine mv data
# ========================

fha_miles = []
for f in sorted(listdir(FHADATADIR)):
    if re.search('miles_functional', f):
        print(f'{f}\n')
        year = int(re.search(r'(\d\d\d\d)', f).group(1))
        if year <= 2008:
            f = 'vm2_miles_functional_2008.xls'
            fha_ex = pd.read_excel(
                f'{FHADATADIR}/{f}',
                names=['State', 'vehicle_miles'],
                # sheet_name='MV1',
                usecols='A,P',
                skiprows=range(1, 14))
        elif 2008 < year:
            fha_ex = pd.read_excel(f'{FHADATADIR}/{f}',
                                   names=['State', 'vehicle_miles'],
                                   sheet_name='A',
                                   usecols='A,R',
                                   skiprows=range(1, 14))
        # elif year == 2013:
        # fha_ex = pd.read_excel(
        # f'{FHADATADIR}/{f}',
        # names=['State', 'vehicle_miles'],
        # sheet_name='A',
        # usecols='A,R',
        # skiprows=range(1, 15))
        fha_ex['Year'] = year
        fha_ex = fha_ex.loc[:50]
        for c in fha_ex.columns[1:]:
            fha_ex[c] = fha_ex[c].astype(int)
        fha_miles.append(fha_ex)

# Concat
fha_miles_df = pd.concat([f for f in fha_miles])
fha_miles_df.reset_index(drop=True, inplace=True)

# Clean State variable
fha_miles_df['State'] = fha_miles_df.State.replace(
    {
        r'\s\(\d+\)': '',
        r'\d/': '',
        r'Dist(rict|\.) of Columbia': 'Dist. of Col.'
    },
    regex=True)

fha_miles_df['State'] = fha_miles_df.State.str.strip()

# ----------------------
# State abbreviations
# --------------------

fha_miles_df['State_Code'] = fha_miles_df.State.replace(state_abb)

fha_miles_df.State.value_counts()
fha_miles_df.State_Code.value_counts()

# ===================
# MERGE
# ===================

fha_df = pd.merge(fha_mv_df,
                  fha_miles_df,
                  on=['State', 'State_Code', 'Year'],
                  how='outer',
                  indicator=True)

print(fha_df._merge.value_counts())

fha_df.drop(columns=['_merge'], inplace=True)

# ===================
# SAVE
# =====================

fha_df.to_csv(f'{DATADIR}/fha_2000_2020.csv')
