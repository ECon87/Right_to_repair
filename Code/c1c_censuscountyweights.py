"""
Find the proportion of each state's population in the different counties
"""

import pandas as pd
import re

from os import listdir

# ==========================================================================
#           * DIRECTORIES *
# ==========================================================================

ROOTDIR = '/home/econ87/Research/Papers/Right_to_repair/'
DATADIR = ROOTDIR + 'Note/Data/'

# ==========================================================================
#           * LOAD DATA *
# ==========================================================================

census_pop = []
census_inc = []

for f in listdir(f'{DATADIR}/Census/Decennial/'):
    if re.search(r'overlays.*\.csv$', f):
        print(f)
        year = re.search(r'(\d\d\d\d)\.', f).group(1)
        df = pd.read_csv(f'{DATADIR}/Census/Decennial/{f}', skiprows=1)
        if re.search(r'(SF4.*DP3|^ACS.*DP03)', f):
            try:
                df = df.loc[df['Population Groups'] == 'Total population']
                df.reset_index(drop=True, inplace=True)
            except KeyError:
                pass
            inc_cols = [
                c for c in df.columns if re.search('capita.*income', c)
            ]
            df = df[['Geographic Area Name'] + [inc_cols[0]]]
            df.rename(columns={
                df.columns[0]: 'County',
                df.columns[1]: 'Inc_percapita'
            },
                inplace=True)
        elif re.search(r'\.P\d{0,3}1', f):
            inc_cols = [c for c in df.columns if re.search(r'(Name|Label)', c)]
            inc_cols += ['Total']
            df = df[inc_cols]
            df.rename(columns={
                df.columns[0]: 'County',
                'Total': 'Population'
            },
                inplace=True)
        df['Census_Year'] = year
        df['State'] = df.County.str.split(', ').str[-1]
        df['County'] = df.County.str.split(", ").str[:-1].str.join(" ")
        df['County'] = df.County.str.replace(r" County$", "")
        try:
            df['Population'] = df.Population.astype(str).str.replace(
                r'\(\w+\)', '')
            df['Population'] = df.Population.astype(int)
            df['State_Population'] = df.groupby(
                'State')['Population'].transform('sum')
            df['Population_Share'] = df.Population / df.State_Population
            census_pop.append(df)
        except AttributeError:
            census_inc.append(df)

# ------------
df_pop = pd.concat([f for f in census_pop])
df_inc = pd.concat([f for f in census_inc])

county_renames = {
    'Do?a Ana': 'Dona Ana',
    'Bayamon Municipio': 'Bayamón Municipio',
    'Canovanas Municipio': 'Canóvanas Municipio',
    'Comerio Municipio': 'Comerío Municipio',
    'Guanica Municipio': 'Guánica Municipio',
    'Juana Diaz Municipio': 'Juana Díaz Municipio',
    'Las Marias Municipio': 'Las Marías Municipio',
    'Loiza Municipio': 'Loíza Municipio',
    'Manati Municipio': 'Manatí Municipio',
    'Mayaguez Municipio': 'Mayagüez Municipio',
    'Rincon Municipio': 'Rincón Municipio',
    'Rio Grande Municipio': 'Río Grande Municipio',
    'San German Municipio': 'San Germán Municipio',
    'San Sebastian Municipio': 'San Sebastián Municipio'}

df_pop.County.replace(county_renames, inplace = True)
df_inc.County.replace(county_renames, inplace = True)

df = pd.merge(df_pop,
              df_inc,
              on=['County', 'State', 'Census_Year'],
              how='outer',
              indicator=True)

df.reset_index(drop = True, inplace = True)


df.drop(columns=['_merge'], inplace=True)

df.to_csv(f"{DATADIR}/Census_weights.csv")
