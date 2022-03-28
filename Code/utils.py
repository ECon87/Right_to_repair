import pandas as pd
import re
import pgeocode
import mpu


def get_lat_long(nom, x):
    """
    Return: (latitude, longitude) of zipcode x.
    Must predefine nom = pgeocode.Nominatim('us')
    """
    x_str = str(x) if len(str(x)) == 5 else f'0{str(x)}'
    query = nom.query_postal_code(x_str)
    return query.latitude, query.longitude, query.county_name, query.state_code


def shortest_distance(df1: pd.core.frame.DataFrame,
                      df2: pd.core.frame.DataFrame, var: str):
    """
    For each zipcode in df1 find the closest zipcode in df2.
    """
    for j in df1.index:
        # brd_lat = df1.loc[j, 'Lat']
        brd_point = df1.loc[j, 'Lat_Long'][:2]
        distances = []
        for i in df2.index:
            # ma_lat = df2.loc[i, 'Lat']
            ma_point = df2.loc[i, 'Lat_Long'][:2]
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


def get_censusyear(x: int) -> int:
    """
    Get corresponding census year.
    If year >= 2007 then returns 2010. Else it returns 2000.
    """
    return 2010 if x >= 2007 else 2000
