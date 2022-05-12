"""
Download County Fip Codes from USDA
"""
import requests as req

from bs4 import BeautifulSoup

import re
import pandas as pd

DATADIR='/home/econ87/Research/Papers/Right_to_repair/Note/Data/Census/'

site = 'https://www.nrcs.usda.gov/wps/portal/nrcs/detail/national/home/?cid=nrcs143_013697'

site_text = req.get(site).text

site_soup = BeautifulSoup(site_text)

site_rows = site_soup.select("div#detail>table>tbody>tr")

datalist = []
for r in site_rows:
    text = r.getText()
    text2 = re.sub(r'\n{1,2}', ',', text)
    text2 = re.sub(r'\s{2,}', ' ', text2)
    text2 = re.sub(r'(^,\s|,\s$)', '', text2)
    datalist.append(text2)

data = [w.split(", ") for w in datalist]


data_df = pd.DataFrame(data[1:], columns=data[0])

data_df.to_csv(f'{DATADIR}/CountyFips.csv')
