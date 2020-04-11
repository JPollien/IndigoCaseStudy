from urllib.request import urlopen
import json
import pandas as pd

with urlopen('https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json') as response:
    counties = json.load(response)

counties["features"][0]

with urlopen('https://indigocasestudyjtp.herokuapp.com/data') as response:
    datadict = json.load(response)

print(datadict)

rows = []

for data in datadict:
    data_row = data['attributes']
    time = data['id']

    for row in data_row:
        row['id'] = time
        rows.append(row)

df = pd.DataFrame(rows)

#df = pd.DataFrame.from_dict(datadict['data'])#, orient='index')

#datadict2 = datadict[['id', 'attributes']]

#df = pd.DataFrame.from_dict(datadict)
#df2 = pd.DataFrame.from_dict(df)

#df = df[['id', 'attributes']]

#df2 = pd.DataFrame.from_dict(df)

with pd.option_context('display.max_rows', None, 'display.max_columns', None):  # more options can be specified also
    print(df)
