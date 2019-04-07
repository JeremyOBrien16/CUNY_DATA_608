# -*- coding: utf-8 -*-
"""
Created on Mon Apr  1 15:59:56 2019

@author: Jeremy
"""

# Create a RESTful Flask API that responds to info from the SOQL API for the NYC Trees Count! 2015-2016 Census:  https://data.cityofnewyork.us/Environment/2015-Street-Tree-Census-Tree-Data/uvpi-gqnh

import pandas as pd
from flask import Flask, jsonify

# Instantiate the app
app = Flask(__name__)

# Call the SOQL URL within each route


# Bind URL to function that returns count of the designated species for each borough
@app.route('/species_boro/<string:species>')  # Designate species in URL
def count_species_boro(species):
    url = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=spc_common,boroname,count(tree_id)' +\
        '&$group=spc_common,boroname').replace(' ', '%20')  # Source counts of trees by species and borough from the SOQL API 
    data = pd.read_json(url)  # Read URL to dataframe
    data = data.dropna()  # Remove rows with NAs, without which throws 'not suppower between instance of str and float' error
    species_count = data[data['spc_common'] == species]  # Filter dataframe on designated species
    return jsonify(pd.Series(species_count.count_tree_id.values, index=species_count.boroname).to_dict())  # Return JSON of count of designated species for each borough


# Bind URL to function that returns count of each tree species within designated borough
@app.route('/boro_species/<string:boro>')  # Designate borough in URL
def count_boro_species(boro):
    url = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
           '$select=boroname,spc_common,count(tree_id)' +\
           '&$group=boroname,spc_common').replace(' ', '%20') # Source counts of trees by borough and species from the SOQL API 
    data = pd.read_json(url)  # Read URL to dataframe
    data = data.dropna()  # Remove rows with NAs, without which throws 'not suppower between instance of str and float' error
    boro_count = data[data['boroname'] == boro]  # Filter dataframe on designated borough
    return jsonify(pd.Series(boro_count.count_tree_id.values, index=boro_count.spc_common).to_dict())  # Return JSON of count of designated species for each borough

# As stretch goal, wanted to try to return health of each species within each borough but could not get the nested dictionary to work correctly with jsonify

# Bind URL to function that returns count of each tree species within designated borough    
@app.route('/boro_species_health/')  # All boroughs, species, and health levels
def count_boro_species_health():
    url = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
           '$select=spc_common,boroname,health,count(tree_id)' +\
           '&$group=boroname,spc_common,health').replace(' ', '%20')  # Source counts of trees by species, borough, and health from the SOQL API 
    data = pd.read_json(url)  # Read URL to dataframe
    data = data.dropna()  # Remove rows with NAs, without which throws 'not suppower between instance of str and float' error
    data = data.set_index(['boroname', 'spc_common'], inplace=True)  # Set multi-index to nest in dictionary
    data = data.sort_index(inplace=True)  # Sort multi-index at both levels
    return jsonify(pd.Series(data.groupby(level=0).apply(lambda data: data.xs(data.boroname).to_dict())))  # Not yet working - return JSON of count of designated species by health for each borough

if __name__ == '__main__':
    app.run(debug=True)