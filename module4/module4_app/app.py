# -*- coding: utf-8 -*-
"""
Created on Sat Mar 23 14:11:39 2019

@author: Jeremy
"""

import pandas as pd
import numpy as np
import plotly.plotly as py
import plotly.graph_objs as go
import dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output

external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']  # Revisit CSS style sheets at later date

app = dash.Dash(__name__, external_stylesheets=external_stylesheets)  # Instantiate the app

# Call SOQL API to create borough selection list for dropdown menu
url_boros= ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=boroname').replace(' ', '%20')
df_boros = pd.read_json(url_boros)
available_boros = df_boros['boroname'].unique()  # Create borough selection list for dropdown menu

# Call SOQL API to create tree species selection list for dropdown menu
url_species = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=spc_common,count(tree_id)' +\
        '&$group=spc_common').replace(' ', '%20')
df_species = pd.read_json(url_species)
# don't implement next line based on current SOQL call
# df_species['spc_common'] = df_species['spc_common'].str.title()
available_species = df_species['spc_common'].unique()  

# Manage appearance of app
app.layout = html.Div([
        
        html.H2('Tree Health in NYC'),
        html.H4('Select borough and species:'),
        
        # Create borough dropdown menu
        dcc.Dropdown(
                id = 'Boro',
                options = [{'label': i, 'value': i} for i in available_boros],
                multi = False,
                value = 'Manhattan',
                placeholder = 'Select borough'
            ),
        
        # Create tree species dropdown menu
        dcc.Dropdown(
                id = 'Species',
                options = [{'label': i, 'value': i} for i in available_species],
                multi = False,
                value = 'maple',
                placeholder = 'Select tree species'
            ),

        # Display charts
        html.Br(),
        html.H4(id = 'proportion-chart-title'),  # Consider centering next time
        dcc.Graph(id = 'proportion-chart'),
        
        html.Br(),
        html.H4(id = 'steward-chart-title'),
        html.H6('(Stewarded over Unstewarded)'),
        dcc.Graph(id = 'steward-chart')
        
    ])


# Create dynamic title for first chart based on user selections
@app.callback(
    Output('proportion-chart-title', 'children'),
    [Input('Boro', 'value'), 
     Input('Species', 'value')])
def update_proportion_chart_title(boro, species):
    return 'Proportion of healthy {} trees in {}'.format(species, boro)


# Create dynamic title for second chart based on user selections (consider two output single callback in next iteration)
@app.callback(
    Output('steward-chart-title', 'children'),
    [Input('Boro', 'value'), 
     Input('Species', 'value')])
def update_steward_chart_title(boro, species):
    return 'Tree health and stewardship for {} trees in {}'.format(species, boro)


# Create proportion chart based on user selections
@app.callback(
    Output('proportion-chart', 'figure'),
    [Input('Boro', 'value'),
    Input('Species', 'value')])
def update_proportion_chart(boro, species):

    # Call SOQL API based on user selections
    url_q1 = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
              '$select=health,count(tree_id)' +\
              '&$where=boroname=\'' +\
              boro +\
              '\'' +\
              '&spc_common=\'' +\
              species +\
              '\'' +\
              '&$group=health').replace(' ', '%20')
    
    # Instantiate dataframe for first chart based on queried JSON object
    df_q1 = pd.read_json(url_q1)
    # don't implement based on current SOQL call
    # df_q1['spc_common'] = df_q1['spc_common'].str.title()
    df_q1['prop'] = df_q1['count_tree_id']/df_q1['count_tree_id'].sum()  # Calculate proportions of trees in good, fair, and poor health
    
    # Create good health proportion trace for stacked horizontal bar chart
    q1_trace1 = go.Bar(
            x = df_q1.query('health == "Good"')['prop'].tolist(),
            y = [species],
            name = 'Good',
            text = 'Good',
            textposition = 'auto',  # Centering in bar requires hacky complex solution, revisit later: https://stackoverflow.com/questions/53856826/how-to-align-trace-text-to-center-in-a-stacked-horizontal-bar-chart
            orientation = 'h',
            marker = dict(
                     color = 'rgba(130, 200, 0, 0.5)',
                     line = dict(
                             color = 'rgba(130, 200, 0, 1.0)',
                             width = 6)
                     )
            )
    
    # Create fair health proportion trace for stacked horizontal bar chart
    q1_trace2 = go.Bar(
            x = df_q1.query('health=="Fair"')['prop'].tolist(),
            y=[species],  # [REMOVE LABEL]
            name='Fair',
            text = 'Fair',
            textposition = 'auto',  # Centering in bar requires hacky complex solution, revisit later: https://stackoverflow.com/questions/53856826/how-to-align-trace-text-to-center-in-a-stacked-horizontal-bar-chart
            orientation='h',
            marker=dict(
                    # [ADJUST COLORS]
                     color = 'rgba(240, 240, 50, 0.6)',
                     line = dict(
                             color = 'rgba(240, 240, 50, 1.0)',
                             width = 6)
                     )
            )
 
    # Create poor health proportion trace for stacked horizontal bar chart
    q1_trace3 = go.Bar(
            x = df_q1.query('health=="Poor"')['prop'].tolist(),
            y=[species],  # [REMOVE LABEL]
            name='Poor',
            text = 'Poor',
            textposition = 'auto',  # Centering in bar requires hacky complex solution, revisit later: https://stackoverflow.com/questions/53856826/how-to-align-trace-text-to-center-in-a-stacked-horizontal-bar-chart
            orientation='h',
            marker=dict(
                     color = 'rgba(250, 10, 10, 0.7)',
                     line = dict(
                             color = 'rgba(250, 10, 10, 1.0)',
                             width = 6)
                     )
            )
                     
    return {
        'data': [q1_trace1, q1_trace2, q1_trace3],
        'layout': go.Layout(
            barmode='stack',
            xaxis=dict(showticklabels=False, showgrid=False),
            yaxis=dict(showticklabels=False),
            showlegend=False,
            # Hiding legend, but can resuscitate as bar annotations are not centered
            # legend=dict(orientation='h',
                        # yanchor='top',
                        # xanchor='center',
                        # y=1.1,
                        # x=.5,
                        # traceorder='normal')
            # Alternative manual approach to bar chart annotations - revisit in later iteration
            # annotations=dict(xref='x',
                             #yref='y',
                             #x=[df_q1['prop'][1]] / 2,
                             #y=[species],
                             #text=str([df_q1['prop'][1]]) + '%',
                             #font=dict(size=14,
                                       #color='rgb(248, 248, 255)'),
                            #showarrow=False)
            )
    }


# Create stewarding chart based on user selections
@app.callback(
    Output('steward-chart', 'figure'),
    [Input('Boro', 'value'),
    Input('Species', 'value')])
def update_steward_chart(boro, species):

    # Call SOQL API based on user selections
    url_q2 = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
              '$select=health,steward,count(tree_id)' +\
              '&$where=boroname=\'' +\
              boro +\
              '\'' +\
              '&spc_common=\'' +\
              species +\
              '\'' +\
              '&$group=steward,health' +\
              '&$order=steward,health').replace(' ', '%20')
    
    # Instantiate dataframe for second chart based on queried JSON object
    df_q2 = pd.read_json(url_q2)
    # don't implement based on current SOQL call
    # df_q2['spc_common'] = df_q2['spc_common'].str.title()
    
    # Code steward values as binary, with any signal as positive
    def steward_coding(row):
        if row['steward'] == 'None':
            val = 'No'
        else:
            val = 'Yes'
        return val
    
    # Calculate stewarded / unstewarded totals by tree health levels of good, fair, and poor
    df_q2['steward_code'] = df_q2.apply(steward_coding, axis=1)
    df_q2_t = df_q2.groupby(['health','steward_code']).sum()
    
    # Create unstewarded trace for stacked vertical bar chart
    q2_trace1 = go.Bar(
            x=['Good', 'Fair', 'Poor'],
            y=[df_q2_t.xs(('Good', 'No'))[0], df_q2_t.xs(('Fair', 'No'))[0], df_q2_t.xs(('Poor', 'No'))[0]],
            name='Unstewarded',
            marker=dict(
                    color=['rgba(130, 200, 0, 0.5)',
                     'rgba(240, 240, 50, 0.6)',
                     'rgba(250, 10, 10, 0.7)']
                    ),
    )
    
    # Create stewarded trace for stacked vertical bar chart
    q2_trace2 = go.Bar(
            x=['Good', 'Fair', 'Poor'],
            y=[df_q2_t.xs(('Good', 'Yes'))[0], df_q2_t.xs(('Fair', 'Yes'))[0], df_q2_t.xs(('Poor', 'Yes'))[0]],
            name='Stewarded',
            marker=dict(
                    color=['rgba(130, 200, 0, 1.0)',
                     'rgba(240, 240, 50, 1.0)',
                     'rgba(250, 10, 10, 1.0)']
                    ),
            )

    return {
        'data': [q2_trace1, q2_trace2],
        'layout': go.Layout(
            barmode='stack',
            showlegend=False,
            # Reintroduce annotations to better visualize stewarded / unstewarded stacked bars
            # annotations=[
                    # dict(
                            # x = .5,
                            # y = 1,
                            # xref = 'x',
                            # yref = 'y',
                            # text = 'Stewarded <br> Unstewarded',
                            # font = dict(
                                    # size = 20
                                    # ),
                            # showarrow = False
                            # )
                            # ]
    )
    }

if __name__ == '__main__':
    app.run_server(debug=True)