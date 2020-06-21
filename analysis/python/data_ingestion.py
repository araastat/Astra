#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""

@author: abhijit
"""
#%% setup
import os
import numpy as np
from glob import glob
import pandas as pd
import json
from pandas.io.json import json_normalize

#%% ingestions

in_dir = '/Volumes/ARAASTAT/Astra/2020/q1'
json_files = glob(os.path.join(in_dir, '*.json'))

#%% Drug information

with open(json_files[0], 'r') as f:
    d = json.load(f)

for i in range(len(d)):
    d[i]['patid'] = i

dat = json_normalize(d['results'])
dat['patid'] = list(range(dat.shape[0]))

def normalize_drug(d):
    """

    Parameters
    ----------
    d : dict
        data for a subject

    Returns
    -------
    A pandas DataFrame

    """
    keep_cols = ['activesubstance.activesubstancename',
                 'drugindication',
                 'drugcharacterization',
                 'medicinalproduct',
                 'openfda.route',
                 'openfda.product_type']
    if 'drug' not in d['patient'].keys():
        return()
    x = {}
    for k in keep_cols:
        nms = k.split('.')
        if len(nms)==2:
            x[k] = [(u[nms[0]][nms[1]] if nms[1] in u[nms[0]].keys() else np.nan)
                    if nms[0] in u.keys() else np.nan
                    for u in d['patient']['drug']]
        else:
            x[k] = [u[k] if k in u.keys() else np.nan
                    for u in d['patient']['drug']]
    D = pd.DataFrame(x)
    D['patid'] = d['patid']
    return(D)


drugs_2020 = []
for f in json_files:
    print(f)
    with open(f, 'r') as infile:
        d = json.load(infile)['results']
    
    x = f.replace('.json','').split('/')[4:]
    x[2] = x[2].split('-')[2]
    for i in range(len(d)):
        d[i]['patid'] = '_'.join(x)+'_'+str(i)
        drugs_2020.append(normalize_drug(d[i]))

out_file = 'data/drugs_2020.csv'
for i in range(len(drugs_2020)):
    print(i)
    if i==0:
        drugs_2020[i].to_csv(out_file, header=True, index=False, mode='w')
    else:
        drugs_2020[i].to_csv(out_file, header=False, index=False, mode='a')

#%% Other patient information

def normalize_other(d):
    """
    

    Parameters
    ----------
    d : dict
        individual JSON data from FDA adverse events database

    Returns
    -------
    DataFrame

    """
    
    keep_cols=['primarysource.qualification',
               'receivedate',
               'receiptdata',
               'primarysource.reportercountry',
               'patient.patientonsetage',
               'patient.patientsex',
               'patient.patientweight',
               'patient.patientagegroup',
               'serious',
               'seriousnessdeath',
               'seriousnessdisabling',
               'seriousnesshospitalization',
               'seriousnesslifethreatening',
               'seriousnessother']
    keep_cols_reaction = ['reactionmeddrapt',
                          'reactionoutcome']
    
    x = {}
    for k in keep_cols:
        nms = k.split('.')
        if len(nms)==2:
            if nms[0] in d.keys():
                if nms[1] in d[nms[0]].keys():
                    x[k] = d[nms[0]][nms[1]]
                else:
                    x[k] = np.nan
            else:
                x[k]=np.nan
        else:
            if k in d.keys():
                x[k] = d[k]
            else:
                x[k] = np.nan
    for k in keep_cols_reaction:
        nms = k.split('.')
        d0 = json_normalize(d['patient']['reaction'])
        if k in d0.columns:
            x[k] = d0[k]
        else:
            x[k]=np.nan
    D = pd.DataFrame(x)
    D['patid'] = d['patid']
    return(D)

other_2020 = []
for f in json_files:
    print(f)
    with open(f, 'r') as infile:
        d = json.load(infile)['results']
    for i in range(len(d)):
        d[i]['patid'] = i
    x = f.replace('.json','').split('/')[4:]
    x[2] = x[2].split('-')[2]
    for i in range(len(d)):
        d[i]['patid'] = '_'.join(x)+'_'+str(i)
        other_2020.append(normalize_other(d[i]))

out_file = 'data/other_2020.csv'
for i in range(len(other_2020)):
    print(i)
    if i==0:
        other_2020[i].to_csv(out_file, header=True, index=False, mode='w')
    else:
        other_2020[i].to_csv(out_file, header=False, index=False, mode='a')

