# -*- coding: utf-8 -*-
"""
Created on Fri Sep  4 09:27:13 2020

@author: oiko957
"""

import glob
from os import path
import numpy as np
import pandas as pd
import os
import importlib
import matplotlib.pyplot as plt

df_hydro_monthly = pd.read_csv('HydroMonthlyVarSchedule.csv')
df_hydro_weekly_tep= pd.read_csv('HydroWeeklyVarSchedule_.csv')
df_name=pd.read_csv('gridview_to_EIA_ID_v2.csv')

## Inputs from https://zenodo.org/records/8408246   
## Select the specific year to generate data from B1_Data/B1_WEEKLY
## Replace the csv file here
df_hydro_weekly= pd.read_csv('../B1_weekly/B1_weekly_2001.csv')

year = 2022

##########TEPPC energy#################################################################################
#################Isolate TEPCC Energy#############################################
df_hydro_monthly_OnlyE=df_hydro_monthly.loc[(df_hydro_monthly['DataTypeName'] == 'MonthlyEnergy'),['Generator Name','Jan', 'Feb', 'Mar','Apr','May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]
df_hydro_monthly_OnlyE.set_index('Generator Name', inplace=True)
df_hydro_monthly_def=df_hydro_monthly_OnlyE.reset_index()
################Filter to GridView Name to EAD id################################
df_hydro_monthly_filter=df_hydro_monthly_def.merge(df_name, how = 'inner' ,indicator=False).sort_values(by=['EIA_ID'])
df_hydro_monthly_filter_fi=df_hydro_monthly_filter.sort_values(by=['EIA_ID'])
################Sum the Energy of all duplicate units############################
df_hydro_monthly_sum=df_hydro_monthly_filter.groupby('EIA_ID').agg('sum')
df_hydro_monthly_sum_fi=df_hydro_monthly_sum.reset_index()
#df_hydro_monthly_sum_fi=df_hydro_monthly_sum_in.drop('BusID', axis=1)
############### Dissagregate back to all plants##################################
df_hydro_monthly_dis_sum=df_hydro_monthly_sum_fi.merge(df_name, how = 'inner' ,indicator=False)

###############Computes allocation factors########################################
df_hydro_monthly_filter_fi[['Jan', 'Feb', 'Mar','Apr','May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]=df_hydro_monthly_filter_fi[['Jan', 'Feb', 'Mar','Apr','May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']].div(df_hydro_monthly_dis_sum[['Jan', 'Feb', 'Mar','Apr','May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']].values,axis=0)
##############The code below fixes the nan values appeared in some cells#########
dupli_mean=df_hydro_monthly_filter_fi.pivot_table(index=['EIA_ID'], aggfunc='size')
dupli_frame_mean=dupli_mean.to_frame().reset_index()
dupli_frame_mean.columns=['EIA_ID', 'Dupli']
dupli_frame_mean['mean']=1/dupli_frame_mean['Dupli']

df_mean_with_dupli= df_hydro_monthly_filter_fi.merge(dupli_frame_mean, how = 'inner' ,indicator=False)

df_mean_with_dupli.loc[df_mean_with_dupli['Jan'].isnull(),'Jan'] = df_mean_with_dupli['mean']
df_mean_with_dupli.loc[df_mean_with_dupli['Feb'].isnull(),'Feb'] = df_mean_with_dupli['mean']
df_mean_with_dupli.loc[df_mean_with_dupli['Mar'].isnull(),'Mar'] = df_mean_with_dupli['mean']
df_mean_with_dupli.loc[df_mean_with_dupli['Apr'].isnull(),'Apr'] = df_mean_with_dupli['mean']
df_mean_with_dupli.loc[df_mean_with_dupli['May'].isnull(),'May'] = df_mean_with_dupli['mean']
df_mean_with_dupli.loc[df_mean_with_dupli['Jun'].isnull(),'Jun'] = df_mean_with_dupli['mean']
df_mean_with_dupli.loc[df_mean_with_dupli['Jul'].isnull(),'Jul'] = df_mean_with_dupli['mean']
df_mean_with_dupli.loc[df_mean_with_dupli['Aug'].isnull(),'Aug'] = df_mean_with_dupli['mean']
df_mean_with_dupli.loc[df_mean_with_dupli['Sep'].isnull(),'Sep'] = df_mean_with_dupli['mean']
df_mean_with_dupli.loc[df_mean_with_dupli['Oct'].isnull(),'Oct'] = df_mean_with_dupli['mean']
df_mean_with_dupli.loc[df_mean_with_dupli['Nov'].isnull(),'Nov'] = df_mean_with_dupli['mean']
df_mean_with_dupli.loc[df_mean_with_dupli['Dec'].isnull(),'Dec'] = df_mean_with_dupli['mean']

df_weight_mean=df_mean_with_dupli

df_weight_mean['Week1']=df_weight_mean['Jan']
df_weight_mean['Week2']=df_weight_mean['Jan']
df_weight_mean['Week3']=df_weight_mean['Jan']
df_weight_mean['Week4']=df_weight_mean['Jan']
df_weight_mean['Week5']=df_weight_mean['Jan']

df_weight_mean['Week6']=df_weight_mean['Feb']
df_weight_mean['Week7']=df_weight_mean['Feb']
df_weight_mean['Week8']=df_weight_mean['Feb']
df_weight_mean['Week9']=df_weight_mean['Feb']

df_weight_mean['Week10']=df_weight_mean['Mar']
df_weight_mean['Week11']=df_weight_mean['Mar']
df_weight_mean['Week12']=df_weight_mean['Mar']
df_weight_mean['Week13']=df_weight_mean['Mar']

df_weight_mean['Week14']=df_weight_mean['Apr']
df_weight_mean['Week15']=df_weight_mean['Apr']
df_weight_mean['Week16']=df_weight_mean['Apr']
df_weight_mean['Week17']=df_weight_mean['Apr']

df_weight_mean['Week18']=df_weight_mean['May']
df_weight_mean['Week19']=df_weight_mean['May']
df_weight_mean['Week20']=df_weight_mean['May']
df_weight_mean['Week21']=df_weight_mean['May']

df_weight_mean['Week22']=df_weight_mean['Jun']
df_weight_mean['Week23']=df_weight_mean['Jun']
df_weight_mean['Week24']=df_weight_mean['Jun']
df_weight_mean['Week25']=df_weight_mean['Jun']

df_weight_mean['Week26']=df_weight_mean['Jul']
df_weight_mean['Week27']=df_weight_mean['Jul']
df_weight_mean['Week28']=df_weight_mean['Jul']
df_weight_mean['Week29']=df_weight_mean['Jul']
df_weight_mean['Week30']=df_weight_mean['Jul']

df_weight_mean['Week31']=df_weight_mean['Aug']
df_weight_mean['Week32']=df_weight_mean['Aug']
df_weight_mean['Week33']=df_weight_mean['Aug']
df_weight_mean['Week34']=df_weight_mean['Aug']
df_weight_mean['Week35']=df_weight_mean['Aug']

df_weight_mean['Week36']=df_weight_mean['Sep']
df_weight_mean['Week37']=df_weight_mean['Sep']
df_weight_mean['Week38']=df_weight_mean['Sep']
df_weight_mean['Week39']=df_weight_mean['Sep']

df_weight_mean['Week40']=df_weight_mean['Oct']
df_weight_mean['Week41']=df_weight_mean['Oct']
df_weight_mean['Week42']=df_weight_mean['Oct']
df_weight_mean['Week43']=df_weight_mean['Oct']
df_weight_mean['Week44']=df_weight_mean['Oct']

df_weight_mean['Week45']=df_weight_mean['Nov']
df_weight_mean['Week46']=df_weight_mean['Nov']
df_weight_mean['Week47']=df_weight_mean['Nov']
df_weight_mean['Week48']=df_weight_mean['Nov']

df_weight_mean['Week49']=df_weight_mean['Dec']
df_weight_mean['Week50']=df_weight_mean['Dec']
df_weight_mean['Week51']=df_weight_mean['Dec']
df_weight_mean['Week52']=df_weight_mean['Dec']

df_weight_mean['Week53']=df_weight_mean['Dec']

df_weight_mean=df_weight_mean.drop(['Jan'], axis = 1) 
df_weight_mean=df_weight_mean.drop(['Feb'], axis = 1) 
df_weight_mean=df_weight_mean.drop(['Mar'], axis = 1) 
df_weight_mean=df_weight_mean.drop(['Apr'], axis = 1) 
df_weight_mean=df_weight_mean.drop(['May'], axis = 1) 
df_weight_mean=df_weight_mean.drop(['Jun'], axis = 1) 
df_weight_mean=df_weight_mean.drop(['Jul'], axis = 1) 
df_weight_mean=df_weight_mean.drop(['Aug'], axis = 1) 
df_weight_mean=df_weight_mean.drop(['Sep'], axis = 1) 
df_weight_mean=df_weight_mean.drop(['Oct'], axis = 1) 
df_weight_mean=df_weight_mean.drop(['Nov'], axis = 1) 
df_weight_mean=df_weight_mean.drop(['Dec'], axis = 1) 




#########TEPCC Pmin#####################################################################
df_hydro_monthly_Only_Min=df_hydro_monthly.loc[(df_hydro_monthly['DataTypeName'] == 'MinGen'),['Generator Name','Jan', 'Feb', 'Mar','Apr','May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]
df_hydro_monthly_Only_Min.set_index('Generator Name', inplace=True)
hydro_month_Final_Min=df_hydro_monthly_Only_Min.reset_index()
df_hydro_monthly_filter_Min=hydro_month_Final_Min.merge(df_name, how = 'inner' ,indicator=False).sort_values(by=['EIA_ID'])
df_hydro_monthly_filter_Min_fi=df_hydro_monthly_filter_Min.sort_values(by=['EIA_ID'])
df_hydro_monthly_Min_sum=df_hydro_monthly_filter_Min.groupby('EIA_ID').agg('sum')

df_hydro_monthly_Min_sum_fi=df_hydro_monthly_Min_sum.reset_index()
#df_hydro_monthly_Min_sum_fi=df_hydro_monthly_Min_sum_in.drop('BusID', axis=1)

df_hydro_monthly_Min_dis_sum=df_hydro_monthly_Min_sum_fi.merge(df_name, how = 'inner' ,indicator=False)
df_hydro_monthly_filter_Min_fi[['Jan', 'Feb', 'Mar','Apr','May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]=df_hydro_monthly_filter_Min_fi[['Jan', 'Feb', 'Mar','Apr','May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']].div(df_hydro_monthly_Min_dis_sum[['Jan', 'Feb', 'Mar','Apr','May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']].values,axis=0)

df_hydro_monthly_filter_Min_fi.pivot_table(index=['EIA_ID'], aggfunc='size')

dupli_min=df_hydro_monthly_filter_Min_fi.pivot_table(index=['EIA_ID'], aggfunc='size')
dupli_frame_min=dupli_min.to_frame().reset_index()
dupli_frame_min.columns=['EIA_ID', 'Dupli']
dupli_frame_min['mean']=1/dupli_frame_min['Dupli']

df_min_with_dupli= df_hydro_monthly_filter_Min_fi.merge(dupli_frame_min, how = 'inner' ,indicator=False)

df_min_with_dupli.loc[df_min_with_dupli['Jan'].isnull(),'Jan'] = df_min_with_dupli['mean']
df_min_with_dupli.loc[df_min_with_dupli['Feb'].isnull(),'Feb'] = df_min_with_dupli['mean']
df_min_with_dupli.loc[df_min_with_dupli['Mar'].isnull(),'Mar'] = df_min_with_dupli['mean']
df_min_with_dupli.loc[df_min_with_dupli['Apr'].isnull(),'Apr'] = df_min_with_dupli['mean']
df_min_with_dupli.loc[df_min_with_dupli['May'].isnull(),'May'] = df_min_with_dupli['mean']
df_min_with_dupli.loc[df_min_with_dupli['Jun'].isnull(),'Jun'] = df_min_with_dupli['mean']
df_min_with_dupli.loc[df_min_with_dupli['Jul'].isnull(),'Jul'] = df_min_with_dupli['mean']
df_min_with_dupli.loc[df_min_with_dupli['Aug'].isnull(),'Aug'] = df_min_with_dupli['mean']
df_min_with_dupli.loc[df_min_with_dupli['Sep'].isnull(),'Sep'] = df_min_with_dupli['mean']
df_min_with_dupli.loc[df_min_with_dupli['Oct'].isnull(),'Oct'] = df_min_with_dupli['mean']
df_min_with_dupli.loc[df_min_with_dupli['Nov'].isnull(),'Nov'] = df_min_with_dupli['mean']
df_min_with_dupli.loc[df_min_with_dupli['Dec'].isnull(),'Dec'] = df_min_with_dupli['mean']


df_weight_min=df_min_with_dupli

df_weight_min['Week1']=df_weight_min['Jan']
df_weight_min['Week2']=df_weight_min['Jan']
df_weight_min['Week3']=df_weight_min['Jan']
df_weight_min['Week4']=df_weight_min['Jan']
df_weight_min['Week5']=df_weight_min['Jan']

df_weight_min['Week6']=df_weight_min['Feb']
df_weight_min['Week7']=df_weight_min['Feb']
df_weight_min['Week8']=df_weight_min['Feb']
df_weight_min['Week9']=df_weight_min['Feb']

df_weight_min['Week10']=df_weight_min['Mar']
df_weight_min['Week11']=df_weight_min['Mar']
df_weight_min['Week12']=df_weight_min['Mar']
df_weight_min['Week13']=df_weight_min['Mar']

df_weight_min['Week14']=df_weight_min['Apr']
df_weight_min['Week15']=df_weight_min['Apr']
df_weight_min['Week16']=df_weight_min['Apr']
df_weight_min['Week17']=df_weight_min['Apr']

df_weight_min['Week18']=df_weight_min['May']
df_weight_min['Week19']=df_weight_min['May']
df_weight_min['Week20']=df_weight_min['May']
df_weight_min['Week21']=df_weight_min['May']

df_weight_min['Week22']=df_weight_min['Jun']
df_weight_min['Week23']=df_weight_min['Jun']
df_weight_min['Week24']=df_weight_min['Jun']
df_weight_min['Week25']=df_weight_min['Jun']

df_weight_min['Week26']=df_weight_min['Jul']
df_weight_min['Week27']=df_weight_min['Jul']
df_weight_min['Week28']=df_weight_min['Jul']
df_weight_min['Week29']=df_weight_min['Jul']
df_weight_min['Week30']=df_weight_min['Jul']

df_weight_min['Week31']=df_weight_min['Aug']
df_weight_min['Week32']=df_weight_min['Aug']
df_weight_min['Week33']=df_weight_min['Aug']
df_weight_min['Week34']=df_weight_min['Aug']
df_weight_min['Week35']=df_weight_min['Aug']

df_weight_min['Week36']=df_weight_min['Sep']
df_weight_min['Week37']=df_weight_min['Sep']
df_weight_min['Week38']=df_weight_min['Sep']
df_weight_min['Week39']=df_weight_min['Sep']

df_weight_min['Week40']=df_weight_min['Oct']
df_weight_min['Week41']=df_weight_min['Oct']
df_weight_min['Week42']=df_weight_min['Oct']
df_weight_min['Week43']=df_weight_min['Oct']
df_weight_min['Week44']=df_weight_min['Oct']

df_weight_min['Week45']=df_weight_min['Nov']
df_weight_min['Week46']=df_weight_min['Nov']
df_weight_min['Week47']=df_weight_min['Nov']
df_weight_min['Week48']=df_weight_min['Nov']

df_weight_min['Week49']=df_weight_min['Dec']
df_weight_min['Week50']=df_weight_min['Dec']
df_weight_min['Week51']=df_weight_min['Dec']
df_weight_min['Week52']=df_weight_min['Dec']

df_weight_min['Week53']=df_weight_min['Dec']

df_weight_min=df_weight_min.drop(['Jan'], axis = 1) 
df_weight_min=df_weight_min.drop(['Feb'], axis = 1) 
df_weight_min=df_weight_min.drop(['Mar'], axis = 1) 
df_weight_min=df_weight_min.drop(['Apr'], axis = 1) 
df_weight_min=df_weight_min.drop(['May'], axis = 1) 
df_weight_min=df_weight_min.drop(['Jun'], axis = 1) 
df_weight_min=df_weight_min.drop(['Jul'], axis = 1) 
df_weight_min=df_weight_min.drop(['Aug'], axis = 1) 
df_weight_min=df_weight_min.drop(['Sep'], axis = 1) 
df_weight_min=df_weight_min.drop(['Oct'], axis = 1) 
df_weight_min=df_weight_min.drop(['Nov'], axis = 1) 
df_weight_min=df_weight_min.drop(['Dec'], axis = 1) 




##########TEPPC Pmax########################################################################
df_hydro_monthly_Only_Max=df_hydro_monthly.loc[(df_hydro_monthly['DataTypeName'] == 'MaxCap'),['Generator Name','Jan', 'Feb', 'Mar','Apr','May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]
df_hydro_monthly_Only_Max.set_index('Generator Name', inplace=True)
hydro_month_Final_Max=df_hydro_monthly_Only_Max.reset_index()
df_hydro_monthly_filter_Max=hydro_month_Final_Max.merge(df_name, how = 'inner' ,indicator=False).sort_values(by=['EIA_ID'])
df_hydro_monthly_filter_Max_fi=df_hydro_monthly_filter_Max.sort_values(by=['EIA_ID'])
df_hydro_monthly_Max_sum=df_hydro_monthly_filter_Max_fi.groupby('EIA_ID').agg('sum')

df_hydro_monthly_Max_sum_fi=df_hydro_monthly_Max_sum.reset_index()
#df_hydro_monthly_Max_sum_fi=df_hydro_monthly_Max_sum_in.drop('BusID', axis=1)

df_hydro_monthly_Max_dis_sum=df_hydro_monthly_Max_sum_fi.merge(df_name, how = 'inner' ,indicator=False)
df_hydro_monthly_filter_Max_fi[['Jan', 'Feb', 'Mar','Apr','May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]=df_hydro_monthly_filter_Max_fi[['Jan', 'Feb', 'Mar','Apr','May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']].div(df_hydro_monthly_Max_dis_sum[['Jan', 'Feb', 'Mar','Apr','May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']].values,axis=0)


dupli_max=df_hydro_monthly_filter_Max_fi.pivot_table(index=['EIA_ID'], aggfunc='size')
dupli_frame_max=dupli_max.to_frame().reset_index()
dupli_frame_max.columns=['EIA_ID', 'Dupli']
dupli_frame_max['mean']=1/dupli_frame_max['Dupli']

df_max_with_dupli= df_hydro_monthly_filter_Max_fi.merge(dupli_frame_max, how = 'inner' ,indicator=False)

df_max_with_dupli.loc[df_max_with_dupli['Jan'].isnull(),'Jan'] = df_max_with_dupli['mean']
df_max_with_dupli.loc[df_max_with_dupli['Feb'].isnull(),'Feb'] = df_max_with_dupli['mean']
df_max_with_dupli.loc[df_max_with_dupli['Mar'].isnull(),'Mar'] = df_max_with_dupli['mean']
df_max_with_dupli.loc[df_max_with_dupli['Apr'].isnull(),'Apr'] = df_max_with_dupli['mean']
df_max_with_dupli.loc[df_max_with_dupli['May'].isnull(),'May'] = df_max_with_dupli['mean']
df_max_with_dupli.loc[df_max_with_dupli['Jun'].isnull(),'Jun'] = df_max_with_dupli['mean']
df_max_with_dupli.loc[df_max_with_dupli['Jul'].isnull(),'Jul'] = df_max_with_dupli['mean']
df_max_with_dupli.loc[df_max_with_dupli['Aug'].isnull(),'Aug'] = df_max_with_dupli['mean']
df_max_with_dupli.loc[df_max_with_dupli['Sep'].isnull(),'Sep'] = df_max_with_dupli['mean']
df_max_with_dupli.loc[df_max_with_dupli['Oct'].isnull(),'Oct'] = df_max_with_dupli['mean']
df_max_with_dupli.loc[df_max_with_dupli['Nov'].isnull(),'Nov'] = df_max_with_dupli['mean']
df_max_with_dupli.loc[df_max_with_dupli['Dec'].isnull(),'Dec'] = df_max_with_dupli['mean']


df_weight_max=df_max_with_dupli

df_weight_max['Week1']=df_weight_max['Jan']
df_weight_max['Week2']=df_weight_max['Jan']
df_weight_max['Week3']=df_weight_max['Jan']
df_weight_max['Week4']=df_weight_max['Jan']
df_weight_max['Week5']=df_weight_max['Jan']

df_weight_max['Week6']=df_weight_max['Feb']
df_weight_max['Week7']=df_weight_max['Feb']
df_weight_max['Week8']=df_weight_max['Feb']
df_weight_max['Week9']=df_weight_max['Feb']

df_weight_max['Week10']=df_weight_max['Mar']
df_weight_max['Week11']=df_weight_max['Mar']
df_weight_max['Week12']=df_weight_max['Mar']
df_weight_max['Week13']=df_weight_max['Mar']

df_weight_max['Week14']=df_weight_max['Apr']
df_weight_max['Week15']=df_weight_max['Apr']
df_weight_max['Week16']=df_weight_max['Apr']
df_weight_max['Week17']=df_weight_max['Apr']

df_weight_max['Week18']=df_weight_max['May']
df_weight_max['Week19']=df_weight_max['May']
df_weight_max['Week20']=df_weight_max['May']
df_weight_max['Week21']=df_weight_max['May']

df_weight_max['Week22']=df_weight_max['Jun']
df_weight_max['Week23']=df_weight_max['Jun']
df_weight_max['Week24']=df_weight_max['Jun']
df_weight_max['Week25']=df_weight_max['Jun']

df_weight_max['Week26']=df_weight_max['Jul']
df_weight_max['Week27']=df_weight_max['Jul']
df_weight_max['Week28']=df_weight_max['Jul']
df_weight_max['Week29']=df_weight_max['Jul']
df_weight_max['Week30']=df_weight_max['Jul']

df_weight_max['Week31']=df_weight_max['Aug']
df_weight_max['Week32']=df_weight_max['Aug']
df_weight_max['Week33']=df_weight_max['Aug']
df_weight_max['Week34']=df_weight_max['Aug']
df_weight_max['Week35']=df_weight_max['Aug']

df_weight_max['Week36']=df_weight_max['Sep']
df_weight_max['Week37']=df_weight_max['Sep']
df_weight_max['Week38']=df_weight_max['Sep']
df_weight_max['Week39']=df_weight_max['Sep']

df_weight_max['Week40']=df_weight_max['Oct']
df_weight_max['Week41']=df_weight_max['Oct']
df_weight_max['Week42']=df_weight_max['Oct']
df_weight_max['Week43']=df_weight_max['Oct']
df_weight_max['Week44']=df_weight_max['Oct']

df_weight_max['Week45']=df_weight_max['Nov']
df_weight_max['Week46']=df_weight_max['Nov']
df_weight_max['Week47']=df_weight_max['Nov']
df_weight_max['Week48']=df_weight_max['Nov']

df_weight_max['Week49']=df_weight_max['Dec']
df_weight_max['Week50']=df_weight_max['Dec']
df_weight_max['Week51']=df_weight_max['Dec']
df_weight_max['Week52']=df_weight_max['Dec']

df_weight_max['Week53']=df_weight_max['Dec']

df_weight_max=df_weight_max.drop(['Jan'], axis = 1) 
df_weight_max=df_weight_max.drop(['Feb'], axis = 1) 
df_weight_max=df_weight_max.drop(['Mar'], axis = 1) 
df_weight_max=df_weight_max.drop(['Apr'], axis = 1) 
df_weight_max=df_weight_max.drop(['May'], axis = 1) 
df_weight_max=df_weight_max.drop(['Jun'], axis = 1) 
df_weight_max=df_weight_max.drop(['Jul'], axis = 1) 
df_weight_max=df_weight_max.drop(['Aug'], axis = 1) 
df_weight_max=df_weight_max.drop(['Sep'], axis = 1) 
df_weight_max=df_weight_max.drop(['Oct'], axis = 1) 
df_weight_max=df_weight_max.drop(['Nov'], axis = 1) 
df_weight_max=df_weight_max.drop(['Dec'], axis = 1) 





###################PNNL Energy Dataset################################################
#######Computes montly energy budget################
#df_hydro_weekly['Energy']=df_hydro_weekly['mean']



df_mean_order=df_hydro_weekly.pivot_table(index='EIA_ID', columns='jweek', values='target_MWh').reset_index()
df_mean_order.columns=['EIA_ID','Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52', 'Week53']
# df_mean_order['Week53']=df_mean_order['Week52']
df_mean_order['DataTypeName'] = 'WeeklyEnergy'

######Make sure filtered equal for multiplication
df_nam_filtered=df_hydro_monthly_dis_sum[['EIA_ID','Generator Name']]
############dissaggregates PNNL plants energy budget to units##############################
df_mean_order_name = df_mean_order.merge(df_nam_filtered, how = 'inner' ,indicator=False).reset_index(drop=True)
df_mean_order_filter=df_mean_order_name[['Generator Name','DataTypeName']]
############Associate PNNL plants to allocation values##############################
df_weight_mean_filter=df_weight_mean.merge(df_mean_order_filter, how = 'inner' ,indicator=False).reset_index(drop=True)
############Multiplies allocation values with PNNL energy budget ##############################

df_mean_order_name[['Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']]=df_mean_order_name[['Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']]*df_weight_mean_filter[['Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']]


#############Updates TEPPC Database with PNNL energy budget ###############
df_mean_order_name['DatatypeID'] = 5
df_mean_order_name['Year'] = 0
df_mean_order_name=df_mean_order_name[['Generator Name','DataTypeName','DatatypeID','Year','Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']]
#############Drops Energy budget from TEPPC components###############
hydro_week_noEn=df_hydro_weekly_tep[df_hydro_weekly_tep.DataTypeName != 'WeeklyEnergy']
#############Concacts Back Energy ##########################
frames = [hydro_week_noEn,df_mean_order_name]
hydro_week_updated_Energy= pd.concat(frames)


#############PNNL Pminimum_dataset#################################
df_min_order=df_hydro_weekly.pivot(index='EIA_ID', columns='jweek', values='p_min').reset_index()
df_min_order.columns=['EIA_ID','Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']
#df_min_order['Week53']=df_min_order['Week52']
df_min_order['DataTypeName'] = 'MinGen'
df_2001_min_order_F = df_min_order.merge(df_nam_filtered, how = 'inner' ,indicator=False)
df_min_order_filter=df_2001_min_order_F[['Generator Name','DataTypeName']]
df_weight_min_filter=df_weight_min.merge(df_min_order_filter, how = 'inner' ,indicator=False)
############Multiplies allocation values with PNNL energy budget ##############################
df_2001_min_order_F[['Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']]=df_2001_min_order_F[['Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']]*df_weight_min_filter[['Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']]
#############All Pmin_component_Updated###############
df_2001_min_order_F['DataTypeName'] = 'MinGen'
df_2001_min_order_F['DatatypeID'] = 3
df_2001_min_order_F['Year'] = 0
df_2001_min_order_F=df_2001_min_order_F[['Generator Name','DataTypeName','DatatypeID','Year','Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']]
#dftest=df_test2.merge(df_name_only, how = 'inner' ,indicator=False)
#dftest.to_csv("Pmin_2009_default.csv", index=False)
#############All_Non_Min_component#######################
hydro_monthly_noMin=hydro_week_updated_Energy[hydro_week_updated_Energy.DataTypeName != 'MinGen']
#############Concact Back Energy and Pmin##########################
frames1 = [hydro_monthly_noMin,df_2001_min_order_F]
hydro_monthly_updated_Energy_Min= pd.concat(frames1)


#############PNNL Pmaximum Dataset###############################
df_max_order=df_hydro_weekly.pivot(index='EIA_ID', columns='jweek', values='p_max').reset_index()
df_max_order.columns=['EIA_ID','Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']
#df_max_order['Week53']=df_min_order['Week52']
df_max_order['DataTypeName'] = 'MaxGen'
df_2001_max_order_F = df_max_order.merge(df_nam_filtered, how = 'inner' ,indicator=False)
df_max_order_filter=df_2001_max_order_F[['Generator Name','DataTypeName']]
df_weight_max_filter=df_weight_max.merge(df_max_order_filter, how = 'inner' ,indicator=False)
############Multiplies allocation values with PNNL energy budget ##############################
df_2001_max_order_F[['Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']]=df_2001_max_order_F[['Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']]*df_weight_max_filter[['Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']]
#############All Pmin_component_Updated###############
df_2001_max_order_F['DataTypeName'] = 'MaxGen'
df_2001_max_order_F['DatatypeID'] = 4
df_2001_max_order_F['Year'] = 0
df_2001_max_order_F=df_2001_max_order_F[['Generator Name','DataTypeName','DatatypeID','Year','Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']]
#dftest=df_test2.merge(df_name_only, how = 'inner' ,indicator=False)
#dftest.to_csv("Pmin_2009_default.csv", index=False)
#############All_Non_Min_component#######################
hydro_monthly_noMax=hydro_monthly_updated_Energy_Min[hydro_monthly_updated_Energy_Min.DataTypeName != 'MaxGen']
#############Concact Back Energy and Pmin##########################
frames2 = [hydro_monthly_noMax,df_2001_max_order_F]
hydro_monthly_updated_Energy_Min_Max= pd.concat(frames2)


#############PNNL Average Dataset###############################
df_avg_order=df_hydro_weekly.pivot(index='EIA_ID', columns='jweek', values='ador').reset_index()
df_avg_order.columns=['EIA_ID','Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']
#df_max_order['Week53']=df_min_order['Week52']
df_avg_order['DataTypeName'] = 'DailyOpRange'
df_2001_avg_order_F = df_avg_order.merge(df_nam_filtered, how = 'inner' ,indicator=False)
df_avg_order_filter=df_2001_avg_order_F[['Generator Name','DataTypeName']]
df_weight_avg_filter=df_weight_max.merge(df_avg_order_filter, how = 'inner' ,indicator=False)
############Multiplies allocation values with PNNL energy budget ##############################
df_2001_avg_order_F[['Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']]=df_2001_avg_order_F[['Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']]*df_weight_avg_filter[['Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']]
#############All Pmin_component_Updated###############
df_2001_avg_order_F['DataTypeName'] = 'DailyOpRange'
df_2001_avg_order_F['DatatypeID'] = 11
df_2001_avg_order_F['Year'] = 0
df_2001_avg_order_F=df_2001_avg_order_F[['Generator Name','DataTypeName','DatatypeID','Year','Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']]
#dftest=df_test2.merge(df_name_only, how = 'inner' ,indicator=False)
#dftest.to_csv("Pmin_2009_default.csv", index=False)
#############All_Non_Min_component#######################
hydro_monthly_noAvg=hydro_monthly_updated_Energy_Min_Max[hydro_monthly_updated_Energy_Min_Max.DataTypeName != 'DailyOpRange']
#############Concact Back Energy and Pmin##########################
frames3 = [hydro_monthly_noAvg,df_2001_avg_order_F]
hydro_monthly_updated_Energy_Min_Max_Avg= pd.concat(frames3)





#############PNNL Pmaximum Dataset###############################
# df_k=df_hydro_weekly.pivot(index='EIA_ID', columns='Week', values='k').reset_index()

# df_k.columns=['EIA_ID','Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']

# df_k['Week53']=df_k['Week52']
# df_k['DataTypeName'] = 'KLoadFollowing'
# df_2001_k_order_F = df_k.merge(df_name, how = 'inner' ,indicator=False)
# df_k_order_filter=df_2001_k_order_F[['Generator Name','DataTypeName']]
# df_weight_max_filter=df_weight_max.merge(df_max_order_filter, how = 'inner' ,indicator=False)

# df_2001_k_order_F['DatatypeID'] = 6
# df_2001_k_order_F['Year'] = 0
# df_k_order_filter=df_2001_k_order_F[['Generator Name','DataTypeName','DatatypeID','Year','Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']]


# df_p=df_hydro_weekly.pivot(index='EIA_ID', columns='Week', values='p').reset_index()
# df_p.columns=['EIA_ID','Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']
# df_p['Week53']=df_p['Week52']
# df_p['DataTypeName'] = 'Pvalue'
# df_2001_p_order_F = df_p.merge(df_name, how = 'inner' ,indicator=False)
# df_2001_p_order_F['DatatypeID'] = 10
# df_2001_p_order_F['Year'] = 0
# df_p_order_filter=df_2001_p_order_F[['Generator Name','DataTypeName','DatatypeID','Year','Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']]


# frames3 = [hydro_monthly_updated_Energy_Min_Max,df_k_order_filter,df_p_order_filter]
# hydro_monthly_updated_Energy_Min_Max_plf= pd.concat(frames3)




############Reorderl##################################
# hydro_monthly_updated_Energy_Min_Max_Avg['C'] = hydro_monthly_updated_Energy_Min_Max_Avg.groupby('DatatypeID')['DatatypeID'].cumcount()
# hydro_monthly_updated_Energy_Min_Max_Avg.sort_values(by=['C', 'DatatypeID'], inplace=True)
# hydro_monthly_updated_Energy_Min_Max_Avg=hydro_monthly_updated_Energy_Min_Max_Avg.drop(['C'], axis = 1) 

folder_path = 'weekly_outputs'
if not os.path.exists(folder_path):
    os.makedirs(folder_path)



hydro_monthly_updated_Energy_Min_Max_Avg.to_csv(os.path.join(folder_path, f'WEEKLY_Outputs_{year}.csv'), index=False)


#############Export##################################
# hydro_monthly_updated_Energy_Min_Max_Avg.to_csv("2012_weekly.csv", index=False)
#df_mean_order_name.to_csv("Pmean_ANL_weekly.csv", index=False)
#df_2001_min_order_F.to_csv("Pmin_ANL_weekly.csv", index=False)
#df_2001_max_order_F.to_csv("Pmax_ANL_weekly.csv", index=False)
#
#













###################k factor#############################
#df_max_order=df_hydro_weekly.pivot(index='EIA_ID', columns='Week', values='max').reset_index()
#df_max_order.columns=['EIA_ID','Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']
##df_max_order['Week53']=df_min_order['Week52']
#df_max_order['DataTypeName'] = 'MaxGen'
#df_2001_max_order_F = df_max_order.merge(df_name, how = 'inner' ,indicator=False)
#df_max_order_filter=df_2001_max_order_F[['Generator Name','DataTypeName']]
#df_weight_max_filter=df_weight_max.merge(df_max_order_filter, how = 'inner' ,indicator=False)
#z############Multiplies allocation values with PNNL energy budget ##############################
#df_2001_max_order_F[['Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']]=df_2001_max_order_F[['Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']]*df_weight_max_filter[['Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']]
##############All Pmin_component_Updated###############
#df_2001_max_order_F['DataTypeName'] = 'MaxGen'
#df_2001_max_order_F['DatatypeID'] = 4
#df_2001_max_order_F['Year'] = 0
#df_2001_max_order_F=df_2001_max_order_F[['Generator Name','DataTypeName','DatatypeID','Year','Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']]
##dftest=df_test2.merge(df_name_only, how = 'inner' ,indicator=False)
##dftest.to_csv("Pmin_2009_default.csv", index=False)
##############All_Non_Min_component#######################
#hydro_monthly_noMax=hydro_monthly_updated_Energy_Min[hydro_monthly_updated_Energy_Min.DataTypeName != 'MaxGen']
##############Concact Back Energy and Pmin##########################
#frames2 = [hydro_monthly_noMax,df_2001_max_order_F]
#hydro_monthly_updated_Energy_Min_Max= pd.concat(frames2)



##############takes k and p from TEPPC################
#
#df_KP = df_hydro_monthly.merge(df_name, how = 'inner' ,indicator=False)
#
#df_K=df_KP.loc[(df_KP['DataTypeName'] == 'KLoadFollowing'),['Generator Name','Jan', 'Feb', 'Mar','Apr','May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]
#df_P=df_KP.loc[(df_KP['DataTypeName'] == 'Pvalue'),['Generator Name','Jan', 'Feb', 'Mar','Apr','May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]
#
#
#name_genK=df_K['Generator Name']
#name_genP=df_P['Generator Name']

#
#j1=df_K['Jan']/4
#j2=df_K['Jan']/4
#j3=df_K['Jan']/4
#j4=df_K['Jan']/4
#
#j5=df_K['Feb']/4
#j6=df_K['Feb']/4
#j7=df_K['Feb']/4
#j8=df_K['Feb']/4
#
#j9=df_K['Mar']/4
#j10=df_K['Mar']/4
#j11=df_K['Mar']/4
#j12=df_K['Mar']/4
#
#j13=df_K['Apr']/4
#j14=df_K['Apr']/4
#j15=df_K['Apr']/4
#j16=df_K['Apr']/4
#
#j17=df_K['May']/4
#j18=df_K['May']/4
#j19=df_K['May']/4
#j20=df_K['May']/4
#
#j21=df_K['Jun']/4
#j22=df_K['Jun']/4
#j23=df_K['Jun']/4
#j24=df_K['Jun']/4
#
#j25=df_K['Jul']/4
#j26=df_K['Jul']/4
#j27=df_K['Jul']/4
#j28=df_K['Jul']/4
#
#j29=df_K['Aug']/4
#j30=df_K['Aug']/4
#j31=df_K['Aug']/4
#j32=df_K['Aug']/4
#
#
#j33=df_K['Sep']/4
#j34=df_K['Sep']/4
#j35=df_K['Sep']/4
#j36=df_K['Sep']/4
#
#
#j37=df_K['Oct']/4
#j38=df_K['Oct']/4
#j39=df_K['Oct']/4
#j40=df_K['Oct']/4
#
#j41=df_K['Nov']/4
#j42=df_K['Nov']/4
#j43=df_K['Nov']/4
#j44=df_K['Nov']/4
#
#j45=df_K['Dec']/4
#j46=df_K['Dec']/4
#j47=df_K['Dec']/4
#j48=df_K['Dec']/4
#
#j49=df_K['Dec']/4
#j50=df_K['Dec']/4
#j51=df_K['Dec']/4
#j52=df_K['Dec']/4
#j53=df_K['Dec']/4
#
#
#k1=df_P['Jan']/4
#k2=df_P['Jan']/4
#k3=df_P['Jan']/4
#k4=df_P['Jan']/4
#
#k5=df_P['Feb']/4
#k6=df_P['Feb']/4
#k7=df_P['Feb']/4
#k8=df_P['Feb']/4
#
#k9=df_P['Mar']/4
#k10=df_P['Mar']/4
#k11=df_P['Mar']/4
#k12=df_P['Mar']/4
#
#k13=df_P['Apr']/4
#k14=df_P['Apr']/4
#k15=df_P['Apr']/4
#k16=df_P['Apr']/4
#
#k17=df_P['May']/4
#k18=df_P['May']/4
#k19=df_P['May']/4
#k20=df_P['May']/4
#
#k21=df_P['Jun']/4
#k22=df_P['Jun']/4
#k23=df_P['Jun']/4
#k24=df_P['Jun']/4
#
#k25=df_P['Jul']/4
#k26=df_P['Jul']/4
#k27=df_P['Jul']/4
#k28=df_P['Jul']/4
#
#k29=df_P['Aug']/4
#k30=df_P['Aug']/4
#k31=df_P['Aug']/4
#k32=df_P['Aug']/4
#
#
#k33=df_P['Sep']/4
#k34=df_P['Sep']/4
#k35=df_P['Sep']/4
#k36=df_P['Sep']/4
#
#
#k37=df_P['Oct']/4
#k38=df_P['Oct']/4
#k39=df_P['Oct']/4
#k40=df_P['Oct']/4
#
#k41=df_P['Nov']/4
#k42=df_P['Nov']/4
#k43=df_P['Nov']/4
#k44=df_P['Nov']/4
#
#k45=df_P['Dec']/4
#k46=df_P['Dec']/4
#k47=df_P['Dec']/4
#k48=df_P['Dec']/4
#
#k49=df_P['Dec']/4
#k50=df_P['Dec']/4
#k51=df_P['Dec']/4
#k52=df_P['Dec']/4
#k53=df_P['Dec']/4
#
#allK_w=pd.concat([name_genK,j1,j2,j3,j4,j5,j6,j7,j8,j9,j10,j11,j12,j13,j14,j15,j16,j17,j18,j19,j20,j21,j22,j23,j24,j25,j26,j27,j28,j29,j30,j31,j32,j33,j34,j35,j36,j37,j38,j39,j40,j41,j42,j43,j44,j45,j46,j47,j48,j49,j50,j51,j52,j53], axis=1) 
#allK_w.columns=['Generator Name','Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']
#allK_w['DataTypeName'] = 'KLoadFollowing'
#allK_w['DatatypeID'] = 6
#allK_w['Year'] = 0
#allK_w = allK_w[['Generator Name','DataTypeName','DatatypeID','Year','Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']]
#
#
#allP_w=pd.concat([name_genP,k1,k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,k14,k15,k16,k17,k18,k19,k20,k21,k22,k23,k24,k25,k26,k27,k28,k29,k30,k31,k32,k33,k34,k35,k36,k37,k38,k39,k40,k41,k42,k43,k44,k45,k46,k47,k48,k49,k50,k51,k52,k53], axis=1) 
#allP_w.columns=['Generator Name','Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']
#allP_w['DataTypeName'] = 'Pvalue'
#allP_w['DatatypeID'] = 10
#allP_w['Year'] = 0
#allP_w = allP_w[['Generator Name','DataTypeName','DatatypeID','Year','Week1','Week2','Week3','Week4','Week5','Week6','Week7','Week8','Week9','Week10','Week11','Week12','Week13','Week14','Week15','Week16','Week17','Week18','Week19','Week20','Week21','Week22','Week23','Week24','Week25','Week26','Week27','Week28','Week29','Week30','Week31','Week32','Week33','Week34','Week35','Week36','Week37','Week38','Week39','Week40','Week41','Week42','Week43','Week44','Week45','Week46','Week47','Week48','Week49','Week50','Week51','Week52','Week53']]
#
#
#frames3 = [hydro_monthly_updated_Energy_Min_Max,allK_w,allP_w]
#hydro_monthly_updated_Energy_Min_Max_KP= pd.concat(frames3)












