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
import os

import matplotlib.pyplot as plt


df_hydro_monthly = pd.read_csv('HydroMonthlyVarSchedule.csv')
df_name = pd.read_csv('gridview_to_EIA_ID_v2.csv')

# Inputs from https://zenodo.org/records/8408246
# Select the specific year to generate data from B1_Data/B1_MONTHLY
# Replace the csv file here
df_2001 = pd.read_csv('../B1_monthly/B1_monthly_2001.csv')

year = 2023
############# Weighting factors for duplicates####################################################

########## TEPPC energy#################################################################################
################# Isolate TEPCC Energy#############################################
df_hydro_monthly_OnlyE = df_hydro_monthly.loc[(df_hydro_monthly['DataTypeName'] == 'MonthlyEnergy'), [
    'Generator Name', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]
df_hydro_monthly_OnlyE.set_index('Generator Name', inplace=True)
df_hydro_monthly_def = df_hydro_monthly_OnlyE.reset_index()

################ Filter to GridView Name to EAD id################################
df_hydro_monthly_filter = df_hydro_monthly_def.merge(df_name, how='inner', indicator=False).sort_values(by=['EIA_ID'])
df_hydro_monthly_filter_fi = df_hydro_monthly_filter.sort_values(by=['EIA_ID'])
################ Sum the Energy of all duplicate units############################
df_hydro_monthly_sum = df_hydro_monthly_filter.groupby('EIA_ID').agg('sum')
df_hydro_monthly_sum_fi = df_hydro_monthly_sum.reset_index()
# df_hydro_monthly_sum_fi=df_hydro_monthly_sum_in.drop('BusID', axis=1)
############### Dissagregate back to all plants##################################
df_hydro_monthly_dis_sum = df_hydro_monthly_sum_fi.merge(df_name, how='inner', indicator=False)
############### Computes allocation factors########################################
df_hydro_monthly_filter_fi[['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']] = df_hydro_monthly_filter_fi[['Jan', 'Feb', 'Mar', 'Apr', 'May',
                                                                                                                                               'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']].div(df_hydro_monthly_dis_sum[['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']].values, axis=0)
############## The code below fixes the nan values appeared in some cells#########
dupli_mean = df_hydro_monthly_filter_fi.pivot_table(index=['EIA_ID'], aggfunc='size')
dupli_frame_mean = dupli_mean.to_frame().reset_index()
dupli_frame_mean.columns = ['EIA_ID', 'Dupli']
dupli_frame_mean['mean'] = 1/dupli_frame_mean['Dupli']

df_mean_with_dupli = df_hydro_monthly_filter_fi.merge(dupli_frame_mean, how='inner', indicator=False)

df_mean_with_dupli.loc[df_mean_with_dupli['Jan'].isnull(), 'Jan'] = df_mean_with_dupli['mean']
df_mean_with_dupli.loc[df_mean_with_dupli['Feb'].isnull(), 'Feb'] = df_mean_with_dupli['mean']
df_mean_with_dupli.loc[df_mean_with_dupli['Mar'].isnull(), 'Mar'] = df_mean_with_dupli['mean']
df_mean_with_dupli.loc[df_mean_with_dupli['Apr'].isnull(), 'Apr'] = df_mean_with_dupli['mean']
df_mean_with_dupli.loc[df_mean_with_dupli['May'].isnull(), 'May'] = df_mean_with_dupli['mean']
df_mean_with_dupli.loc[df_mean_with_dupli['Jun'].isnull(), 'Jun'] = df_mean_with_dupli['mean']
df_mean_with_dupli.loc[df_mean_with_dupli['Jul'].isnull(), 'Jul'] = df_mean_with_dupli['mean']
df_mean_with_dupli.loc[df_mean_with_dupli['Aug'].isnull(), 'Aug'] = df_mean_with_dupli['mean']
df_mean_with_dupli.loc[df_mean_with_dupli['Sep'].isnull(), 'Sep'] = df_mean_with_dupli['mean']
df_mean_with_dupli.loc[df_mean_with_dupli['Oct'].isnull(), 'Oct'] = df_mean_with_dupli['mean']
df_mean_with_dupli.loc[df_mean_with_dupli['Nov'].isnull(), 'Nov'] = df_mean_with_dupli['mean']
df_mean_with_dupli.loc[df_mean_with_dupli['Dec'].isnull(), 'Dec'] = df_mean_with_dupli['mean']

df_weight_mean = df_mean_with_dupli


########## TEPCC Pmin#####################################################################
df_hydro_monthly_Only_Min = df_hydro_monthly.loc[(df_hydro_monthly['DataTypeName'] == 'MinGen'), [
    'Generator Name', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]
df_hydro_monthly_Only_Min.set_index('Generator Name', inplace=True)
hydro_month_Final_Min = df_hydro_monthly_Only_Min.reset_index()
df_hydro_monthly_filter_Min = hydro_month_Final_Min.merge(
    df_name, how='inner', indicator=False).sort_values(by=['EIA_ID'])
df_hydro_monthly_filter_Min_fi = df_hydro_monthly_filter_Min.sort_values(by=['EIA_ID'])
df_hydro_monthly_Min_sum = df_hydro_monthly_filter_Min.groupby('EIA_ID').agg('sum')

df_hydro_monthly_Min_sum_fi = df_hydro_monthly_Min_sum.reset_index()
# df_hydro_monthly_Min_sum_fi=df_hydro_monthly_Min_sum_in.drop('BusID', axis=1)

df_hydro_monthly_Min_dis_sum = df_hydro_monthly_Min_sum_fi.merge(df_name, how='inner', indicator=False)
df_hydro_monthly_filter_Min_fi[['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']] = df_hydro_monthly_filter_Min_fi[['Jan', 'Feb', 'Mar', 'Apr', 'May',
                                                                                                                                                       'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']].div(df_hydro_monthly_Min_dis_sum[['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']].values, axis=0)

df_hydro_monthly_filter_Min_fi.pivot_table(index=['EIA_ID'], aggfunc='size')

dupli_min = df_hydro_monthly_filter_Min_fi.pivot_table(index=['EIA_ID'], aggfunc='size')
dupli_frame_min = dupli_min.to_frame().reset_index()
dupli_frame_min.columns = ['EIA_ID', 'Dupli']
dupli_frame_min['mean'] = 1/dupli_frame_min['Dupli']

df_min_with_dupli = df_hydro_monthly_filter_Min_fi.merge(dupli_frame_min, how='inner', indicator=False)

df_min_with_dupli.loc[df_min_with_dupli['Jan'].isnull(), 'Jan'] = df_min_with_dupli['mean']
df_min_with_dupli.loc[df_min_with_dupli['Feb'].isnull(), 'Feb'] = df_min_with_dupli['mean']
df_min_with_dupli.loc[df_min_with_dupli['Mar'].isnull(), 'Mar'] = df_min_with_dupli['mean']
df_min_with_dupli.loc[df_min_with_dupli['Apr'].isnull(), 'Apr'] = df_min_with_dupli['mean']
df_min_with_dupli.loc[df_min_with_dupli['May'].isnull(), 'May'] = df_min_with_dupli['mean']
df_min_with_dupli.loc[df_min_with_dupli['Jun'].isnull(), 'Jun'] = df_min_with_dupli['mean']
df_min_with_dupli.loc[df_min_with_dupli['Jul'].isnull(), 'Jul'] = df_min_with_dupli['mean']
df_min_with_dupli.loc[df_min_with_dupli['Aug'].isnull(), 'Aug'] = df_min_with_dupli['mean']
df_min_with_dupli.loc[df_min_with_dupli['Sep'].isnull(), 'Sep'] = df_min_with_dupli['mean']
df_min_with_dupli.loc[df_min_with_dupli['Oct'].isnull(), 'Oct'] = df_min_with_dupli['mean']
df_min_with_dupli.loc[df_min_with_dupli['Nov'].isnull(), 'Nov'] = df_min_with_dupli['mean']
df_min_with_dupli.loc[df_min_with_dupli['Dec'].isnull(), 'Dec'] = df_min_with_dupli['mean']


df_weight_min = df_min_with_dupli


########## TEPPC Pmax########################################################################
df_hydro_monthly_Only_Max = df_hydro_monthly.loc[(df_hydro_monthly['DataTypeName'] == 'MaxCap'), [
    'Generator Name', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]
df_hydro_monthly_Only_Max.set_index('Generator Name', inplace=True)
hydro_month_Final_Max = df_hydro_monthly_Only_Max.reset_index()
df_hydro_monthly_filter_Max = hydro_month_Final_Max.merge(
    df_name, how='inner', indicator=False).sort_values(by=['EIA_ID'])
df_hydro_monthly_filter_Max_fi = df_hydro_monthly_filter_Max.sort_values(by=['EIA_ID'])
df_hydro_monthly_Max_sum = df_hydro_monthly_filter_Max_fi.groupby('EIA_ID').agg('sum')

df_hydro_monthly_Max_sum_fi = df_hydro_monthly_Max_sum.reset_index()
# df_hydro_monthly_Max_sum_fi=df_hydro_monthly_Max_sum_in.drop('BusID', axis=1)

df_hydro_monthly_Max_dis_sum = df_hydro_monthly_Max_sum_fi.merge(df_name, how='inner', indicator=False)
df_hydro_monthly_filter_Max_fi[['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']] = df_hydro_monthly_filter_Max_fi[['Jan', 'Feb', 'Mar', 'Apr', 'May',
                                                                                                                                                       'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']].div(df_hydro_monthly_Max_dis_sum[['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']].values, axis=0)


dupli_max = df_hydro_monthly_filter_Max_fi.pivot_table(index=['EIA_ID'], aggfunc='size')
dupli_frame_max = dupli_max.to_frame().reset_index()
dupli_frame_max.columns = ['EIA_ID', 'Dupli']
dupli_frame_max['mean'] = 1/dupli_frame_max['Dupli']

df_max_with_dupli = df_hydro_monthly_filter_Max_fi.merge(dupli_frame_max, how='inner', indicator=False)

df_max_with_dupli.loc[df_max_with_dupli['Jan'].isnull(), 'Jan'] = df_max_with_dupli['mean']
df_max_with_dupli.loc[df_max_with_dupli['Feb'].isnull(), 'Feb'] = df_max_with_dupli['mean']
df_max_with_dupli.loc[df_max_with_dupli['Mar'].isnull(), 'Mar'] = df_max_with_dupli['mean']
df_max_with_dupli.loc[df_max_with_dupli['Apr'].isnull(), 'Apr'] = df_max_with_dupli['mean']
df_max_with_dupli.loc[df_max_with_dupli['May'].isnull(), 'May'] = df_max_with_dupli['mean']
df_max_with_dupli.loc[df_max_with_dupli['Jun'].isnull(), 'Jun'] = df_max_with_dupli['mean']
df_max_with_dupli.loc[df_max_with_dupli['Jul'].isnull(), 'Jul'] = df_max_with_dupli['mean']
df_max_with_dupli.loc[df_max_with_dupli['Aug'].isnull(), 'Aug'] = df_max_with_dupli['mean']
df_max_with_dupli.loc[df_max_with_dupli['Sep'].isnull(), 'Sep'] = df_max_with_dupli['mean']
df_max_with_dupli.loc[df_max_with_dupli['Oct'].isnull(), 'Oct'] = df_max_with_dupli['mean']
df_max_with_dupli.loc[df_max_with_dupli['Nov'].isnull(), 'Nov'] = df_max_with_dupli['mean']
df_max_with_dupli.loc[df_max_with_dupli['Dec'].isnull(), 'Dec'] = df_max_with_dupli['mean']


df_weight_max = df_max_with_dupli


########## TEPPC avg########################################################################
df_hydro_monthly_Only_avg = df_hydro_monthly.loc[(df_hydro_monthly['DataTypeName'] == 'DailyOpRange'), [
    'Generator Name', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]
df_hydro_monthly_Only_avg.set_index('Generator Name', inplace=True)
hydro_month_Final_avg = df_hydro_monthly_Only_avg.reset_index()
df_hydro_monthly_filter_avg = hydro_month_Final_avg.merge(
    df_name, how='inner', indicator=False).sort_values(by=['EIA_ID'])
df_hydro_monthly_filter_avg_fi = df_hydro_monthly_filter_avg.sort_values(by=['EIA_ID'])
df_hydro_monthly_avg_sum = df_hydro_monthly_filter_avg_fi.groupby('EIA_ID').agg('sum')

df_hydro_monthly_avg_sum_fi = df_hydro_monthly_avg_sum.reset_index()
# df_hydro_monthly_Max_sum_fi=df_hydro_monthly_Max_sum_in.drop('BusID', axis=1)

df_hydro_monthly_avg_dis_sum = df_hydro_monthly_avg_sum_fi.merge(df_name, how='inner', indicator=False)
df_hydro_monthly_filter_avg_fi[['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']] = df_hydro_monthly_filter_avg_fi[['Jan', 'Feb', 'Mar', 'Apr', 'May',
                                                                                                                                                       'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']].div(df_hydro_monthly_avg_dis_sum[['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']].values, axis=0)


dupli_avg = df_hydro_monthly_filter_avg_fi.pivot_table(index=['EIA_ID'], aggfunc='size')
dupli_frame_avg = dupli_avg.to_frame().reset_index()
dupli_frame_avg.columns = ['EIA_ID', 'Dupli']
dupli_frame_avg['mean'] = 1/dupli_frame_avg['Dupli']

df_avg_with_dupli = df_hydro_monthly_filter_avg_fi.merge(dupli_frame_avg, how='inner', indicator=False)

df_avg_with_dupli.loc[df_avg_with_dupli['Jan'].isnull(), 'Jan'] = df_avg_with_dupli['mean']
df_avg_with_dupli.loc[df_avg_with_dupli['Feb'].isnull(), 'Feb'] = df_avg_with_dupli['mean']
df_avg_with_dupli.loc[df_avg_with_dupli['Mar'].isnull(), 'Mar'] = df_avg_with_dupli['mean']
df_avg_with_dupli.loc[df_avg_with_dupli['Apr'].isnull(), 'Apr'] = df_avg_with_dupli['mean']
df_avg_with_dupli.loc[df_avg_with_dupli['May'].isnull(), 'May'] = df_avg_with_dupli['mean']
df_avg_with_dupli.loc[df_avg_with_dupli['Jun'].isnull(), 'Jun'] = df_avg_with_dupli['mean']
df_avg_with_dupli.loc[df_avg_with_dupli['Jul'].isnull(), 'Jul'] = df_avg_with_dupli['mean']
df_avg_with_dupli.loc[df_avg_with_dupli['Aug'].isnull(), 'Aug'] = df_avg_with_dupli['mean']
df_avg_with_dupli.loc[df_avg_with_dupli['Sep'].isnull(), 'Sep'] = df_avg_with_dupli['mean']
df_avg_with_dupli.loc[df_avg_with_dupli['Oct'].isnull(), 'Oct'] = df_avg_with_dupli['mean']
df_avg_with_dupli.loc[df_avg_with_dupli['Nov'].isnull(), 'Nov'] = df_avg_with_dupli['mean']
df_avg_with_dupli.loc[df_avg_with_dupli['Dec'].isnull(), 'Dec'] = df_avg_with_dupli['mean']


df_weight_avg = df_avg_with_dupli


################### PNNL Energy Dataset################################################
####### Computes montly energy budget################
df_2001_1 = df_2001.pivot_table(index='EIA_ID', columns='month', values='target_MWh').reset_index()
df_2001_1_EIA = df_2001_1['EIA_ID']
df_2001_1_Jan = df_2001_1['Jan']
df_2001_1_Feb = df_2001_1['Feb']
df_2001_1_March = df_2001_1['Mar']
df_2001_1_April = df_2001_1['Apr']
df_2001_1_May = df_2001_1['May']
df_2001_1_June = df_2001_1['Jun']
df_2001_1_July = df_2001_1['Jul']
df_2001_1_August = df_2001_1['Aug']
df_2001_1_Sep = df_2001_1['Sep']
df_2001_1_Oct = df_2001_1['Oct']
df_2001_1_Nov = df_2001_1['Nov']
df_2001_1_Dec = df_2001_1['Dec']
df_mean_order = pd.concat(
    [df_2001_1_EIA, df_2001_1_Jan, df_2001_1_Feb, df_2001_1_March, df_2001_1_April, df_2001_1_May, df_2001_1_June,
     df_2001_1_July, df_2001_1_August, df_2001_1_Sep, df_2001_1_Oct, df_2001_1_Nov, df_2001_1_Dec],
    axis=1, sort=False)
df_mean_order['DataTypeName'] = 'MonthlyEnergy'


############ dissaggregates PNNL plants energy budget to units##############################
df_nam_filtered = df_hydro_monthly_dis_sum[['EIA_ID', 'Generator Name']]


df_mean_order_name = df_mean_order.merge(df_nam_filtered, how='inner', indicator=False)
df_mean_order_filter = df_mean_order_name[['Generator Name', 'DataTypeName']]


############ Associate PNNL plants to allocation values##############################
df_weight_mean_filter = df_weight_mean.merge(df_mean_order_filter, how='inner', indicator=False)
############ Multiplies allocation values with PNNL energy budget ##############################
df_mean_order_name[['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']] = df_mean_order_name[['Jan', 'Feb', 'Mar', 'Apr', 'May',
                                                                                                                               'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]*df_weight_mean_filter[['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]


# -------------

############# Updates TEPPC Database with PNNL energy budget ###############
df_hydro_monthly_OnlyE.update(df_mean_order_name.set_index('Generator Name'))
hydro_month_Final = df_hydro_monthly_OnlyE.reset_index()
hydro_month_Final['DataTypeName'] = 'MonthlyEnergy'
hydro_month_Final['DatatypeID'] = 5
hydro_month_Final['Year'] = 0
hydro_month_Final = hydro_month_Final[['Generator Name', 'DataTypeName', 'DatatypeID',
                                       'Year', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]
############# Drops Energy budget from TEPPC components###############
hydro_monthly_noEn = df_hydro_monthly[df_hydro_monthly.DataTypeName != 'MonthlyEnergy']
############# Concacts Back Energy ##########################
frames = [hydro_monthly_noEn, hydro_month_Final]
hydro_monthly_updated_Energy = pd.concat(frames)


############# PNNL Pminimum_dataset#################################
df_2001_min = df_2001.pivot_table(index='EIA_ID', columns='month', values='p_min').reset_index()
df_2001_min['DataTypeName'] = 'MinGen'
df_min_2001_order = df_2001_min[['DataTypeName', 'EIA_ID', 'Jan', 'Feb',
                                 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]
df_2001_min_order_F = df_min_2001_order.merge(df_nam_filtered, how='inner', indicator=False)
df_min_order_filter = df_2001_min_order_F[['Generator Name', 'DataTypeName']]
df_weight_min_filter = df_weight_min.merge(df_min_order_filter, how='inner', indicator=False)
############ Multiplies allocation values with PNNL energy budget ##############################
df_2001_min_order_F[['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']] = df_2001_min_order_F[['Jan', 'Feb', 'Mar', 'Apr', 'May',
                                                                                                                                 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]*df_weight_min_filter[['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]
############# All Pmin_component_Updated###############
df_hydro_monthly_Only_Min.update(df_2001_min_order_F.set_index('Generator Name'))
hydro_month_Final_Min = df_hydro_monthly_Only_Min.reset_index()
hydro_month_Final_Min['DataTypeName'] = 'MinGen'
hydro_month_Final_Min['DatatypeID'] = 3
hydro_month_Final_Min['Year'] = 0
hydro_month_Final_Min = hydro_month_Final_Min[['Generator Name', 'DataTypeName', 'DatatypeID',
                                               'Year', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]
# dftest=df_test2.merge(df_name_only, how = 'inner' ,indicator=False)
# dftest.to_csv("Pmin_2009_default.csv", index=False)
############# All_Non_Min_component#######################
hydro_monthly_noMin = hydro_monthly_updated_Energy[hydro_monthly_updated_Energy.DataTypeName != 'MinGen']
############# Concact Back Energy and Pmin##########################
frames1 = [hydro_monthly_noMin, hydro_month_Final_Min]
hydro_monthly_updated_Energy_Min = pd.concat(frames1)


############# PNNL Pmaximum Dataset###############################
df_2001_max = df_2001.pivot_table(index='EIA_ID', columns='month', values='p_max').reset_index()
df_2001_max['DataTypeName'] = 'MaxCap'
df_max_2001_order = df_2001_max[['DataTypeName', 'EIA_ID', 'Jan', 'Feb',
                                 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]
df_2001_max_order_F = df_max_2001_order.merge(df_nam_filtered, how='inner', indicator=False)
df_max_order_filter = df_2001_max_order_F[['Generator Name', 'DataTypeName']]
df_weight_max_filter = df_weight_max.merge(df_max_order_filter, how='inner', indicator=False)
############ Multiplies allocation values with PNNL energy budget ##############################
df_2001_max_order_F[['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']] = df_2001_max_order_F[['Jan', 'Feb', 'Mar', 'Apr', 'May',
                                                                                                                                 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]*df_weight_max_filter[['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]
############# All Pmax_component_Updated###############
df_hydro_monthly_Only_Max.update(df_2001_max_order_F.set_index('Generator Name'))
hydro_month_Final_Max = df_hydro_monthly_Only_Max.reset_index()
hydro_month_Final_Max['DataTypeName'] = 'MaxCap'
hydro_month_Final_Max['DatatypeID'] = 4
hydro_month_Final_Max['Year'] = 0
hydro_month_Final_Max = hydro_month_Final_Max[['Generator Name', 'DataTypeName', 'DatatypeID',
                                               'Year', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]
############# All_Non_Pmax_component####################
hydro_monthly_noMax = hydro_monthly_updated_Energy_Min[hydro_monthly_updated_Energy_Min.DataTypeName != 'MaxCap']
############# Concact Back All##########################
frames2 = [hydro_monthly_noMax, hydro_month_Final_Max]
hydro_monthly_updated_Energy_Min_Max = pd.concat(frames2)


############# PNNL Avg Dataset###############################
df_2001_avg = df_2001.pivot_table(index='EIA_ID', columns='month', values='ador').reset_index()
df_2001_avg['DataTypeName'] = 'DailyOpRange'
df_avg_2001_order = df_2001_avg[['DataTypeName', 'EIA_ID', 'Jan', 'Feb',
                                 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]
df_2001_avg_order_F = df_avg_2001_order.merge(df_nam_filtered, how='inner', indicator=False)
df_avg_order_filter = df_2001_avg_order_F[['Generator Name', 'DataTypeName']]
df_weight_avg_filter = df_weight_avg.merge(df_avg_order_filter, how='inner', indicator=False)
############ Multiplies allocation values with PNNL energy budget ##############################
df_2001_avg_order_F[['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']] = df_2001_avg_order_F[['Jan', 'Feb', 'Mar', 'Apr', 'May',
                                                                                                                                 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]*df_weight_avg_filter[['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]
############# All Pmax_component_Updated###############
df_hydro_monthly_Only_avg.update(df_2001_avg_order_F.set_index('Generator Name'))
hydro_month_Final_avg = df_hydro_monthly_Only_avg.reset_index()
hydro_month_Final_avg['DataTypeName'] = 'DailyOpRange'
hydro_month_Final_avg['DatatypeID'] = 11
hydro_month_Final_avg['Year'] = 0
hydro_month_Final_avg = hydro_month_Final_avg[['Generator Name', 'DataTypeName', 'DatatypeID',
                                               'Year', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']]
############# All_Non_Pmax_component####################
hydro_monthly_noAvg = hydro_monthly_updated_Energy_Min_Max[hydro_monthly_updated_Energy_Min_Max.DataTypeName !=
                                                           'DailyOpRange']
############# Concact Back All##########################
frames3 = [hydro_monthly_noAvg, hydro_month_Final_avg]
hydro_monthly_updated_Energy_Min_Max_Avg = pd.concat(frames3)


frames_final = [df_mean_order_name, df_2001_min_order_F, df_2001_max_order_F, df_2001_avg_order_F]

hydro_monthly_663 = pd.concat(frames_final)


# #############Reorderl##################################
# hydro_monthly_updated_Energy_Min_Max_Avg['C'] = hydro_monthly_updated_Energy_Min_Max_Avg.groupby('DatatypeID')['DatatypeID'].cumcount()
# hydro_monthly_updated_Energy_Min_Max_Avg.sort_values(by=['C', 'DatatypeID'], inplace=True)
# hydro_monthly_updated_Energy_Min_Max_Avg=hydro_monthly_updated_Energy_Min_Max_Avg.drop(['C'], axis = 1)


############# Export##################################

# folder_path = 'monthly_outputs'
# if not os.path.exists(folder_path):
#     os.makedirs(folder_path)


# hydro_monthly_663.to_csv(os.path.join(folder_path, f'MONTHLY_Outputs_{year}.csv'), index=False)
# df_mean_order_name.to_csv("Pmean_ANL_monthly.csv", index=False)
# df_2001_min_order_F.to_csv("Pmin_ANL_monthly.csv", index=False)
# df_2001_max_order_F.to_csv("Pmax_ANL_monthly.csv", index=False)
# df_2001_avg_order_F.to_csv("Avg_ANL_monthly.csv", index=False)


# df_hydro_monthly_filter_fi.pivot_table(index=['EIA_ID'], aggfunc='size')
# df_weight_min=df_hydro_monthly_filter_Min_fi.fillna(1)
# df_hydro_monthly_filter_Max_fi.pivot_table(index=['EIA_ID'], aggfunc='size')
