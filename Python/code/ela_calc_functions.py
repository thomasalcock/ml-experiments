# Functions for ela_calc.py
import pandas as pd
import numpy as np
import os
import statsmodels.api as sm

def filter_data(df, is_lease):
    """Selects columns and rows by sales channel"""
    print("Filter data")
    cols = ['type_class', 'month', 'retail_count', 'net_amount_total']
    if is_lease == True:
        dat = df.loc[df['product'] == "Lease"]
        cols.append('std_monthly_payment')
        return dat[cols]
    else:
        dat = df.loc[df['product'] == "No Leasing"]
        cols.append('price')
        return dat[cols]
        
def select_tc(df, tc):
    """Filters data by type class"""
    print("Select typeclass")
    df = df.loc[df['type_class']==tc]
    return df

def normalize(df, is_lease):
    """Normalizes price and sales data by typeclass"""
    print("Normalize data")
    if is_lease:
        price_var = 'std_monthly_payment'
    else:
        price_var = 'price'
    agg_dat = df.groupby('type_class').aggregate(np.mean)
    agg_dat.columns = ['mean_retail_count', 'mean_net_amount_total', 'mean_' + price_var]
    df = df.join(agg_dat, on='type_class', how='left')
    df['net_amount_total'] = df['retail_count'] / df['mean_net_amount_total']
    df['price'] = df[price_var] / df['mean_' + price_var]
    df = df.drop(['mean_retail_count', 'mean_net_amount_total', 'mean_' + price_var], axis = 1)
    return df

def dummy_vars(df):
    """Returns df with dummy vars"""
    print("Get dummies")
    df = df.drop('type_class', axis = 1)
    df = pd.get_dummies(df)
    return df

def pois_model(df, dist_family):
    """Fits poisson model"""
    print("Fit poisson model")
    y = df['retail_count']
    X = df.drop('retail_count', axis = 1)
    X = sm.add_constant(X)
    stat_model = sm.GLM(y, X, family=dist_family)
    model_results = stat_model.fit()
    return model_results

def model_results(results):
    """Retrieves model results"""
    print("Get model results")
    df = pd.DataFrame()
    df['var_name'] = results.params.index
    df['coeff'] = results.params.values
    df['lower'] = results.conf_int()[0].values
    df['upper'] = results.conf_int()[1].values
    return df

def general_data_prep(df, is_lease):
    """Performs general data prep"""
    df = filter_data(df, is_lease)
    df = normalize(df, is_lease)
    unique_tcs = df['type_class'].unique()
    df = df.replace([-np.inf, np.inf], np.nan).dropna()
    return df, unique_tcs

def try_poisson_calc(df_ela, is_lease, typeclass_list):
    sales_type = 'lease' if is_lease else 'sales'

    print("---- Calculation for " + sales_type + " -----")
    for i in range(len(typeclass_list)):
        print("Iteration: " + str(i))
        print("Typeclass: " + typeclass_list[i])
        try:
            df = select_tc(df_ela, typeclass_list[i])
            df = dummy_vars(df)
            model = pois_model(df=df, dist_family=sm.families.Poisson())
            results = model_results(model)
            results['type_class'] = typeclass_list[i]
            results.to_csv("../output/model_results_" + typeclass_list[i] + "_" + sales_type + ".csv")
        except:
            print("Some error with typeclass " + typeclass_list[i])
            continue