# import libraries
import pandas as pd
import numpy as np
import os
import statsmodels.api as sm
import importlib
from ela_calc_functions import filter_data, select_tc, normalize, dummy_vars, pois_model, model_results, general_data_prep, try_poisson_calc

# read data
ela_data = pd.read_csv("../data/ela_data.csv")

# sales
df_ela_sales, unique_tcs_sales = general_data_prep(ela_data, False)

try_poisson_calc(df_ela = df_ela_sales, 
                 is_lease = False, 
                 typeclass_list = unique_tcs_sales)


# leases
df_ela_lease, unique_tcs_lease = general_data_prep(ela_data, True)

try_poisson_calc(df_ela = df_ela_lease, 
                 is_lease = False, 
                 typeclass_list = unique_tcs_lease)
