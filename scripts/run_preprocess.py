import configparser
import os
import warnings
import pandas as pd
from lung.preprocess import *

pd.options.mode.chained_assignment = None
warnings.filterwarnings('ignore')

CONFIG = configparser.ConfigParser()
CONFIG.read(os.path.join(os.path.dirname(__file__), 'script_config.ini'))
BASE_PATH = CONFIG['file_locations']['base_path']

DATA_RAW = os.path.join(BASE_PATH, 'raw')
DATA_PROCESSED = os.path.join(BASE_PATH, '..', 'results', 'processed')
DATA_RESULTS = os.path.join(BASE_PATH, '..', 'results', 'final')

all_data = os.path.join(DATA_RAW, 'cdc_mort_2005to2022_agesexethsum.csv')
output_path = os.path.join(DATA_PROCESSED, '2005to2022_age_sex_race.csv')

input_data   = os.path.join(DATA_PROCESSED, '2005to2022_age_sex_race.csv')
census_2010  = os.path.join(DATA_RAW, 'census_2010.csv')
census_2017  = os.path.join(DATA_RAW, 'census_2017.csv')
census_2020  = os.path.join(DATA_RAW, 'census_2020.csv')
output_path  = os.path.join(DATA_PROCESSED, 'mortality_with_population.csv')

mort_path = os.path.join(DATA_PROCESSED, 'mortality_with_population.csv')
mort_out = os.path.join(DATA_PROCESSED)

###### AIR QUALITY PREPROCESSING ######
mortality_path = os.path.join(DATA_PROCESSED, 'mortality_by_age.csv')
pm25_2010_path = os.path.join(DATA_RAW, 'counties_pm25_2010.csv')
pm25_2017_path = os.path.join(DATA_RAW, 'counties_pm25_2017.csv')
pm25_2022_path = os.path.join(DATA_RAW, 'counties_pm25_2022.csv') 
mortality_output = os.path.join(DATA_PROCESSED, 'mortality_with_pm25.csv')
 
if __name__ == "__main__":
    '''result = aggregate_by_epoch(data_path   = all_data,
        epochs      = [(2005, 2010), (2011, 2017), (2018, 2022)],
        output_path = output_path)
    attach_population(input_data, census_2010, census_2017, census_2020, output_path)
    aggregate_mortality(mort_path,
        strata = ["sex", "race_recode3", "age_cat"], output_dir = mort_out
    )'''
    attach_pm25(mortality_path, pm25_2010_path, pm25_2017_path, pm25_2022_path, mortality_output)
 