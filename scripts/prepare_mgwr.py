import configparser
import os
import pandas as pd
from statsmodels.stats.outliers_influence import variance_inflation_factor

CONFIG = configparser.ConfigParser()
CONFIG.read(os.path.join(os.path.dirname(__file__), 'script_config.ini'))
BASE_PATH = CONFIG['file_locations']['base_path']

DATA_RAW = os.path.join(BASE_PATH, 'raw')
DATA_PROCESSED = os.path.join(BASE_PATH, '..', 'results', 'processed')
DATA_RESULTS = os.path.join(BASE_PATH, '..', 'results', 'final')

INPUT_FILE  = os.path.join(DATA_PROCESSED, 'mort_pm25_health_socio.csv')
OUTPUT_FILE = os.path.join(DATA_PROCESSED, 'mortality_mgwr_ready.csv')

# Model variables for MGWR (after dropping high-VIF columns)
MODEL_VARS = [
    'pm_25',
    'CSMOKING_CrudePrev',
    'OBESITY_CrudePrev',
    'LPA_CrudePrev',
    'ACCESS2_CrudePrev',
    'unemployment_rate',
    'rucc_code',
    'encoded_age',
]

# Columns constant within (fips, epoch) — take first value
META_COLS = [
    'res_statefips', 'res_countyfips', 'fips', 'county_name',
    'latitude', 'longitude', 'population', 'epoch',
] + MODEL_VARS


df = pd.read_csv(INPUT_FILE, low_memory=False)

agg_dict = {'total_mort_count': ('total_mort_count', 'sum')}
agg_dict.update({col: (col, 'first') for col in META_COLS if col in df.columns})

agg = (
    df.groupby(['fips', 'epoch'], as_index=False)
    .agg(**agg_dict)
)

agg['mortality_per_100k'] = (
    agg['total_mort_count'] / agg['population'] * 100000
)

required = ['mortality_per_100k', 'latitude', 'longitude'] + MODEL_VARS
before   = len(agg)
agg      = agg.dropna(subset=required).reset_index(drop=True)


# ── Step 3 — VIF check ────────────────────────────────────────────────────────
print('\nVIF check (values > 7.5 indicate multicollinearity):')
print(f'  {"Variable":<30} {"VIF":>8}')
print(f'  {"-"*38}')

X      = agg[MODEL_VARS].values.astype(float)
issues = []
for i, col in enumerate(MODEL_VARS):
    vif = variance_inflation_factor(X, i)
    flag = '  ⚠  HIGH' if vif > 7.5 else ''
    print(f'  {col:<30} {vif:>8.2f}{flag}')
    if vif > 7.5:
        issues.append(col)

final_cols = [
    'fips', 'county_name', 'res_statefips', 'res_countyfips',
    'latitude', 'longitude', 'epoch', 'population',
    'total_mort_count', 'mortality_per_100k',
] + MODEL_VARS

final_cols = [c for c in final_cols if c in agg.columns]
agg        = agg[final_cols].sort_values(['fips', 'epoch']).reset_index(drop=True)

agg.to_csv(OUTPUT_FILE, index=False)
size_mb = os.path.getsize(OUTPUT_FILE) / 1024 / 1024
print(f'\n✓ Saved -> {OUTPUT_FILE}  ({size_mb:.2f} MB)')