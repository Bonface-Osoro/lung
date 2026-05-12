import io
import os
import requests
import zipfile
import geopandas as gpd
import pandas as pd
from pathlib import Path
 
def aggregate_by_epoch(data_path, epochs,
    output_path=None):
    """
    This function aggregates CDC mortality data by epoch.
 
    Parameters
    ----------
    data_path : str
        Path to the input CSV file.
    epochs : list of (start_year, end_year) tuples
        Each tuple defines an inclusive year range, e.g.
        [(2005, 2010), (2011, 2017), (2018, 2022)].
        Epochs must be non-overlapping and in ascending order.
    output_path : str, optional
        If provided, the aggregated DataFrame is saved to this CSV path.
 
    Returns
    -------
    pd.DataFrame
        Aggregated DataFrame with columns:
        res_statefips, res_countyfips, sex, race_recode3,
        age_cat, epoch, total_mort_count
    """
    #Validate epochs
    if not epochs:

        raise ValueError('epochs must contain at least one (start, end) tuple.')
 
    for i, (start, end) in enumerate(epochs):

        if start > end:

            raise ValueError(f"Epoch {i}: start ({start}) must be <= end ({end}).")
        if i > 0 and start <= epochs[i - 1][1]:

            raise ValueError(
                f"Epoch {i} starts at {start}, which overlaps with the previous "
                f"epoch ending at {epochs[i - 1][1]}. Epochs must be non-overlapping."
            )
 
    # Build bins and labels from epochs
    bins   = [epochs[0][0] - 1] + [end for _, end in epochs]
    labels = [f"{start}-{end}" for start, end in epochs]
 
    print(f'Reading {data_path} ...')
    df = pd.read_csv(data_path)
    print(f"  {len(df):,} rows | years: {df['fileyear'].min()}–{df['fileyear'].max()}")
 
    df["epoch"] = pd.cut(df["fileyear"], bins = bins,
        labels = labels, right = True)
 
    dropped = df["epoch"].isna().sum()
    if dropped:

        print(f"{dropped:,} rows outside epoch range dropped.")
        df = df.dropna(subset=["epoch"])

    group_cols = ['res_statefips', 'res_countyfips',
        'sex', 'race_recode3', 'age_cat', 'epoch']
 
    result = (df.groupby(group_cols, observed=True)["mort_count"]
        .sum().reset_index()
        .rename(columns={"mort_count": "total_mort_count"}))
 
    result["epoch"] = pd.Categorical(result["epoch"], categories=labels, ordered=True)
    result = result.sort_values(group_cols).reset_index(drop=True)
 
    print(f"  {len(result):,} aggregated rows across {result['epoch'].nunique()} epoch(s).")

    if output_path:
        result.to_csv(output_path, index=False)
        print(f"  Saved → {output_path}")
 
    return result


EPOCH_MAP = {
    "2005-2010": "pop_2010",
    "2011-2017": "pop_2017",
    "2018-2022": "pop_2020",
}
 
def get_county_centroids():
    """
    This is a helper function to compute county 
    centroids for all US counties using the Census TIGER shapefile.
    """
    url = 'https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_county_500k.zip'
    print('\nDownloading Census county boundaries for centroid calculation ...')
    r = requests.get(url, timeout=120)
    r.raise_for_status()
 
    gdf = gpd.read_file(io.BytesIO(r.content))
 
    gdf = gdf.to_crs('EPSG:5070')         
    gdf['geometry'] = gdf.geometry.centroid
    gdf = gdf.to_crs('EPSG:4326')          
 
    gdf['fips_key'] = gdf['GEOID'].str.zfill(5)
    gdf['latitude']  = gdf.geometry.y.round(6)
    gdf['longitude'] = gdf.geometry.x.round(6)
 
    print(f'  Centroids computed for {len(gdf):,} counties')
    return gdf[['fips_key', 'latitude', 'longitude']]
 

def attach_population(input_data, census_2010,
    census_2017, census_2020, output_path):
    """
    Join epoch-appropriate census population onto a mortality dataset.
 
    Parameters
    ----------
    input_data  : str
        path to the aggregated mortality CSV
                  (must contain: res_statefips, res_countyfips, epoch)
    census_2010 : str
        path to 2010 census CSV  (must contain: state_abbr, county_fips, pop_2010)
    census_2017 : str
        path to 2017 census CSV  (must contain: state_abbr, county_fips, pop_2017)
    census_2020 : str
        path to 2020 census CSV  (must contain: state_abbr, county_fips, pop_2020)
    output_path : str | None
        optional CSV path to save the result
 
    Returns
    -------
    pd.DataFrame with all original columns plus a `population` column.
    Rows where no census match was found will have NaN in `population`.
    """
    mort = pd.read_csv(input_data)
    c10  = pd.read_csv(census_2010)
    c17  = pd.read_csv(census_2017)
    c20  = pd.read_csv(census_2020)
 
    required_mort   = {'res_statefips', 'res_countyfips', 'epoch'}
    required_census = {'state_abbr', 'county_fips', 'fips', 'county_name'}
 
    for label, df, req in [
        ('input_data',  mort, required_mort),
        ('census_2010', c10,  required_census | {'pop_2010'}),
        ('census_2017', c17,  required_census | {'pop_2017'}),
        ('census_2020', c20,  required_census | {'pop_2020'}),
    ]:
        missing = req - set(df.columns)
        if missing:
            raise ValueError(f"[{label}] missing columns: {missing}")

    # Keep only the columns needed for joining from each census file,
    # then merge all three on state_abbr + county_fips.
    lookup = (
        c10[['state_abbr', 'county_fips', 'pop_2010', 'fips', 'county_name']]
        .merge(c17[['state_abbr', 'county_fips', 'pop_2017', 'fips', 'county_name']], 
               on = ['state_abbr', 'county_fips'], how = 'outer')
        .merge(c20[['state_abbr', 'county_fips', 'pop_2020', 'fips', 'county_name']], 
               on = ['state_abbr', 'county_fips'], how='outer')
    )

    # mortality : res_statefips = str abbreviation, res_countyfips = int
    # census    : state_abbr    = str abbreviation, county_fips    = int
    mort['res_statefips']  = mort['res_statefips'].astype(str).str.strip().str.upper()
    mort['res_countyfips'] = mort['res_countyfips'].astype(int)
    lookup['state_abbr']   = lookup['state_abbr'].astype(str).str.strip().str.upper()
    lookup['county_fips']  = lookup['county_fips'].astype(int)

    merged = mort.merge(lookup, left_on  = ['res_statefips', 'res_countyfips'],
        right_on = ['state_abbr',    'county_fips'], how = 'left')
 
    def pick_population(row):

        pop_col = EPOCH_MAP.get(row["epoch"])
        if pop_col is None:

            return float("nan")
        return row[pop_col]
 
    merged["population"] = merged.apply(pick_population, axis=1)
 
    result = merged.drop(columns = ['state_abbr', 'county_fips', 'fips_x', 'county_name_x',
                                    'fips_y', 'county_name_y', 'pop_2010', 'pop_2017', 'pop_2020'])  
    if output_path:
        result.to_csv(output_path, index=False)
        size_mb = os.path.getsize(output_path) / 1024 / 1024
 
    return result


def aggregate_mortality(input_path, strata, output_dir):
    """
    Aggregate total_mort_count by each requested stratification variable.
 
    Parameters
    ----------
    input_path : str
        path to mortality_with_population CSV
    strata     : list
        list of strata to compute, subset of
                 ["sex", "race_recode3", "age_cat"].
                 Defaults to all three.
    output_dir : str
        directory for saved CSVs.
        Defaults to the same folder as input_path.
 
    Returns
    -------
    dict mapping stratum name → aggregated DataFrame
    """
    GEO_COLS   = ['res_statefips', 'res_countyfips']
    META_COLS  = ['fips', 'county_name', 'population']  
    EPOCH_COL  = 'epoch'
    COUNT_COL  = 'total_mort_count'
    
    STRATA_CONFIG = {
        'sex': {'col': 'sex', 'output': 'mortality_by_sex.csv'},
        'race_recode3': {'col': 'race_recode3', 'output': 'mortality_by_race.csv'},
        'age_cat': {'col': 'age_cat', 'output': 'mortality_by_age.csv'}}
    
    strata     = strata     or list(STRATA_CONFIG.keys())
    output_dir = output_dir or os.path.dirname(os.path.abspath(input_path))
 
    invalid = set(strata) - set(STRATA_CONFIG)
    if invalid:

        raise ValueError(
            f"Unknown strata: {invalid}. "
            f"Valid options: {list(STRATA_CONFIG.keys())}"
        )
 
    df = pd.read_csv(input_path, low_memory=False)
 
    results = {}
 
    for stratum in strata:

        cfg       = STRATA_CONFIG[stratum]
        stratum_col = cfg["col"]
        out_file    = os.path.join(output_dir, cfg["output"])

        group_cols = GEO_COLS + [stratum_col, EPOCH_COL]
 
        agg = (df.groupby(group_cols, dropna=False)[COUNT_COL]
            .sum()
            .reset_index())
 
        meta = (
            df.groupby(GEO_COLS + [EPOCH_COL], dropna=False)[META_COLS]
            .first()
            .reset_index())
 
        agg = agg.merge(meta, on=GEO_COLS + [EPOCH_COL], how="left")
 
        final_cols = (GEO_COLS
            + [stratum_col, EPOCH_COL]
            + META_COLS
            + [COUNT_COL])
        agg = agg[final_cols]
 
        agg.to_csv(out_file, index=False)
        size_kb = os.path.getsize(out_file) / 1024
        print(f"\n  [{stratum}] {len(agg):,} rows → {out_file}  ({size_kb:.0f} KB)")
 
        results[stratum] = agg

    return results


EPOCH_MAP = {
    "2005-2010": "pm25_2010",
    "2011-2017": "pm25_2017",
    "2018-2022": "pm25_2022",
}
 
 
def _normalise_fips(series):
    """
    This is a helper function to convert any FIPS 
    representation to a zero-padded 5-character string.
      2020.0  → "02020"
      1001    → "01001"
      "1001"  → "01001"
      "01001" → "01001"
    """
    return (
        series
        .fillna(-1)
        .astype(float)
        .astype(int)
        .astype(str)
        .str.zfill(5)
    )

def attach_pm25(mortality_path, pm25_2010_path,
    pm25_2017_path, pm25_2022_path, output_path):
    """"
    This function is designed to join epoch-appropriate 
    PM2.5 values onto a mortality dataset.

    Parameters
    ----------  
    mortality_path  : str
        Path to the input mortality CSV file. Must contain 
        columns: 'fips' and 'epoch'.
    pm25_2010_path  : str
        Path to the PM2.5 CSV file for 2010. Must contain 
        columns: 'STCOFIPS' and 'pm_25'.
    pm25_2017_path  : str   
        Path to the PM2.5 CSV file for 2017. Must contain 
        columns: 'STCOFIPS' and 'pm_25'.
    pm25_2022_path  : str   
        Path to the PM2.5 CSV file for 2022. Must contain 
        columns: 'STCOFIPS' and 'pm_25'.
    output_path     : str or None
        Optional path to save the resulting DataFrame as 
        a CSV file. If None, the result is not saved.

    Returns
    -------
    pd.DataFrame
        The input mortality DataFrame with an additional 
        'pm_25' column containing the joined PM2.5 values.
        Rows where no PM2.5 match was found will have NaN in 'pm_25'.
    """
    EPOCH_MAP = {
        '2005-2010': 'pm25_2010',
        '2011-2017': 'pm25_2017',
        '2018-2022': 'pm25_2022',
    }
    mort = pd.read_csv(mortality_path, encoding ='latin-1', 
                       low_memory=False)
    pm25_raw = {
        'pm25_2010': pd.read_csv(pm25_2010_path, encoding = 'latin-1', 
                                 low_memory = False),
        'pm25_2017': pd.read_csv(pm25_2017_path, encoding = 'latin-1', 
                                 low_memory = False),
        'pm25_2022': pd.read_csv(pm25_2022_path, encoding = 'latin-1', 
                                 low_memory = False),
    }
 
    for k, df in pm25_raw.items():

        print(f'  {k} rows       : {len(df):,}')
 
    if 'fips' not in mort.columns:

        raise ValueError("mortality file must contain a 'fips' column")
    
    if 'epoch' not in mort.columns:

        raise ValueError("mortality file must contain an 'epoch' column")
    
    for k, df in pm25_raw.items():

        if 'STCOFIPS' not in df.columns:

            raise ValueError(f'{k} must contain a STCOFIPS column')
        
        if 'pm_25' not in df.columns:

            raise ValueError(f'{k} must contain a pm_25 column')
 
    pm25_lookup = {}
    for key, df in pm25_raw.items():

        df = df.copy()
        df['fips_key'] = _normalise_fips(df['STCOFIPS'])
        lookup = (df[['fips_key', 'pm_25']]
            .drop_duplicates('fips_key')
            .set_index('fips_key')['pm_25'])
        
        pm25_lookup[key] = lookup
 
    mort = mort.copy()
    mort['fips_key'] = _normalise_fips(mort['fips'])

 
    def lookup_pm25(row):

        dataset_key = EPOCH_MAP.get(row['epoch'])
        if dataset_key is None:

            return float('nan')
        
        return pm25_lookup[dataset_key].get(row['fips_key'], float('nan'))
 
    mort['pm_25'] = mort.apply(lookup_pm25, axis = 1)
    mort = mort.drop(columns=['fips_key'])
 
    n_matched   = mort['pm_25'].notna().sum()
    n_unmatched = mort['pm_25'].isna().sum()

    if n_unmatched:

        unmatched = (
            mort[mort['pm_25'].isna()][['res_statefips', 
                                        'res_countyfips', 'epoch']]
            .drop_duplicates()
        )

    centroids = get_county_centroids()
    mort['fips_key'] = _normalise_fips(mort['fips'])
    mort = mort.merge(centroids, on='fips_key', how='left')
    mort = mort.drop(columns=['fips_key'])
    print(f'  Centroids matched : {mort["latitude"].notna().sum():,}')

    mort = mort[mort['res_statefips'] != 'AK'].reset_index(drop = True)
    if output_path:
        mort.to_csv(output_path, index=False)
        size_mb = os.path.getsize(output_path) / 1024 / 1024
        print(f'\n✓ Saved -> {output_path}  ({size_mb:.2f} MB)')
 
    return mort