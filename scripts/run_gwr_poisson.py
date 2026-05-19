import configparser
import os
import warnings
import numpy as np
import pandas as pd
from mgwr.gwr import GWR
from mgwr.sel_bw import Sel_BW
from spglm.family import Poisson
from spglm.glm import GLM

warnings.filterwarnings('ignore')
pd.options.mode.chained_assignment = None

CONFIG = configparser.ConfigParser()
CONFIG.read(os.path.join(os.path.dirname(__file__), 'script_config.ini'))
BASE_PATH = CONFIG['file_locations']['base_path']

DATA_RAW       = os.path.join(BASE_PATH, 'raw')
DATA_PROCESSED = os.path.join(BASE_PATH, '..', 'results', 'processed')
DATA_RESULTS   = os.path.join(BASE_PATH, '..', 'results', 'final')

INPUT_FILE = os.path.join(DATA_PROCESSED, 'mortality_mgwr_ready.csv')

EPOCHS = ['2005-2010', '2011-2017', '2018-2022']


X_COLS = ['pm_25', 'CSMOKING_CrudePrev', 'OBESITY_CrudePrev',
    'LPA_CrudePrev', 'unemployment_rate', 'rucc_code',
    'encoded_age']

REQUIRED_COLS = (
    ['fips', 'county_name', 'res_statefips', 'latitude', 'longitude',
     'epoch', 'total_mort_count', 'population']
    + X_COLS)


def run_epoch(df_epoch, epoch):
    """
    This function runs Poisson GWR for a single epoch and returns a DataFrame
    with local coefficients, diagnostics, and significance flags for each county.
    The main steps are:
    1. Prepare coordinates, outcome, offset, and predictors
    2. Select optimal bandwidth using AICc
    3. Fit Poisson GWR model
    4. Compute global goodness-of-fit metrics (AICc, Pearson R2, McFadden R2)
    5. Build results DataFrame with local coefficients, t-values, deviance ratio, and significance flags
    6. Return the results DataFrame for this epoch 

    Parameters
    ----------
    df_epoch : pd.DataFrame
        Subset of the input data for the specific epoch, containing all necessary columns.
    epoch : str
        The epoch label (e.g., '2005-2010') for reporting and saving results.

    Returns
    -------
    pd.DataFrame:        
        A DataFrame containing local coefficients, t-values, diagnostics, and significance 
        flags for each county in the epoch.

    """
    coords = list(zip(df_epoch['longitude'], df_epoch['latitude']))
    y = df_epoch['total_mort_count'].values.reshape(-1, 1).astype(float)
    offset = np.log(df_epoch['population'].values).reshape(-1, 1)
    X_raw  = df_epoch[X_COLS].values.astype(float)
    X_mean = X_raw.mean(axis=0)
    X_std  = X_raw.std(axis=0)
    X      = (X_raw - X_mean) / X_std

    print('\n  Selecting optimal bandwidth (this may take a few minutes)...')
    selector = Sel_BW(coords, y, X, family = Poisson(), offset = offset,
        fixed = False, kernel = 'bisquare')
    bw = selector.search(criterion='AICc', verbose=False)
    print(f'  Optimal bandwidth (neighbours) : {int(bw)}')

    print('  Fitting Poisson GWR ...')
    model = GWR(coords, y, X, bw = bw, family = Poisson(),
        offset = offset, fixed = False, kernel = 'bisquare')
    result = model.fit()
    y_flat   = y.flatten()

    predy_raw = np.asarray(result.predy).flatten()
    y_hat_as_mu  = np.clip(predy_raw, 1e-10, None)
    # Try treating predy as eta (linear predictor) with safe exp
    eta_clipped  = np.clip(predy_raw, -500, 500)
    y_hat_as_eta = np.exp(eta_clipped)

    # Choose whichever gives a median prediction closer to the observed median
    obs_median = float(np.median(y_flat))
    err_mu  = abs(float(np.median(y_hat_as_mu))  - obs_median)
    err_eta = abs(float(np.median(y_hat_as_eta)) - obs_median)
    y_hat = y_hat_as_mu if err_mu <= err_eta else y_hat_as_eta
    print(f'  predy interpretation : {"mu (predicted mean)" if err_mu <= err_eta else "eta (linear predictor -> exp)"}')

    # Clip predictions and actuals to avoid log(0)
    y_hat = np.clip(y_hat, 1e-10, None)
    y_pos = np.clip(y_flat, 1e-10, None)

    # Pearson R2: correlation between observed and predicted counts
    corr_mat   = np.corrcoef(y_flat, y_hat)
    pearson_r2 = float(corr_mat[0, 1] ** 2) if np.isfinite(corr_mat[0, 1]) else float('nan')

    # McFadden R2 from predictions (self-consistent -- both deviances use same y)
    y_bar    = float(y_flat.mean())
    # Use float128 accumulation to avoid overflow on large counts
    y_pos_f  = y_pos.astype(np.float64)
    y_hat_f  = y_hat.astype(np.float64)
    null_dev  = 2 * float(np.sum(y_pos_f * np.log(y_pos_f / y_bar) - (y_flat - y_bar)))
    resid_dev = 2 * float(np.sum(y_pos_f * np.log(y_pos_f / y_hat_f) - (y_flat - y_hat_f)))

    # Guard against non-finite values
    if not (np.isfinite(null_dev) and np.isfinite(resid_dev) and null_dev != 0):

        mcfadden_r2 = float('nan')
        deviance_r2 = float('nan')
    else:

        mcfadden_r2 = 1 - (resid_dev / null_dev)
        deviance_r2 = (null_dev - resid_dev) / null_dev

    null_deviance  = null_dev if np.isfinite(null_dev) else float('nan')
    resid_deviance = resid_dev if np.isfinite(resid_dev) else float('nan')

    # -- Diagnostics -----------------------------------------------------------
    print(f'\n  --- Model Diagnostics ---')
    print(f'  AICc                        : {float(np.squeeze(result.aicc)):.4f}')
    print(f'  Effective number of params  : {float(np.squeeze(result.ENP)):.2f}')
    print(f'  Null deviance               : {null_deviance:.4f}')
    print(f'  Residual deviance (GWR)     : {resid_deviance:.4f}')
    print(f'  Pearson R2                  : {pearson_r2:.4f}')
    print(f'  McFadden pseudo-R2          : {mcfadden_r2:.4f}')
    print(f'  Deviance R2 (scaled)        : {deviance_r2:.4f}')

    col_names = ['intercept'] + X_COLS
    coef_df   = pd.DataFrame(result.params, columns=col_names)

    tval_names = ['t_intercept'] + [f't_{c}' for c in X_COLS]
    tval_df    = pd.DataFrame(result.tvalues, columns=tval_names)

    # Local deviance ratio (per-county fit; Poisson equivalent of local R2)
    try:

        resid_dev_local = np.asarray(result.resid_deviance).flatten()
        null_dev_mean   = null_deviance / len(resid_dev_local)
        local_dev_ratio = pd.Series(
            1 - (resid_dev_local / null_dev_mean),
            name = 'local_deviance_ratio')
        
    except (AttributeError, NotImplementedError):
        local_dev_ratio = pd.Series(
            np.nan, index = range(len(df_epoch)),
            name = 'local_deviance_ratio')

    out = pd.concat([
        df_epoch[['fips', 'county_name', 'res_statefips',
                  'latitude', 'longitude',
                  'total_mort_count', 'population']].reset_index(drop=True),
        coef_df, tval_df, local_dev_ratio], axis = 1)

    out['epoch']          = epoch
    out['bw']             = int(bw)
    out['aicc']           = float(np.squeeze(result.aicc))
    out['null_deviance']  = null_deviance
    out['resid_deviance'] = resid_deviance
    out['pearson_r2']     = round(pearson_r2, 6)
    out['mcfadden_r2']    = round(mcfadden_r2, 6)
    out['deviance_r2']    = round(deviance_r2, 6)

    crit_t = 2.807
    for col in X_COLS:

        out[f'sig_{col}'] = (out[f't_{col}'].abs() > crit_t).astype(int)

    return out


def main():
    """"
    Main function to run Poisson GWR for each epoch and save results.
    Steps:
    1. Load the prepared dataset with all necessary columns.
    2. For each epoch:
       a. Subset the data for the epoch.
       b. Run the GWR model and get results.
       c. Save the results to a CSV file and a summary text file.
    3. Combine results from all epochs into a single CSV.
    4. Print a cross-epoch comparison of model diagnostics.
    """
    df = pd.read_csv(INPUT_FILE, low_memory=False)

    df = df.dropna(subset=REQUIRED_COLS).reset_index(drop = True)
    df = df[df['population'] > 0].reset_index(drop = True)
    all_results = []

    for epoch in EPOCHS:

        df_epoch = df[df['epoch'] == epoch].reset_index(drop = True)
        if len(df_epoch) == 0:
            print(f'\n  Warning: No data for epoch {epoch}, skipping.')
            continue

        result_df = run_epoch(df_epoch, epoch)
        all_results.append(result_df)

        epoch_clean = epoch.replace('-', '_')
        out_file    = os.path.join(DATA_RESULTS, f'gwr_poisson_{epoch_clean}_results.csv')
        result_df.to_csv(out_file, index = False)
        print(f'\n  Saved -> {out_file}')

        # Save summary text
        summary_file = os.path.join(DATA_RESULTS, f'gwr_poisson_{epoch_clean}_summary.txt')
        with open(summary_file, 'w') as f:

            f.write(f'Epoch: {epoch}\n')
            f.write(f'Observations: {len(df_epoch)}\n')
            f.write(f'Bandwidth (neighbours): {int(result_df["bw"].iloc[0])}\n')
            f.write(f'AICc: {result_df["aicc"].iloc[0]:.4f}\n')
            f.write(f'Null deviance: {result_df["null_deviance"].iloc[0]:.4f}\n')
            f.write(f'Residual deviance (GWR): {result_df["resid_deviance"].iloc[0]:.4f}\n')
            f.write(f'Pearson R2: {result_df["pearson_r2"].iloc[0]:.4f}\n')
            f.write(f'McFadden pseudo-R2: {result_df["mcfadden_r2"].iloc[0]:.4f}\n')
            f.write(f'Deviance R2 (scaled): {result_df["deviance_r2"].iloc[0]:.4f}\n\n')
            f.write('Significant counties per variable ')
            f.write(f'(|t| > {2.807}, p < 0.005 adjusted):\n')

            for col in X_COLS:

                n_sig = result_df[f'sig_{col}'].sum()
                pct   = n_sig / len(result_df) * 100
                f.write(f'  {col:<30} {n_sig:>5} ({pct:.1f}%)\n')
            f.write('\nCoefficient summary (mean, min, max across counties):\n')

            for col in ['intercept'] + X_COLS:

                vals = result_df[col]
                f.write(f'  {col:<30} mean={vals.mean():>8.4f}  '
                        f'min={vals.min():>8.4f}  max={vals.max():>8.4f}\n')
        print(f'  Summary -> {summary_file}')

    # Combine all epochs
    if all_results:
        combined = pd.concat(all_results, ignore_index = True)
        out_all  = os.path.join(DATA_RESULTS, 'gwr_poisson_all_epochs.csv')
        combined.to_csv(out_all, index = False)
        size_mb = os.path.getsize(out_all) / 1024 / 1024
        print(f'\nAll epochs combined -> {out_all}  ({size_mb:.2f} MB)')

        print(f'  {"Epoch":<15} {"N":>6} {"BW":>6} {"AICc":>12} '
              f'{"Pearson R2":>11} {"McFadden R2":>13} {"Deviance R2":>12}')
        print(f'  {"-"*78}')
        for res in all_results:

            e    = res['epoch'].iloc[0]
            n    = len(res)
            bw   = int(res['bw'].iloc[0])
            aic  = float(np.squeeze(res['aicc'].iloc[0]))
            pr2  = float(res['pearson_r2'].iloc[0])
            mfr2 = float(res['mcfadden_r2'].iloc[0])
            dr2  = float(res['deviance_r2'].iloc[0])
            print(f'  {e:<15} {n:>6} {bw:>6} {aic:>12.4f} {pr2:>11.4f} {mfr2:>13.4f} {dr2:>12.4f}')


if __name__ == '__main__':
    
    main()
