# Cherry Blossom Bloom Phenology: Hierarchical Climate Controls and Phenological Memory Forecasting

This repository contains the R and Python code accompanying the manuscript:

> **Hierarchically controlled flowering timing and implications for phenological forecasting**  
> Wenhuan Xu et al.

---

## Overview

We investigate the drivers of cherry blossom bloom timing across three focal cities — **Kyoto (Japan)**, **Liestal (Switzerland)**, and **Washington D.C. (USA)** — and ~330 cities spanning Asia, Europe, and North America. Using regression trees, random forests, and leave-one-year-out (LOYO) cross-validation, we show that:

1. Bloom day-of-year (DOY) is governed by a **hierarchical climate threshold structure**: chilling accumulation sets a temperature floor; subsequent heat accumulation (GDD5) then triggers flowering within a photoperiod-modulated window.
2. Incorporating **phenological memory** (prior k-year mean bloom DOY, k = 2–10) substantially improves prediction accuracy and enables genuine out-of-sample forecasting.
3. A Random Forest model augmented with an 8-year lag-DOY window (RF+Lag, k = 8) produces the best validated forecasts across all three cities (LOYO RMSE ≤ 5 days; true out-of-sample 2021–2024 accuracy confirmed).

---

## Repository structure

```
├── data/                          # Processed datasets (see data/README.md)
├── Fig1 simulated hist plot.R     # Fig. 1 — null-model distribution comparison
├── Fig2a-d bloom_plots_updated.R  # Fig. 2a–d — historical bloom trends per city
├── Fig2a-f blossom DD5 analysis.R # Fig. 2 combined version
├── Fig2e_GDD5 sensitivity_analysis.R   # Fig. 2e — GDD5 sensitivity shift analysis
├── Fig2ef_biome_all_cities.R      # Fig. 2e–f — Whittaker biome + trend across 330 cities
├── Fig3 Regression Tree Model comprsn Predicted_Actual_DOY.R  # Fig. 3 regression tree
├── Fig3-4a-c Model comprsn Predicted_Actual_DOY.R             # Fig. 3–4 model comparison
├── Fig3d_f_PDP_SEM_analysis.R     # Fig. 3d–f — PDP and SEM analysis
├── Fig4d_cities_distribution.R    # Fig. 4d — city-level spatial distribution
├── cherry_blossom_tree_rf_forecasting.R  # Core M1–M4 model fitting and LOYO validation
├── lag_DOY_analysis.R             # Phenological memory: lag-DOY k=2–10 model sweep
├── fig4_composite.R               # Fig. 4 composite panel assembly
├── fig4_v2_composite.R            # Fig. 4 revised composite (final)
├── lastDOY and currentDOY.R       # Lag predictor construction utilities
├── shapiro_bloom_daylight.R       # Normality and photoperiod checks
├── FigS*.R / TableS*.R            # Supplementary figures and tables
├── FL-*.R                         # Collaboration scripts (F. He)
├── 1921-1983 three cities weather data.py   # Historical weather scraping
├── 1984-2024 three cities weather data.py   # Modern weather scraping
├── Future 30 days weather from OpenWeatherMap.py  # Real-time forecast retrieval
└── Other_cities_weather.py        # Weather collection for 330-city dataset
```

---

## Models

Four model classes are evaluated throughout:

| ID | Name | Predictors |
|----|------|-----------|
| M1 | RT | Climate variables only (regression tree) |
| M2 | RF | Climate variables only (random forest) |
| M3 | TS-RT | Climate + lag-DOY (regression tree) |
| M4 | RF+Lag | Climate + lag-DOY (random forest) — **best overall** |

**Climate predictors** include: cumulative GDD5, mean temperature (Jan–Mar, Feb–Mar, Mar), photoperiod at bloom, chilling days.  
**Lag-DOY predictors**: mean bloom DOY over the prior k years (k = 2–10, evaluated separately).

**Validation approaches:**
- Leave-one-year-out (LOYO) cross-validation across all years 1940–2024
- True out-of-sample forecast: trained on years ≤ 2020, evaluated on 2021–2024

---

## Key data files

| File | Description |
|------|-------------|
| `data/3cities_doy_weather_1940_2024.csv` | Main modelling dataset: bloom DOY + climate variables for Kyoto, Liestal, Washington DC (1940–2024) |
| `data/loyo_predictions_all_models.csv` | LOYO predicted vs. observed DOY for all four models |
| `data/lag_DOY_model_comparison.csv` | RMSE / R² for each model × lag window combination |
| `data/forecast_predictions_2021_2024_all_models.csv` | True out-of-sample forecasts 2021–2024 |
| `data/model_comparison_metrics.csv` | Summary metrics table (all models, all cities) |
| `data/LOYO_model_metrics.csv` | Per-city LOYO performance breakdown |
| `data/Whittaker_biomes_plotbiomes.csv` | Whittaker biome polygon data (for Fig. 2e–f) |
| `data/Kyoto_Bloom_DOY_with_Photoperiod.csv` | Kyoto bloom DOY with derived photoperiod values |
| `data/Fig2f_trend_data_used.csv` | Processed trend data for ~330 cities |
| `data/kyoto.csv`, `data/liestal.csv`, `data/washingtondc.csv` | Raw bloom DOY records |

See `data/README.md` for full dataset descriptions and data source citations.

---

## Requirements

**R** (≥ 4.2) with packages:

```r
install.packages(c(
  "tidyverse", "rpart", "randomForest", "car",
  "ggplot2", "patchwork", "sf", "scales",
  "mgcv", "xgboost", "caret", "cowplot"
))
```

**Python** (≥ 3.9) with packages:

```
requests, pandas, numpy
```

An [OpenWeatherMap API key](https://openweathermap.org/api) is required for `Future 30 days weather from OpenWeatherMap.py`.

---

## Reproducing the main figures

```r
# Fig. 2e — GDD5 sensitivity period shift (1942–1971 vs 1995–2024)
source("Fig2e_GDD5 sensitivity_analysis.R")

# Fig. 2e–f — Whittaker biome + 330-city trend
source("Fig2ef_biome_all_cities.R")

# Core LOYO validation and model comparison (M1–M4)
source("cherry_blossom_tree_rf_forecasting.R")

# Phenological memory sweep (lag k = 2–10)
source("lag_DOY_analysis.R")   # runtime ~15–25 min

# Fig. 4 composite (6-panel)
source("fig4_v2_composite.R")
```

---

## Citation

If you use this code or data, please cite:

> Xu W. et al. (2026). Hierarchically controlled flowering timing and implications for phenological forecasting. *[Journal name, in review]*.

Raw bloom DOY data are from their respective original sources; see `data/README.md` for per-dataset citation requirements.

---

## License

MIT © 2026 Wenhuan Xu
