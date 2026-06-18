# Data and Reproducibility Notes

This repository shares the main code and figure data used for the published Brazil wildfire-related PM2.5 mortality analysis.

## Included materials

- `Main_model/code_main_model.R`: main two-stage time-series modelling workflow.
- `Main_model/readme`: short description of the statistical analysis and method references.
- `Sensitivity_analysis/readme`: sensitivity analysis settings used in the study.
- `Code_for_plotting/code_Figure2.R`: plotting code for Figure 2.
- `Data_for_Figures/`: prepared figure data files used for the published visualizations.

## Analysis framework

The main statistical approach uses:

1. first-stage subregion-specific time-series models;
2. distributed lag linear modelling with the `dlnm` R package;
3. quasi-Poisson regression for mortality counts;
4. controls for time trend, day of week, holidays, temperature, and relative humidity;
5. second-stage meta-analysis with the `mvmeta` R package.

## Sensitivity analyses

The sensitivity analysis varied:

- maximum lag time for wildfire-related PM2.5;
- degrees of freedom for the lag dimension;
- degrees of freedom for meteorological variables;
- moving-average window for temperature.

## Reproducibility boundary

The repository is intended to support transparency around the published analysis, but it is not a fully self-contained raw-data release. Users should check the licenses, access rules, and citation requirements for any external exposure, mortality, meteorological, or geographic datasets needed to rerun the full pipeline.

## Citation

Please cite the published Nature Communications article when using this repository.
