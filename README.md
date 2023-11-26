# Ambio Adaptive management modelling and analysis
This repository includes the code to run GOTM modelling using a sensitivity approach to investigate how changes in flow will interact with air temeprature warming to impact lake water temperatures. 
The scripts to run the workflow are in the `Workflows` folder. The `GOTM` folder contains all the configuration files needed to run GOTM as well as the baseline driving data and the `R` folder contains additional functions to complete the workflow. 

The steps are 1. run GOTM scenarios; 2. run post-model analysis; and 3. Generate figures
## To reproduce figures only
- Run `Workflows/05_manuscript_plots.R`

## To rerun post-modelling analysis
- Run `Workflows/02_xxx`
- Run `Workflows/03_xxx`
- Run `Workflows/04_xxx`
- Run `Workflows/05_manuscript_plots.R`
  
## To run the GOTM scenarios
- Run `Workflows/01_run_scenarios.R`
- Run `Workflows/02_xxx`
- Run `Workflows/03_xxx`
- Run `Workflows/04_xxx`
- Run `Workflows/05_manuscript_plots.R`

The configuration files for GOTM calibration using ParSAC are also provided but not included in the above workflow. 
