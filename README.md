# SDG-Modelling

Preliminary PhD Research: Using factor analyses and regression modelling on UN Global Goals data.

This submission consists of working with World Bank and UN data relating to the SDGs. Data wrangling, restructuring, visualisation, modelling, and Principal Component Analysis is performed. 

### Data

Please note that the raw data from the UN Database that was used in this repo can be accessed directly from [here](https://unstats.un.org/sdgs/indicators/database/) and is not included. This should be placed in /data/new.

### Structure 

It is comprised of 6 components:
- 3 Jupyter Notebooks which wrangle and visualise data [0,1,2] 
- 2 Jupyter Notebooks which perform predictions based on models previously developed for Child Mortality [3,4]
- One R script which performs PCA factor analysis on different datasets

Packages The only additional Python package used is the World Bank API: 
- wbdata

The additional R packages used are:
- PerformanceAnalytics 
- FactoMineR 
- factoextra 
- lavaan

### References 
The work is based on the following papers:
- Spaiser, V., Ranganathan, S., Bali Swain, R. and Sumpter, D.J.T. (2017): The sustainable development oxymoron: quantifying and modelling the incompatibility of sustainable development goals. International Journal of Sustainable Development & World Ecology, 24(6), 457-470.
- Ranganathan, S., Nicolis, S.C., Bali Swain, R. and Sumpter, D.J.T. (2017): Setting development goals using stochastic dynamical system models. PLoS ONE, 12(2)

Data Sources
- Old data from the Spaiser - “oxymoron” folder 
- Data provided by the UN directly regarding four countries (Bangladesh, Laos, Ethiopia, Tanzania) – “vito” folder
- The World Bank - Accessed through API to fetch data for given targets 
- The World Bank - wb_income_groups.csv, wb_income_groups_2013.csv 
- UNSTATS SDG Database - should be downloaded from UNSTATS and named SDG_all_countries_90_19.csv 
- UN Data - UN_country_codes.csv 
- The Heritage Foundation Index of Economic Freedom - heritage_data_2019.csv 
- Our World in Data - OWiD_co2-emissions-per-capita.csv
