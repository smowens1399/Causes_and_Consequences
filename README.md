# Causes and consequences of juvenile migratory diversity in a wild population yearling Chinook Salmon
Includes raw data, processed data and code used in project analysis.
## Software
 - R
 - R-studio
 - Microsoft Excel
## Running Code
The order in which code is run is important to perform analysis. Below the order code should be run has been listed
### Code Order
#### Intial code
Before doing anything the following files should be run
- lgr_efficiency
- strategy categorization
- abundance
#### causes
It doesn't matter whether causes or consequences is run first, but this is the order for within causes
1. causes_prep
2. cayses_model_build
3. causes_model_ranking
4. causes_model_summary
#### consequeces
Within each folder within consequences there will be two file types; prep and analysis. Prep should always be run before analysis. Read files from folders in the following order.
1. growth
2. ocean_entry
3. sar
4. return age
   

   

