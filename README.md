# survey_climate_change
Survey on the views on climate change across the Canadian forest sector

This repository contains the code needed to analyze the data gathered on a survey on the views of climate change,
its impacts, adaptation options and limitations to adapt, conducted across several stakeholders belonging to the Canadian forest sector.

The survey was conducted in spring 2015, and included > 1000 participants from British Columbia, Alberta, Ontario, Quebec and
New Brunswick. The repository contains the following scripts:


* `00_Import_Clean_Data.R`: reads the raw data as provided by the survey software (`raw_SurveyData.Rdata`) and does some wrangling 
(clean mispellings, group some variables, etc). The results are saved as `SurveyData_Clean.Rdata`

* `00_Weight_Data.R`: reads the former file (`SurveyData_Clean.Rdata`) and assigns weights based on the different proportions of each 
stakeholder across provinces. Saves the resulting data frame as `SurveyData_Clean_Weighted.Rdata`

* `01_Sociodemography.R`: performs some sociodemographic analyses: assesses the distribution of gender, age, education level, stakeholder, province and forest type across provinces and stakeholders. It also evaluates the differences in NEP and political view across these sociodemographic variables.

* `02_LikertR`: analyzes the likert type questions of the survey, using the tools in the `likert`package. It does not plot any figure, but created the object needed to plot them using the `Figures.R`script.

* `03_Random_forests.R`: 

* `survey_functions.R`contains all the functions to perform the analyses. It does not modify any file *per se*, but needs to be loaded in all sessions to perform the analyses.

* `zz_AMZ_colors.R`: contains a series of color palettes to create the figures






