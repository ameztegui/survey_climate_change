# survey_climate_change
Survey on the views on climate change across the Canadian forest sector

This repository contains the code needed to analyze the data gathered on a survey on the views of climate change,
its impacts, adaptation options and limitations to adapt, conducted across several stakeholders belonging to the Canadian forest sector.

The survey was conducted in spring 2015, and included > 1000 participants from British Columbia, Alberta, Ontario, Quebec and
New Brunswick. The repository contains the following scripts:


* 00_Import_Clean_Data.R: reads the raw data as provided by the survey software (raw_SurveyData.Rdata) and does some wrangling 
(clean mispellings, group some variables, etc). The results are saved as SurveyData_Clean.Rdata

* 00_Weight_Data.R: reads the former file (SurveyData_Clean.Rdata) and assigns weights based on the different proportions of each 
stakeholder across provinces. Saves the resulting data frame as SurveyData_Clean_Weighted.Rdata

* 01_Sociodemography.R: calculates some sociodemographic 
