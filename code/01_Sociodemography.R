
## Sociodemographics per province and organization ##

rm(list=ls())

source("./code/survey_functions.R")
load("./data/SurveyData_Clean_Weighted.Rdata")

library(scales)


# Sociodemographics per province ------------------------------------------
    
    ## Gender
    chisq (dataframe=survey, dep="Gender", indep="Province","right") +
    annotate("text",x=c(1,2,3,4,5),y=1.05,ymax=1.10,label=c("a","a","a","a","b"),size=4)
    
    ## Age
    chisq (dataframe=survey, dep="Age", indep="Province","n") +
    annotate("text",x=c(1,2,3,4,5),y=1.05,ymax=1.10,label=c("a","a","a","b","a"),size=4) 
    
    ## Education
    chisq (dataframe=survey, dep="Education", indep="Province","n") +
     annotate("text",x=c(1,2,3,4,5),y=1.05,ymax=1.10,label=c("a","a","a","b","a"),size=4) 
    
    ## Stakeholder
    chisq (dataframe=survey, dep="Stakeholder", indep="Province","right") +
    annotate("text",x=c(1,2,3,4,5),y=1.05,ymax=1.10,label=c("a","ac","b","c","ab"),size=4) 
    
    ## Forest_Type
    chisq (dataframe=survey, dep="Forest_Type", indep="Province","n") +
    annotate("text",x=c(1,2,3,4,5),y=1.05,ymax=1.10,label=c("a","b","c","c","d"),size=4) 
     

### By stakeholder ####
    
    ## Gender
    chisq (dataframe=survey, dep="Gender", indep="Stakeholder","n") +
    annotate("text",x=c(1,2,3,4,5,6),y=1.05,ymax=1.10,label=c("a","a","a","a","a","b"),size=4)
    
    ## Age
    chisq (dataframe=survey, dep="Age", indep="Stakeholder","n") +
    annotate("text",x=c(1,2,3,4,5,6),y=1.05,ymax=1.10,label=c("a","a","b","b","b","c"),size=4)
    
    ## Education
    chisq (dataframe=survey, dep="Education", indep="Stakeholder","n") +
    annotate("text",x=c(1,2,3,4,5,6),y=1.05,ymax=1.10,label=c("b","c","e","cde","a","d"),size=4)
    
    ## Province
    wtd_chisq (dataframe=survey, dep="Province", indep="Stakeholder","right") 
    annotate("text",x=c(1,2,3,4,5,6),y=1.05,ymax=1.10,label=c("a","a","a","a","a","b"),size=4) 
    
    ## Forest Type
    chisq (dataframe=survey, dep="Forest_Type", indep="Stakeholder","n") +
    annotate("text",x=c(1,2,3,4,5,6),y=1.05,ymax=1.10,label=c("a","ab","ab","c","bc","bc"),size=4) 

### NEP and Politics across indep. variables ------------------------------------------

     ## New Ecological Paradigm
     survey_comparison (dataframe=survey,dep="nep",indep="Province")
     survey_comparison (dataframe=survey,dep="nep",indep="Gender")
     survey_comparison (dataframe=survey,dep="nep",indep="Age")
     survey_comparison (dataframe=survey,dep="nep",indep="Education")
     survey_comparison (dataframe=survey,dep="nep",indep="Stakeholder")
     survey_comparison (dataframe=survey,dep="nep",indep="Forest_Type")
     survey_comparison (dataframe=survey,dep="nep",indep="Politics")


     ## Political View
     survey_comparison (dataframe=survey,dep="Political_View",indep="Province")
     survey_comparison (dataframe=survey,dep="Political_View",indep="Gender")
     survey_comparison (dataframe=survey,dep="Political_View",indep="Age")
     survey_comparison (dataframe=survey,dep="Political_View",indep="Education")
     survey_comparison (dataframe=survey,dep="Political_View",indep="Stakeholder")
     survey_comparison (dataframe=survey,dep="Political_View",indep="Forest_Type")



