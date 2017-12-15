rm(list=ls())

load("./data/SurveyData_Clean_Weighted.Rdata")

source("./code/survey_functions.R")

#library(relaimpo)
library(party)
library(tidyverse)
library(VSURF)

# Redefine the levels of the explanatory variables
    survey$Politics <- ordered(survey$Politics)
    survey$Age <- ordered(survey$Age)
    survey$Education  <- ordered(survey$Education)
    
    levels(survey$Province) <- c("BC", "AB", "ON", "QC", "NB")
    levels(survey$Stakeholder) <- c("F.Gov", "P.Gov", "Ind", "Pri", "Aca", "Stu")
    levels(survey$Gender) <- c("Fem", "Mal", NA)
    levels(survey$Age) <- c("<34", "35-44", "45-54", "55-64")
    levels(survey$Education) <- c("Non U.", "BSc", "MSc", "PhD")
    levels(survey$Forest_Type) <- c("Boreal",  "Mixed", NA,"Temp.")
    
    
# Define the most important variables using VSURF
    
    var_select <- function (variables) {
        var_results <- list()
        for (i in variables) {
        survey_rf <- survey %>%
            dplyr::select (Province, Stakeholder , Gender, Age, 
                           Education, Forest_Type, Politics,get(i)) %>%
            filter(complete.cases(.))

        set.seed(42)
        relimp <- VSURF(x = survey_rf[,1:7],
                        y = survey_rf[[8]])
        
        results <- list(threshold = relimp$varselect.thres,
                        interp = relimp$varselect.interp,
                        predict = relimp$varselect.pred)
        var_results[[which(variables==i)]] <- results
        }
        return(var_results)
    }
    
    # 3 levels of the likert response
        for (i in 5:22) {
            survey[,i]<- ordered(survey[,i])
            levels(survey[,i]) <- c("Disagree", "Disagree", "Disagree",
                                    "Unsure", "Unsure", "Agree", "Agree")
            print(table(survey[,i]))
        }
    
        first_part <- colnames(survey)[5:22]
        select_vars_threelev_b <- var_select(first_part)
    save(select_vars_threelev_b, file="./data/var_select3b.Rdata")
    
    
        

    }
    
    
    