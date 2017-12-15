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
    
    var_select <- function (variables, n_likert) {
        var_results <- list()
        nsurvey <- survey
        for (i in variables) {
            
            nsurvey[,i]<- ordered(nsurvey[,i])
            if (n_likert == "3a") {
                levels(nsurvey[,i]) <- c("Disagree", "Disagree", 
                                         "Unsure", "Unsure", "Unsure", 
                                         "Agree", "Agree") 
                print(table(nsurvey[,i])) 

            }  else if (n_likert == "3b") {
                levels(nsurvey[,i]) <- c("Disagree", "Disagree", "Disagree",
                                         "Unsure", 
                                         "Agree", "Agree", "Agree") 
                print(table(nsurvey[,i]))

            }  else if (n_likert == "3c") {
                levels(nsurvey[,i]) <- c("Disagree", "Disagree", "Disagree",
                                         "Unsure", "Unsure",
                                         "Agree", "Agree") 
                print(table(nsurvey[,i]))

            }  else if (n_likert == "5") {
                levels(nsurvey[,i]) <- c("Disagree", "Disagree", "Slightly disagree",
                                         "Unsure", "Slightly agree", "Agree", "Agree")
                print(table(nsurvey[,i]))
            }
        
        survey_rf <- nsurvey %>%
            dplyr::select (Province, Stakeholder , Gender, Age, 
                           Education, Forest_Type, Politics,nep,get(i)) %>%
            filter(complete.cases(.))

        set.seed(42)
        
        relimp <- VSURF(x = survey_rf[,1:8],
                        y = survey_rf[[9]])
        
        results <- list(threshold = relimp$varselect.thres,
                        interp = relimp$varselect.interp,
                        predict = relimp$varselect.pred)
        var_results[[which(variables==i)]] <- results
        }
        return(var_results)
    }
    
    # levels of the likert response
    
    # 3a: disagre, disagree, unsure, unsure, unsure, agree, agree
    # 3b: disagree, disagree, disagree, unsure, agree, agree, agree
    # 3c: disagree, disagree, disagree, unsure, unsure, agree, agree
    # 5: disagree, disagree, slightly dis., unsure, slightly agree, agree, agree
    

    
    
            first_part <- colnames(survey)[5:22]
            
        select_vars_3a <- var_select(first_part, "3a")
        save(select_vars_3a, file="./data/var_select3a_nep.Rdata")
        
        select_vars_3b <- var_select(first_part, "3b")
        save(select_vars_3b, file="./data/var_select3b_nep.Rdata")
    
        select_vars_3c <- var_select(first_part, "3c")
        save(select_vars_3c, file="./data/var_select3c_nep.Rdata")
        
        select_vars_5 <- var_select(first_part, "5")
        save(select_vars_5, file="./data/var_select5.Rdata")

    
    
    