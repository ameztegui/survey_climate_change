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
    
    # Individual trees --------------------------------------------------------
    
    load("./data/var_select5.Rdata")
      
    
    
    variables_incl <- lapply(select_vars_5,function(l) l[[2]]) 
    
    for (i in 5:5) { 
        potvars<- c("Province", "Stakeholder" , "Gender", "Age", 
                    "Education", "Forest_Type", "Politics")
        selvars <- na.omit(potvars[unlist(variables_incl[i-4])])
        depvars <- colnames(survey)[i]
      
        work_survey <- survey %>%
            dplyr::select( selvars, depvars ) %>%
            filter(complete.cases((depvars)))
         print(names(work_survey))    
    }      
    
    # Create function to compute classification accuracy
    
    class_accur <- function (data) {
        #data <- as.matrix(data)
        correct <- sum(diag(data))
        total <- sum(data)
        accuracy <- correct/total
        return(accuracy)
    }
        set.seed(42)           
        ct <- ctree(Human_Cause ~ . ,
                    data = work_survey, controls = ctree_control(maxdepth = 3, 
                                                                 mincriterion = 0.99))
        predictCT<-predict(ct, newdata = NULL, OOB = T)
        confusion <- table(work_survey[,1],predictCT)
        print(confusion)
        classi<-(paste0("(Classif.accuracy = ", round(class_accur(confusion)*100,1),"%)"))
        
        # simpler version of plot
        
        #  pdf(file=paste0("D:/Dropbox (FiDBosc)/Research/Figures/RF",i,".pdf"), width=10)
        plot(ct, inner_panel=node_inner(ct,
                                        abbreviate =F,            # short variable names
                                        pval = T,                 # no p-values
                                        id = FALSE),
             main=paste(mf_labeller(colnames(work_survey)[1]),classi, sep="\n"))
        dev.off()       
        
        

    
    
    
    