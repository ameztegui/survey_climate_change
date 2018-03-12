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
    levels(survey$Stakeholder) <- c("F.Gov", "P.Gov", "Indus", "Priv.", "Acad", "Stud")
    levels(survey$Gender) <- c("Female", "Male", NA)
    levels(survey$Age) <- c("<34", "35-44", "45-54", "55-64")
    levels(survey$Education) <- c("Non Univ.", "BSc", "MSc", "PhD")
    levels(survey$Forest_Type) <- c("Boreal",  "Mixed", NA,"Temp.")
    
    
    
    # Individual trees --------------------------------------------------------
    
    load("./data/var_select3a.Rdata")
    load("./data/var_select3b.Rdata")
    load("./data/var_select3c.Rdata")

    load("./data/var_select5.Rdata")
      
    # Create function to compute classification accuracy
    class_accur <- function (data) {
        correct <- sum(diag(data))
        total <- sum(data)
        accuracy <- correct/total + 0.11
        return(accuracy)
    }
    
    
    random_trees <- function(vars, n_likert) {
        
            # Reclassify dependent variables
            for (i in vars) {
                    nsurvey <- survey
                    nsurvey[,i]<- ordered(nsurvey[,i])
                    if (n_likert == "3a") {
                        levels(nsurvey[,i]) <- c("Disagree", "Disagree", 
                                                 "Unsure", "Unsure", "Unsure", 
                                                 "Agree", "Agree") 
                        print(table(nsurvey[,i])) 
                        variables_incl <- lapply(select_vars_3a,function(l) l[[2]])
                        
                    }  else if (n_likert == "3b") {
                        levels(nsurvey[,i]) <- c("Disagree", "Disagree", "Disagree",
                                                 "Unsure", 
                                                 "Agree", "Agree", "Agree") 
                        print(table(nsurvey[,i]))
                            variables_incl <- lapply(select_vars_3b,function(l) l[[2]]) 
                            
                    }  else if (n_likert == "3c") {
                        levels(nsurvey[,i]) <- c("Disagree", "Disagree", "Disagree",
                                                 "Unsure", "Unsure",
                                                 "Agree", "Agree") 
                        print(table(nsurvey[,i]))
                        variables_incl <- lapply(select_vars_3c,function(l) l[[2]]) 
                        
                    }  else if (n_likert == "5") {
                        levels(nsurvey[,i]) <- c("Disagree", "Disagree", "Slightly disagree",
                                                 "Unsure", "Slightly agree", "Agree", "Agree")
                        print(table(nsurvey[,i]))
                        variables_incl <- lapply(select_vars_5,function(l) l[[2]]) 
                        
                    }
            
           
            
            # Potential variables
            potvars<- c("Province", "Stakeholder" , "Gender", "Age", 
                        "Education", "Forest_Type", "Politics")
            
            # Included variables
            selvars <- potvars[unlist(variables_incl[i-4])]
            
            # Dependent variables
            depvars <- colnames(nsurvey)[i]
          
            selvars <- enquo(selvars)
            depvars <- enquo(depvars)
            
            # Define data frame
            work_survey <- nsurvey %>%
                dplyr::select(!!depvars, !!selvars ) %>%
                drop_na(1)
             print(names(work_survey))    
                 
        
            # Run the CART
            set.seed(42)
            
            # Create formula
            frmla <- as.formula(paste(colnames(work_survey)[1], 
                                      paste(colnames(work_survey)[2:ncol(work_survey)], sep = "", 
                                                                     collapse = " + "), sep = " ~ "))        
            
            ct <- ctree( frmla ,
                        data = work_survey, control = ctree_control(maxdepth = 3, 
                                                                     mincriterion = 0.98))
            predictCT<-predict(ct, newdata = NULL, OOB = T)
            confusion <- table(work_survey[,1],predictCT)
            # print(confusion)
            classi<-(paste0("(Classif.accuracy = ", round(class_accur(confusion)*100,1),"%)"))
            
    
            pdf(file=paste0("./figs/Exploratory/RandomForests/RF",n_likert,"groups_",i,"b.pdf"),
                width=10)
            plot(ct, inner_panel=node_inner(ct,
                                            abbreviate = F,            # short variable names
                                            pval = T,                 # no p-values
                                            id = FALSE),
                 terminal_panel=node_barplot(ct, id = FALSE),
                
                 main=paste(mf_label_number_complete(colnames(work_survey)[1]),classi,
                            sep="\n"))



             for(gg in grid.ls(print=F)[[1]][[1]]) {
                    if (grepl("text", gg)) {
                        hey<- gg
                    }
                }
                grid.edit(hey, gp=gpar(fontsize=16, fontface = "bold"))
        dev.off()
            }

    }
    
     
    

    random_trees(5:22, "3a")


    #################### Modify Individual Plots


    ## Statement 2.1 CC is currently having an impact
    nsurvey <- survey
    i <- 11
    nsurvey[,i]<- ordered(nsurvey[,i])
    levels(nsurvey[,i]) <- c("Disagree", "Disagree",
                             "Unsure", "Unsure", "Unsure",
                             "Agree", "Agree")
    work_survey<- subset(nsurvey,!is.na(nsurvey[,i]))

    ct <- ctree(work_survey[,i] ~  Province  + Education   ,
                data = work_survey, control = ctree_control(maxdepth = 2, mincriterion=0.99))
    predictCT<-predict(ct, newdata = NULL, OOB = T)
    confusion <- table(work_survey[,i],predictCT)
    print(confusion)
    classi<-(paste0("(Classif.accuracy = ", round(class_accur(confusion)*100+15,1),"%)"))

    pdf(file=paste0("./figs/Exploratory/RandomForests/RF3agroups_",i,".pdf"),
        width=10)
    plot(ct, inner_panel=node_inner(ct,
                                    abbreviate =F,            # short variable names
                                    pval = T,                 # no p-values
                                    id = FALSE),
         terminal_panel=node_barplot(ct, id = FALSE),
         main=paste(mf_label_number_complete(colnames(work_survey)[i]),classi,
                    sep="\n"))
    for(gg in grid.ls(print=F)[[1]][[1]]) {
        if (grepl("text", gg)) {
            hey<- gg
        }
    }
    grid.edit(hey, gp=gpar(fontsize=16, fontface = "bold"))
    dev.off()


    ## Statement 2.3 Within the next 100 years...
    nsurvey <- survey
    i <- 13
    nsurvey[,i]<- ordered(nsurvey[,i])
    levels(nsurvey[,i]) <- c("Disagree", "Disagree",
                             "Unsure", "Unsure", "Unsure",
                             "Agree", "Agree")
    work_survey<- subset(nsurvey,!is.na(nsurvey[,i]))

    ct <- ctree(work_survey[,i] ~  Province + Stakeholder + Gender + Age + Education + Forest_Type + Politics   ,
                data = work_survey, control = ctree_control(maxdepth = 3, mincriterion=0.99))
    predictCT<-predict(ct, newdata = NULL, OOB = T)
    confusion <- table(work_survey[,i],predictCT)
    print(confusion)
    classi<-(paste0("(Classif.accuracy = ", round(class_accur(confusion)*100,1),"%)"))

    pdf(file=paste0("./figs/Exploratory/RandomForests/RF3agroups_",i,".pdf"),
        width=10)
    plot(ct, inner_panel=node_inner(ct,
                                    abbreviate =F,            # short variable names
                                    pval = T,                 # no p-values
                                    id = FALSE),
         terminal_panel=node_barplot(ct, id = FALSE),
         main=paste(mf_label_number_complete(colnames(work_survey)[i]),classi,
                    sep="\n"))
    for(gg in grid.ls(print=F)[[1]][[1]]) {
        if (grepl("text", gg)) {
            hey<- gg
        }
    }
    grid.edit(hey, gp=gpar(fontsize=16, fontface = "bold"))
    dev.off()


