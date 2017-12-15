
load("./data/SurveyData_Clean_Weighted.Rdata")
source("./code/survey_functions.R")

library(likert)

# Make the values a factor with given levels (instead of numerical values)
    fct_survey <- survey    

    # For perceptions on CC
        for (i in 5:28) {
            fct_survey[[i]] <- factor(survey[[i]],
                           labels = c("Strongly disagree", "Disagree", "Slightly disagree",
                                                      "Neutral", "Slightly Agree", "Agree", "Strongly Agree"))
            }

      # New Ecological Paradigm
            for (i in c(73:87)){
              fct_survey[[i]] <- factor(survey[[i]], labels = c("Strongly disagree", "Disagree", "Slightly disagree",
                                                          "Neutral", "Slightly Agree", "Agree", "Strongly Agree"))
            }



# Generate the likert objects prepared to plot

    general_impacts <- colnames(fct_survey)[5:10]
    gen_imp <- fct_survey[general_impacts]
    colnames(gen_imp) <- mf_label_number_complete(colnames(gen_imp))
    gen_imp_rev <- rev(names(gen_imp))
    likert_gen_imp <- likert(gen_imp)
    likert_gen_imp$results [,2:8] <- round(likert_gen_imp$results[,2:8], 1)
    likert_gen_imp$results$Item <- factor(likert_gen_imp$results$Item,
                                          levels= rev(likert_gen_imp$results$Item))

    
    forest_impacts <- colnames(fct_survey)[11:17]
    for_imp <- fct_survey[forest_impacts]
    colnames(for_imp) <- mf_label_number_complete(colnames(for_imp))
    for_imp_rev <- rev(names(for_imp))
    likert_for_imp <- likert(for_imp)
    likert_for_imp$results [,2:8]<- round(likert_for_imp$results[,2:8], 1)
    likert_for_imp$results$Item = with(likert_for_imp$results, 
                                       factor(Item, levels = rev(levels(Item))))
  
    
    current_practices <- colnames(fct_survey)[18:22]
    current <- fct_survey[current_practices]
    colnames(current) <- mf_label_number(colnames(current))
    likert_current <- likert(current)
    likert_current$results [,2:8]<- round(likert_current$results[,2:8], 1)
    likert_current$results$Item = with(likert_current$results, 
                                       factor(Item, levels = rev(levels(Item))))
  
    
    ecosystem_management <- colnames(fct_survey)[23:28]
    ecosystem <- fct_survey[ecosystem_management]
    colnames(ecosystem) <- mf_label_number(colnames(ecosystem))
    likert_ecosys <- likert(ecosystem)
    likert_ecosys$results [,2:8]<- round(likert_ecosys$results[,2:8], 1)
    likert_ecosys$results$Item = with(likert_ecosys$results,
                                      factor(Item, levels = rev(levels(Item))))
  
    new_nep <- colnames(fct_survey)[73:87]
    nep <- fct_survey[new_nep]
    colnames(nep) <- mf_label_number(colnames(nep))
    likert_nep <- likert(nep)
    likert_nep$results [,2:8]<- round(likert_nep$results[,2:8], 1)
    likert_nep$results$Item = with(likert_nep$results, 
                                   factor(Item, levels = rev(levels(Item))))
  
    awareness <- colnames(fct_survey)[29:31]
    aware <- fct_survey[awareness]
    colnames(aware) <- mf_label_number(colnames(aware))
    likert_aware <- likert(aware)
    likert_aware$results [,2:4]<- round(likert_aware$results[,2:4], 1)
    likert_aware$results$Item = with(likert_aware$results, 
                                     factor(Item, levels = rev(levels(Item))))
  



  