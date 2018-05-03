rm(list=ls())

load("./data/SurveyData_Clean_Weighted.Rdata")
source("./code/survey_functions.R")

library(likert)
library(tidyverse)

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
# 
#     ## Test con stakeholder por provincia -----
#     
#     west <- survey %>% filter(!Province %in% c("British Columbia", "Alberta"))
#     fct_west <- fct_survey %>% filter(!Province %in% c("British Columbia", "Alberta"))
#     
#     
#     # General Impacts
#     general_impacts <- colnames(fct_west)[5:10]
#     gen_imp <- fct_west[general_impacts]
#     colnames(gen_imp) <- mf_label_number_complete(colnames(gen_imp))
#     gen_imp_rev <- rev(names(gen_imp))
#     likert_gen_imp <- likert(gen_imp)
#     likert_gen_imp$results [,2:8] <- round(likert_gen_imp$results[,2:8], 1)
#     likert_gen_imp$results$Item <- factor(likert_gen_imp$results$Item,
#                                           levels= rev(likert_gen_imp$results$Item))
#     
#     letters_gen_imp_west <- mean_likert(west, grouping = "Stakeholder", 5:10) %>%
#         arrange(Stakeholder)
#     
#     gen_imp_stake <- likert(gen_imp, grouping = west$Stakeholder) 
#     gen_imp_stake$results$Group <- factor (gen_imp_stake$results$Group, 
#                                            levels = rev(levels(gen_imp_stake$results$Group)))
#     colnames(gen_imp_stake$results) <- c("Group","Item", "Strongly\n disagree", "Disagree","Slightly\n disagree",
#                                          "Neutral", "Slightly\n agree", "Agree", "Strongly\n agree")
#     
#     
#     plot(gen_imp_stake, ordered=F,text.size=3,plot.percents=T, wrap=100) +
#     theme(axis.text.y = element_text(size = 9),
#           axis.text.x = element_text (size = 9),
#           strip.text = element_text(size = 8, face="bold", hjust=0),
#           strip.background = element_rect (fill= "white", colour="white"),
#           legend.text = element_text(size = 8)) +
#         guides(fill=guide_legend("",nrow=1)) +
#         annotate("text", x=6, y =-98, size=3, fontface=1,hjust=0,
#                  label=unlist(letters_gen_imp_west[1,-1])) + #Federal
#         annotate("text", x=5, y =-98, size=3, fontface=1,hjust=0,
#                  label=unlist(letters_gen_imp_west[2, -1])) + #Provincial
#         annotate("text", x=4, y =-98, size=3, fontface=1,hjust=0,
#                  label=unlist(letters_gen_imp_west[3, -1])) + # Industry
#         annotate("text", x=3, y =-98, size=3, fontface=1,hjust=0,
#                  label=unlist(letters_gen_imp_west[4, -1])) + # Private
#         annotate("text", x=2, y =-98, size=3, fontface=1,hjust=0,
#                  label=unlist(letters_gen_imp_west[5, -1])) + # Academia
#         annotate("text", x=1, y =-98, size=3, fontface=1,hjust=0,
#                  label=unlist(letters_gen_imp_west[6, -1]))# Students
#     
#     # Forest Impacts
#     forest_impacts <- colnames(fct_west)[11:17]
#     for_imp <- fct_west[forest_impacts]
#     colnames(for_imp) <- mf_label_number_complete(colnames(for_imp))
#     for_imp_rev <- rev(names(for_imp))
#     likert_for_imp <- likert(for_imp)
#     likert_for_imp$results [,2:8] <- round(likert_for_imp$results[,2:8], 1)
#     likert_for_imp$results$Item <- factor(likert_for_imp$results$Item,
#                                           levels= rev(likert_for_imp$results$Item))
#     
#     letters_for_imp_west <- mean_likert(west, grouping = "Stakeholder", 11:17) %>%
#         arrange(Stakeholder)
#     
#     for_imp_stake <- likert(for_imp, grouping = west$Stakeholder) 
#     for_imp_stake$results$Group <- factor (for_imp_stake$results$Group, 
#                                            levels = rev(levels(for_imp_stake$results$Group)))
#     colnames(for_imp_stake$results) <- c("Group","Item", "Strongly\n disagree", "Disagree","Slightly\n disagree",
#                                          "Neutral", "Slightly\n agree", "Agree", "Strongly\n agree")
#     
#     
#     plot(for_imp_stake, ordered=F,text.size=3,plot.percents=T, wrap=100) +
#         theme(axis.text.y = element_text(size = 9),
#               axis.text.x = element_text (size = 9),
#               strip.text = element_text(size = 8, face="bold", hjust=0),
#               strip.background = element_rect (fill= "white", colour="white"),
#               legend.text = element_text(size = 8)) +
#         guides(fill=guide_legend("",nrow=1)) +
#         annotate("text", x=6, y =-98, size=3, fontface=1,hjust=0,
#                  label=unlist(letters_for_imp_west[1,-1])) + #Federal
#         annotate("text", x=5, y =-98, size=3, fontface=1,hjust=0,
#                  label=unlist(letters_for_imp_west[2, -1])) + #Provincial
#         annotate("text", x=4, y =-98, size=3, fontface=1,hjust=0,
#                  label=unlist(letters_for_imp_west[3, -1])) + # Industry
#         annotate("text", x=3, y =-98, size=3, fontface=1,hjust=0,
#                  label=unlist(letters_for_imp_west[4, -1])) + # Private
#         annotate("text", x=2, y =-98, size=3, fontface=1,hjust=0,
#                  label=unlist(letters_for_imp_west[5, -1])) + # Academia
#         annotate("text", x=1, y =-98, size=3, fontface=1,hjust=0,
#                  label=unlist(letters_for_imp_west[6, -1]))# Students
#     


  