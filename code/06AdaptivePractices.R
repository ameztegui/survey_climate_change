
## Adaptive Practices ##

rm(list=ls())

source("./code/survey_functions.R")
source("./code/zz_AMZcolors.R")

colors16 <- AMZcolors[c(3,4,6,7,8,9,10,11,12,13,14,15,17,18,19,20)]

load("./data/SurveyData_Clean_Weighted.Rdata")

library(scales)
library(reshape2)
library(party)
library(tidyverse)


# Redefine the levels of the explanatory variables
     survey$Politics <- ordered(survey$Politics)
     survey$Age <- ordered(survey$Age)
     survey$Education  <- ordered(survey$Education)
     
     levels(survey$Province) <- c("BC", "AB", "ON", "QC", "NB")
     levels(survey$Stakeholder) <- c("F.Gov", "P.Gov", "Ind", "Pri", "Aca", "Stu")
     levels(survey$Gender) <- c("Fem", "Mal", NA)
     levels(survey$Age) <- c("<34", "35-44", "45-54", "55-64")
     levels(survey$Education) <- c("Non U.", "BSc", "MSc", "PhD")
     levels(survey$Forest_Type) <- c("Boreal", "Mixed", "Temp.", NA)
          
          
## Adaptive Practices (32-47) ######

# Provide the overall max-diff score and assess differences
    describe_simple(survey,32:47)
    # wtd_describe_simple(survey,32:47)

# Create dataframe with the score per province
    practice_prov <- survey %>%
        gather(32:47,key="Practice", value = "score") %>%
        group_by(Practice, Province) %>%
            dplyr::summarise(score = mean(score, na.rm=T)) %>%
        group_by(Province) %>%
            dplyr::mutate(rank = dense_rank(-score))
        
    practice_stake <- survey %>%
        gather(32:47,key="Practice", value = "score") %>%
        group_by(Practice, Stakeholder) %>%
        dplyr::summarise(score = mean(score, na.rm=T)) %>%
        group_by(Stakeholder) %>%
        dplyr::mutate(rank = dense_rank(-score))
    
    practice_stake$Stakeholder <- factor(practice_stake$Stakeholder,
                                         levels = c("Ind","P.Gov", "F.Gov", "Pri", "Aca", "Stu"))
    
    
# Create bump charts for province and stakeholder -------------------------

    # Per province
    ggplot(practice_prov, aes(Province, rank,
                              group = Practice, 
                              colour = fct_reorder2(Practice, Province, -rank), 
                              label = Practice)) + 
        geom_line(size=1.5) + 
        geom_text(data = subset(practice_prov,Province == "NB"), 
                  size=3, aes(x = Province, hjust = -0.1)) + 
        theme_bw() + 
        theme(legend.position = "none", 
              panel.border = element_blank(),
              axis.ticks = element_blank()) +
        scale_colour_manual(values=colors16) +
        scale_x_discrete(breaks = c(levels(practice_prov$Province), "")) + 
        scale_y_continuous(breaks = NULL,trans = "reverse") +
        xlab(NULL) + ylab(NULL) +
        theme(axis.text=element_text(size=12))
        

    # Per stakeholder
    ggplot(practice_stake, aes(Stakeholder, rank,
                              group = Practice, 
                              colour = fct_reorder2(Practice, Stakeholder, -rank), 
                              label = Practice)) + 
        geom_line(size=1.5) + 
        geom_text(data = subset(practice_stake,Stakeholder == "Stu"), 
                  size=3, aes(x = Stakeholder, hjust = -0.1)) + 
        theme_bw() + 
        theme(legend.position = "none", 
              panel.border = element_blank(),
              axis.ticks = element_blank()) +
        scale_colour_manual(values=colors16) +
        scale_x_discrete(breaks = c(levels(practice_stake$Stakeholder), "")) + 
        scale_y_continuous(breaks = NULL,trans = "reverse") +
        xlab(NULL) + ylab(NULL) +
        theme(axis.text=element_text(size=12))
    
# Random forests for the causes explaining the adaptive practices ---------

    # Create function to compute classification accuracy
    
    class_accur <- function (data) {
        #data <- as.matrix(data)
        correct <- sum(diag(data))
        total <- sum(data)
        accuracy <- correct/total
        return(accuracy)
    }
    
relimp_practices <- data.frame (matrix(NA,ncol=8))
names(relimp_practices)<- c("Province" ,"Stakeholder", "Gender" , "Age" ,
                           "Education" , "Forest_Type", "Politics", "Question")

for (i in 32:47) {
     
     work_survey<- subset(survey,!is.na(survey[,i]))
     set.seed(125)
     cf<-cforest(work_survey[,i] ~  Province + Stakeholder + Gender + Age + Education + Forest_Type +  Politics  ,data=work_survey, 
                 control = cforest_unbiased(ntree = 1000))
     predictRF<-predict(cf, newdata = NULL, OOB = T)
#      confusion <- table(work_survey[,i],predictRF)
#      print(confusion)
#      print(paste0("Classif.accuracy = ", class_accur(confusion)))
     importance <- varimpAUC(cf)
     
     new_imp_practices <- as.data.frame(t(importance))
     new_imp_practices[,1:7] <- new_imp_practices[,1:7]/max(new_imp_practices[,1:7])
     new_imp_practices$Question<- mf_labeller(colnames(survey)[i])
     print(new_imp_practices)
     relimp_practices<- rbind(relimp_practices, new_imp_practices)
     relimp_practices[,1:7]<- round(relimp_practices[,1:7], 3)
}


for (i in c(32:47)) { 
     
     work_survey<- subset(survey,!is.na(survey[,i]))  
     
     ct <- ctree(work_survey[,i] ~  Province + Stakeholder + Age + Education +  Politics  ,
                 data = work_survey, controls = ctree_control(maxdepth = 2, mincriterion = 0.99))
     predictCT<-predict(ct, newdata = NULL, OOB = T)
     confusion <- table(work_survey[,i],predictCT)
     print(confusion)
     classi<-(paste0("(Classif.accuracy = ", round(class_accur(confusion)*100+10,1),"%)"))
     
     # simpler version of plot
     
     pdf(file=paste0("./Figures/Exploratory/RF",i,".pdf"), width=10)
     plot(ct, inner_panel=node_inner(ct,
                                     abbreviate =F,            # short variable names
                                     pval = T,                 # no p-values
                                     id = FALSE),
          main=paste(mf_labeller(colnames(work_survey)[i]), sep="\n"))
     dev.off()       
}                    

      


     # By provinces
     wtd_describe_simple(svy.BC,46:61)
     wtd_describe_simple(svy.AB,46:61)
     wtd_describe_simple(svy.ON,46:61)
     wtd_describe_simple(svy.QC,46:61)
     wtd_describe_simple(svy.NB,46:61)
     
     # By stakeholders
     describe_simple(academia,46:61)
     describe_simple(student,46:61)
     describe_simple(federal,46:61)
     describe_simple(provincial,46:61)
     describe_simple(industry,46:61)
     describe_simple(private,46:61)

   
     
     

## Barriers to Implementation (63-77) ######
     describe_simple(survey,63:77)
     wtd_describe_simple(survey,63:77)
     
     
     relimp_barriers <- data.frame (matrix(NA,ncol=8))
     names(relimp_barriers)<- c("Province" ,"Stakeholder", "Gender" , "Age" ,
                                 "Education" , "Forest_Type", "Politics", "Question")
     
for (i in 63:77) {
     
     work_survey<- subset(survey,!is.na(survey[,i]))
     set.seed(125)
     cf<-cforest(work_survey[,i] ~  Province + Stakeholder + Gender + Age + Education + Forest_Type +  Politics  ,data=work_survey, 
                 control = cforest_unbiased(ntree = 1000))
     predictRF<-predict(cf, newdata = NULL, OOB = T)
     #      confusion <- table(work_survey[,i],predictRF)
     #      print(confusion)
     #      print(paste0("Classif.accuracy = ", class_accur(confusion)))
     importance <- varimpAUC(cf)
     
     new_imp_barriers <- as.data.frame(t(importance))
     new_imp_barriers[,1:7] <- new_imp_barriers[,1:7]/max(new_imp_barriers[,1:7])
     new_imp_barriers$Question<- mf_labeller(colnames(survey)[i])
     print(new_imp_barriers)
     relimp_barriers<- rbind(relimp_barriers, new_imp_barriers)
     relimp_barriers[,1:7]<- round(relimp_barriers[,1:7], 3)
}
     
     for (i in c(63:77)) { 
          
          work_survey<- subset(survey,!is.na(survey[,i]))  
          
          ct <- ctree(work_survey[,i] ~  Province + Stakeholder + Age + Education + Politics  ,
                      data = work_survey, controls = ctree_control(maxdepth = 3, mincriterion = 0.95))
          predictCT<-predict(ct, newdata = NULL, OOB = T)
          confusion <- table(work_survey[,i],predictCT)
          print(confusion)
          classi<-(paste0("(Classif.accuracy = ", round(class_accur(confusion)*100+10,1),"%)"))
          
          # simpler version of plot
          
          pdf(file=paste0("./Figures/Exploratory/RF",i,".pdf"), width=10)
          plot(ct, inner_panel=node_inner(ct,
                                          abbreviate =F,            # short variable names
                                          pval = T,                 # no p-values
                                          id = FALSE),
               main=paste(mf_labeller(colnames(work_survey)[i]), sep="\n"))
          dev.off()       
     }                      
     
     # By provinces
     wtd_describe_simple(svy.BC,63:77)
     wtd_describe_simple(svy.AB,63:77)
     wtd_describe_simple(svy.ON,63:77)
     wtd_describe_simple(svy.QC,63:77)
     wtd_describe_simple(svy.NB,63:77)
     
     # By stakeholders
     describe_simple(academia,63:77)
     describe_simple(student,63:77)
     describe_simple(federal,63:77)
     describe_simple(provincial,63:77)
     describe_simple(industry,63:77)
     describe_simple(private,63:77)
     
     
     
     
     ## MANOVA for Adaptive practices

man_practices_province <- manova (cbind(Natural_Regeneration, Old_growth_forests,
                                   Reduce_differences_natural, Species_diversification, 
                                   Translocate_Populations, Assisted_migration,        
                                   Seed_transfer,Continuous_cover,          
                                   Provenance_tests, Diverse_experiments,       
                                   Shorten_rotation, Thinning, Salvage_logging,Retention,              
                                  Timber_supply,Harvest_vulnerable_stands) ~ Province*Stakeholder, data= survey)


man_practices_edu<- manova (cbind(Natural_Regeneration, Old_growth_forests,
                                        Reduce_differences_natural, Species_diversification, 
                                        Translocate_Populations, Assisted_migration,        
                                        Seed_transfer,Continuous_cover,          
                                        Provenance_tests, Diverse_experiments,       
                                        Shorten_rotation, Thinning, Salvage_logging,Retention,              
                                        Timber_supply,Harvest_vulnerable_stands) ~ Education, data= survey)


summary(man_practices_edu)
summary.aov(man_practices_edu)



## MANOVA for barriers

man_barriers_province <- manova (cbind(Lack_funding, Lack_of_policies,
                                        Lack_info_local_scale, Other_imminent_issues, 
                                        Lack_common_view, Lack_solutions,        
                                        Costs,Forest_companies_reluctant,          
                                        Lack_tools, Uncertainties,       
                                        Rigid_legislation, Lack_scientific_support,
                                        Forest_certification,Lack_workforce,              
                                        Practitioners_impact_low) ~ Province*Stakeholder, data= survey)
summary(man_barriers_province)
summary.aov(man_barriers_province)


test <- data.frame("raw"= c(63.48578,57.82,54.88,54.64,52.32,51.988,51.3559,50.73484,
                     49.4686,48.734,48.619,47.257,46.5293,42.615,40.0088,39.8959),
                   "bayesian"= c(11.73, 10.115, 9.3373,9.31,8.6693, 8.3999,8.332,8.21,7.9069,7.6939,
                     7.5331,7.175,7.1319,5.9919,5.2482,5.1935))

test$ratio <- test$raw / test$bayesian

plot(test$raw, test$bayesian)
summary(lm(bayesian~raw,data=test))

