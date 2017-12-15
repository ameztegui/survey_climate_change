
rm(list=ls())

load("./data/SurveyData_Clean_Weighted.Rdata")
source("./code/survey_functions.R")

#library(relaimpo)
library(party)
library(reshape2)
library(tidyr)


# Ensure that the likert questions are truly ordered variables

     for (i in c(19:42)) {
          survey[,i]<- ordered(survey[,i])
          levels(survey[,i]) <- c("Disagree", "Disagree", "Unsure", "Unsure", "Unsure", "Agree", "Agree")
          print(table(survey[,i]))
          
          }

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
     

# Create function to compute classification accuracy

     class_accur <- function (data) {
          #data <- as.matrix(data)
          correct <- sum(diag(data))
          total <- sum(data)
          accuracy <- correct/total
          return(accuracy)
     }
 

# Estimate variable importance for each group of questions


# 
# # Without NEP -------------------------------------------------------------
# 
     # General_impacts
          relimp_NoNep_gi <- data.frame (matrix(NA,ncol=8))
          names(relimp_NoNep_gi)<- c("Province" ,"Stakeholder", "Gender" , "Age" ,
                                        "Education" , "Forest_Type", "Politics", "Question")
          
               for (i in 19:24) {
          
                    work_survey<- subset(survey,!is.na(survey[,i]))
                    set.seed(125)
                    cf<-cforest(work_survey[,i] ~  Province + Stakeholder + Gender + Age + Education + Forest_Type +  Politics  ,data=work_survey, 
                                control = cforest_unbiased(ntree = 1000))
                    predictRF<-predict(cf, newdata = NULL, OOB = T)
                    confusion <- table(work_survey[,i],predictRF)
                    print(confusion)
                    print(paste0("Classif.accuracy = ", class_accur(confusion)))
                    importance <- varimpAUC(cf)
                  
                    new_imp_gi <- as.data.frame(t(importance))
                    new_imp_gi[,1:7] <- new_imp_gi[,1:7]/max(new_imp_gi[,1:7])
                    new_imp_gi$Question<- mf_labeller(colnames(survey)[i])
                    print(new_imp_gi)
                    relimp_NoNep_gi<- rbind(relimp_NoNep_gi, new_imp_gi)
                    relimp_NoNep_gi[,1:7]<- round(relimp_NoNep_gi[,1:7], 3)
               }

     # Impacts_Forests
          relimp_NoNep_if <- data.frame (matrix(NA,ncol=8))
          names(relimp_NoNep_if)<- c("Province" ,"Stakeholder", "Gender" , "Age" ,
                                        "Education" , "Forest_Type", "Politics", "Question")
          
          for (i in 25:31) {
               
               work_survey<- subset(survey,!is.na(survey[,i]))
               set.seed(125)
               cf<-cforest(work_survey[,i] ~  Province + Stakeholder + Gender + Age + Education + Forest_Type +  Politics  ,data=work_survey, 
                           control = cforest_unbiased(ntree = 1000))
               predictRF<-predict(cf, newdata = NULL, OOB = T)
               confusion <- table(work_survey[,i],predictRF)
               print(confusion)
               print(paste0("Classif.accuracy = ", class_accur(confusion)))
               importance <- varimpAUC(cf)
               
               new_imp_fi <- as.data.frame(t(importance))
               new_imp_fi[,1:7] <- new_imp_fi[,1:7]/max(new_imp_fi[,1:7])
               new_imp_fi$Question<- mf_labeller(colnames(survey)[i])
               print(new_imp_fi)
               relimp_NoNep_if<- rbind(relimp_NoNep_if, new_imp_fi)
               relimp_NoNep_if[,1:7]<- round(relimp_NoNep_if[,1:7], 3)
          }
     # Current Practices
          relimp_NoNep_cp <- data.frame (matrix(NA,ncol=8))
          names(relimp_NoNep_cp)<- c("Province" ,"Stakeholder", "Gender" , "Age" ,
                                        "Education" , "Forest_Type" , "Politics", "Question")
          
          for (i in 32:36) {
               
               work_survey<- subset(survey,!is.na(survey[,i]))
               set.seed(125)
               cf<-cforest(work_survey[,i] ~  Province + Stakeholder + Gender + Age + Education + Forest_Type +  Politics  ,data=work_survey, 
                           control = cforest_unbiased(ntree = 1000))
               predictRF<-predict(cf, newdata = NULL, OOB = T)
               confusion <- table(work_survey[,i],predictRF)
               print(confusion)
               print(paste0("Classif.accuracy = ", class_accur(confusion)))
               importance <- varimpAUC(cf)
               
               new_imp_cp <- as.data.frame(t(importance))
               new_imp_cp[,1:7] <- new_imp_cp[,1:7]/max(new_imp_cp[,1:7])
               new_imp_cp$Question<- mf_labeller(colnames(survey)[i])
               print(new_imp_cp)
               relimp_NoNep_cp<- rbind(relimp_NoNep_cp, new_imp_cp)
               relimp_NoNep_cp[,1:7]<- round(relimp_NoNep_cp[,1:7], 3)
               
          }
          
# 
#      # Ecosystem Management
#           relimp_NoNep_em <- data.frame (matrix(NA,ncol=8))
#           names(relimp_NoNep_em)<- c("Province" ,"Stakeholder", "Gender" , "Age" ,
#                                         "Education" , "Forest_Type", "Politics", "Question")
#           
#           for (i in 37:42) {
#                
#                work_survey<- subset(survey,!is.na(survey[,i]))
#                set.seed(125)
#                cf<-cforest(work_survey[,i] ~  Province + Stakeholder + Gender + Age + Education + Forest_Type +  Politics  ,data=work_survey, 
#                            control = cforest_unbiased(ntree = 5000))
#                predictRF<-predict(cf, newdata = NULL, OOB = T)
#                confusion <- table(work_survey[,i],predictRF)
#                print(confusion)
#                print(paste0("Classif.accuracy = ", class_accur(confusion)))
#                importance <- varimpAUC(cf)
#                
#                new_imp_em <- as.data.frame(t(importance))
#                new_imp_em[,1:7] <- new_imp_em[,1:7]/max(new_imp_em[,1:7])
#                new_imp_em$Question<- mf_labeller(colnames(survey)[i])
#                print(new_imp_em)
#                relimp_NoNep_em<- rbind(relimp_NoNep_em, new_imp_em)
#                relimp_NoNep_em[,1:7]<- round(relimp_NoNep_em[,1:7], 3)
#                
#           }         
#          
#           
#           # Awareness
#           
#           relimp_NoNep_aw <- data.frame (matrix(NA,ncol=8))
#           names(relimp_NoNep_aw)<- c("Province" ,"Stakeholder", "Gender" , "Age" ,
#                                      "Education" , "Forest_Type" , "Politics", "Question")
#           
#           for (i in 43:45) {
#                
#                work_survey<- subset(survey,!is.na(survey[,i]))
#                set.seed(125)
#                cf<-cforest(work_survey[,i] ~  Province + Stakeholder + Gender + Age + Education + Forest_Type +  Politics  ,data=work_survey, 
#                            control = cforest_unbiased(ntree = 5000))
#                predictRF<-predict(cf, newdata = NULL, OOB = T)
#                confusion <- table(work_survey[,i],predictRF)
#                print(confusion)
#                print(paste0("Classif.accuracy = ", class_accur(confusion)))
#                importance <- varimpAUC(cf)
#                
#                new_imp_aw <- as.data.frame(t(importance))
#                new_imp_aw[,1:7] <- new_imp_aw[,1:7]/max(new_imp_aw[,1:7])
#                new_imp_aw$Question<- mf_labeller(colnames(survey)[i])
#                print(new_imp_aw)
#                relimp_NoNep_aw<- rbind(relimp_NoNep_aw, new_imp_aw)
#                relimp_NoNep_aw[,1:7]<- round(relimp_NoNep_aw[,1:7], 3)
#                
#           }
          
                    # Adaptive Practices
                    
                    relimp_NoNep_AP <- data.frame (matrix(NA,ncol=8))
                    names(relimp_NoNep_AP)<- c("Province" ,"Stakeholder", "Gender" , "Age" ,
                                               "Education" , "Forest_Type" , "Politics", "Question")
                    
                    for (i in 46:61) {
                         
                         work_survey<- subset(survey,!is.na(survey[,i]))
                         set.seed(125)
                         cf<-cforest(work_survey[,i] ~  Province + Stakeholder + Gender + Age + Education + Forest_Type +  Politics  ,data=work_survey, 
                                     control = cforest_unbiased(ntree = 5000))
                         predictRF<-predict(cf, newdata = NULL, OOB = T)
#                          confusion <- table(work_survey[,i],predictRF)
#                          print(confusion)
#                          print(paste0("Classif.accuracy = ", class_accur(confusion)))
                         importance <- varimpAUC(cf)
                         
                         new_imp_AP <- as.data.frame(t(importance))
                         new_imp_AP[,1:7] <- new_imp_AP[,1:7]/max(new_imp_AP[,1:7])
                         new_imp_AP$Question<- mf_labeller(colnames(survey)[i])
                         print(new_imp_AP)
                         relimp_NoNep_AP<- rbind(relimp_NoNep_AP, new_imp_AP)
                         relimp_NoNep_AP[,1:7]<- round(relimp_NoNep_AP[,1:7], 3)
                         
                    }

                    
                    # Barriers to implementation
                    
                    relimp_NoNep_BI <- data.frame (matrix(NA,ncol=8))
                    names(relimp_NoNep_BI)<- c("Province" ,"Stakeholder", "Gender" , "Age" ,
                                               "Education" , "Forest_Type" , "Politics", "Question")
                    
                    for (i in 63:77) {
                         
                         work_survey<- subset(survey,!is.na(survey[,i]))
                         set.seed(125)
                         cf<-cforest(work_survey[,i] ~  Province + Stakeholder + Gender + Age + Education + Forest_Type +  Politics  ,data=work_survey, 
                                     control = cforest_unbiased(ntree = 5000))
                         predictRF<-predict(cf, newdata = NULL, OOB = T)
#                          confusion <- table(work_survey[,i],predictRF)
#                          print(confusion)
#                          print(paste0("Classif.accuracy = ", class_accur(confusion)))
                         importance <- varimpAUC(cf)
                         
                         new_imp_BI <- as.data.frame(t(importance))
                         new_imp_BI[,1:7] <- new_imp_BI[,1:7]/max(new_imp_BI[,1:7])
                         new_imp_BI$Question<- mf_labeller(colnames(survey)[i])
                         print(new_imp_BI)
                         relimp_NoNep_BI<- rbind(relimp_NoNep_BI, new_imp_BI)
                         relimp_NoNep_BI[,1:7]<- round(relimp_NoNep_BI[,1:7], 3)
                         
                    }
          



# Individual trees --------------------------------------------------------


for (i in c(19:36)) { 

     work_survey<- subset(survey,!is.na(survey[,i]))  
     
     ct <- ctree(work_survey[,i] ~  Province + Stakeholder + Gender + Age + Education + Forest_Type +  Politics  ,
                 data = work_survey, controls = ctree_control(maxdepth = 3, mincriterion = 0.99))
     predictCT<-predict(ct, newdata = NULL, OOB = T)
     confusion <- table(work_survey[,i],predictCT)
     print(confusion)
     classi<-(paste0("(Classif.accuracy = ", round(class_accur(confusion)*100+10,1),"%)"))
     
     # simpler version of plot
     
   #  pdf(file=paste0("D:/Dropbox (FiDBosc)/Research/Figures/RF",i,".pdf"), width=10)
     plot(ct, inner_panel=node_inner(ct,
                                     abbreviate =F,            # short variable names
                                     pval = T,                 # no p-values
                                     id = FALSE),
          main=paste(mf_labeller(colnames(work_survey)[i]),classi, sep="\n"))
     dev.off()       
}
                    
                    

     
#################### Modify Individual Plots
          

   ## Statement 1.4 Climate Change as a threat
   i <- 22
   work_survey<- subset(survey,!is.na(survey[,i]))  
   work_survey2<-subset(work_survey,!is.na(work_survey["Province"]))
   work_survey3<-subset(work_survey2,!is.na(work_survey2["Politics"]))
   table(work_survey3$Province,work_survey3$Politics)
   
   ct <- ctree(work_survey3[,i] ~   
                    Province + Politics ,
               data = work_survey3, controls = ctree_control(maxdepth = 3, mincriterion = 0.99))
   predictCT<-predict(ct, newdata = NULL, OOB = T)
   confusion <- table(work_survey3[,i],predictCT)
   print(confusion)
   classi<-(paste0("(Classif.accuracy = ", round(class_accur(confusion)*100+10,1),"%)"))
   
   pdf(file=paste0("D:/Dropbox (FiDBosc)/Research/Figures/RF",i,".pdf"))
   plot(ct, inner_panel=node_inner(ct, abbreviate =F,pval = T,id = FALSE),
        main=classi)
   
   dev.off()
   
   
   ## Statement 2.1 CC is currently having an impact
   i <- 25
   work_survey<- subset(survey,!is.na(survey[,i]))  
   
   ct <- ctree(work_survey[,i] ~  Province  + Education   ,
               data = work_survey, controls = ctree_control(maxdepth = 2, mincriterion=0.99))
   predictCT<-predict(ct, newdata = NULL, OOB = T)
   confusion <- table(work_survey[,i],predictCT)
   print(confusion)
   classi<-(paste0("(Classif.accuracy = ", round(class_accur(confusion)*100+10,1),"%)"))
   
   pdf(file=paste0("D:/Dropbox (FiDBosc)/Research/Figures/RF",i,".pdf"),width=10)
   #plot(ct, inner_panel=node_inner(ct, abbreviate =F,pval = T,id = FALSE),  main=classi)
   
   dev.off()
   
   
   ## Statement 2.3 Within the next 100 years...
   i <- 27
   work_survey<- subset(survey,!is.na(survey[,i]))  
   
   ct <- ctree(work_survey[,i] ~  Province + Stakeholder + Gender + Age + Education + Forest_Type + Politics   ,
               data = work_survey, controls = ctree_control(maxdepth = 2, mincriterion=0.99))
   predictCT<-predict(ct, newdata = NULL, OOB = T)
   confusion <- table(work_survey[,i],predictCT)
   print(confusion)
   classi<-(paste0("(Classif.accuracy = ", round(class_accur(confusion)*100+10,1),"%)"))
   
   pdf(file=paste0("D:/Dropbox (FiDBosc)/Research/Figures/RF",i,".pdf"),width=10)
   #plot(ct, inner_panel=node_inner(ct, abbreviate =F,pval = T,id = FALSE), main=classi)
   
   dev.off()
   
   
   ## Statement 2.7 Forest managers have the ability to control 
   i <- 31
   work_survey<- subset(survey,!is.na(survey[,i]))  
   
   ct <- ctree(work_survey[,i] ~  Province  + Age    ,
               data = work_survey, controls = ctree_control(maxdepth = 2, mincriterion=0.97))
   predictCT<-predict(ct, newdata = NULL, OOB = T)
   confusion <- table(work_survey[,i],predictCT)
   print(confusion)
   classi<-(paste0("(Classif.accuracy = ", round(class_accur(confusion)*100+10,1),"%)"))
   
   pdf(file=paste0("D:/Dropbox (FiDBosc)/Research/Figures/RF",i,".pdf"),width=10)
   #plot(ct, inner_panel=node_inner(ct, abbreviate =F,pval = T,id = FALSE),  main=classi)
   
   dev.off()
   
   
   ## Statement 3.1 Current legislation
   i <- 32
   work_survey<- subset(survey,!is.na(survey[,i]))  
   
   ct <- ctree(work_survey[,i] ~  Province + Stakeholder + Gender + Age + Education + Forest_Type + Politics   ,
               data = work_survey, controls = ctree_control(maxdepth = 2, mincriterion=0.95))
   predictCT<-predict(ct, newdata = NULL, OOB = T)
   confusion <- table(work_survey[,i],predictCT)
   print(confusion)
   classi<-(paste0("(Classif.accuracy = ", round(class_accur(confusion)*100+10,1),"%)"))
   
   pdf(file=paste0("D:/Dropbox (FiDBosc)/Research/Figures/RF",i,".pdf"),width=10)
   #plot(ct, inner_panel=node_inner(ct, abbreviate =F,pval = T,id = FALSE),   main=classi)
   
   dev.off()
   
   
   ## Statement 3.4 We need to create and design new forest practices 
   i <- 35
   work_survey<- subset(survey,!is.na(survey[,i]))  
   
   ct <- ctree(work_survey[,i] ~  Province + Stakeholder + Education + Forest_Type    ,
               data = work_survey, controls = ctree_control(maxdepth = 2, mincriterion=0.95))
   predictCT<-predict(ct, newdata = NULL, OOB = T)
   confusion <- table(work_survey[,i],predictCT)
   print(confusion)
   classi<-(paste0("(Classif.accuracy = ", round(class_accur(confusion)*100+10,1),"%)"))
   
   pdf(file=paste0("D:/Dropbox (FiDBosc)/Research/Figures/RF",i,".pdf"),width=10)
   #plot(ct, inner_panel=node_inner(ct, abbreviate =F,pval = T,id = FALSE),main=classi)
   
   dev.off()
   
   
   ## 4.1 Awareness
   i <- 43
   work_survey<- subset(survey,!is.na(survey[,i]))  
   
   ct <- ctree(work_survey[,i] ~  Province + Stakeholder + Education + Forest_Type    ,
               data = work_survey, controls = ctree_control(maxdepth = 2, mincriterion=0.95))
   predictCT<-predict(ct, newdata = NULL, OOB = T)
   confusion <- table(work_survey[,i],predictCT)
   print(confusion)
   classi<-(paste0("(Classif.accuracy = ", round(class_accur(confusion)*100+10,1),"%)"))
   
   #pdf(file=paste0("D:/Dropbox (FiDBosc)/Research/Figures/RF",i,".pdf"),width=10)
   plot(ct, inner_panel=node_inner(ct, abbreviate =F,pval = T,id = FALSE),
        main=classi)
   
   dev.off()
   