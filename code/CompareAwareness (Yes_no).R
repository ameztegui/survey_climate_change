rm(list=ls())

load("./data/SurveyData_Clean_Weighted.Rdata")
source("./code/survey_functions.R")
source("./code/02_Likert.R")




## By province ####
     
     pdf(file="./Figures/Exploratory/YesNo/01Province_yes_Awareness.pdf", width=6.5, height=4.5)
          wtd_chisq (dataframe=survey, dep="Awareness", indep="Province", "n") +
          annotate("text",x=c(1,2,3,4,5),y=1.05,ymax=1.10,label=c("b","b","b","b","a"),size=4) 
          dev.off()     
          
     
     pdf(file="./Figures/Exploratory/YesNo/01Province_yes_Consider.pdf", width=6.5, height=4.5)
          wtd_chisq (dataframe=survey, dep="Consider", indep="Province","n") +
          annotate("text",x=c(1,2,3,4,5),y=1.05,ymax=1.10,label=c("b","a","a","b","a"),size=4) 
          dev.off()   
     
     pdf(file="./Figures/Exploratory/YesNo/01Province_yes_Priority.pdf", width=6.5, height=4.5)
          wtd_chisq (dataframe=survey, dep="Priority", indep="Province","n") +
          annotate("text",x=c(1,2,3,4,5),y=1.05,ymax=1.10,label=c("b","b","b","b","a"),size=4)
          dev.off()   

## By Stakeholder ####

     pdf(file="./Figures/Exploratory/YesNo/02Stakeholder_yes_Awareness.pdf", width=8.5, height=4.5)
          chisq (dataframe=survey, dep="Awareness", indep="Stakeholder","n") +
          annotate("text",x=c(1,2,3,4,5,6,7),y=1.05,ymax=1.10,label=c("b","a","b","a","a","b", "ab"),size=4)
          dev.off()     
     
     pdf(file="./Figures/Exploratory/YesNo/02Stakeholder_yes_Consider.pdf", width=8.5, height=4.5)
          chisq (dataframe=survey, dep="Consider", indep="Stakeholder","n") +
          annotate("text",x=c(1,2,3,4,5,6,7),y=1.05,ymax=1.10,label=c("bc","a","bc","a","ab","c","bc"),size=4)
          dev.off()   
     
     pdf(file="./Figures/Exploratory/YesNo/02Stakeholder_yes_Priority.pdf", width=8.5, height=4.5)
          chisq (dataframe=survey, dep="Priority", indep="Stakeholder","n") +
          annotate("text",x=c(1,2,3,4,5,6,7),y=1.05,ymax=1.10,label=c("b","b","bc","a","ab","c", "ab"),size=4) 
          dev.off()   

## By Gender ####

     pdf(file="./Figures/Exploratory/YesNo/03Gender_yes_Awareness.pdf", width=6.5, height=4.5)
          chisq (dataframe=survey, dep="Awareness", indep="Gender","n") +
          annotate("text",x=c(1,2),y=1.05,ymax=1.10,label=c("a","a"),size=4)
          dev.off()     
     
     pdf(file="./Figures/Exploratory/YesNo/03Gender_yes_Consider.pdf", width=6.5, height=4.5)
          chisq (dataframe=survey, dep="Consider", indep="Gender","n") +
          annotate("text",x=c(1,2),y=1.05,ymax=1.10,label=c("a","a"),size=4) 
          dev.off()   
     
     pdf(file="./Figures/Exploratory/YesNo/03Gender_yes_Priority.pdf", width=6.5, height=4.5)
          chisq (dataframe=survey, dep="Priority", indep="Gender","n") +
          annotate("text",x=c(1,2),y=1.05,ymax=1.10,label=c("a","a"),size=4) 
          dev.off()   

## By Age ####
     
     pdf(file="./Figures/Exploratory/YesNo/04Age_yes_Awareness.pdf", width=6.5, height=4.5)
          chisq (dataframe=survey, dep="Awareness", indep="Age","n") +
          annotate("text",x=c(1,2,3,4),y=1.05,ymax=1.10,label=c("b","ab","a","a"),size=4)
          dev.off()     
     
     pdf(file="./Figures/Exploratory/YesNo/04Age_yes_Consider.pdf", width=6.5, height=4.5)
          chisq (dataframe=survey, dep="Consider", indep="Age","n") +
          annotate("text",x=c(1,2,3,4),y=1.05,ymax=1.10,label=c("b","b","a","a"),size=4)
          dev.off()   
     
     pdf(file="./Figures/Exploratory/YesNo/04Age_yes_Priority.pdf", width=6.5, height=4.5)
          chisq (dataframe=survey, dep="Priority", indep="Age","n") +
          annotate("text",x=c(1,2,3,4),y=1.05,ymax=1.10,label=c("a","a","a","a"),size=4) 
          dev.off()   

## By Education ####

     pdf(file="./Figures/Exploratory/YesNo/05Education_yes_Awareness.pdf", width=6.5, height=4.5)
          chisq (dataframe=survey, dep="Awareness", indep="Education","n") +
          annotate("text",x=c(1,2,3,4),y=1.05,ymax=1.10,label=c("a","ab","bc","c"),size=4)
          dev.off()     
     
     pdf(file="./Figures/Exploratory/YesNo/05Education_yes_Consider.pdf", width=6.5, height=4.5)
          chisq (dataframe=survey, dep="Consider", indep="Education","n") +
          annotate("text",x=c(1,2,3,4),y=1.05,ymax=1.10,label=c("a","a","a","a"),size=4) 
          dev.off()   
     
     pdf(file="./Figures/Exploratory/YesNo/05Education_yes_Priority.pdf", width=6.5, height=4.5)
          chisq (dataframe=survey, dep="Priority", indep="Education","n") +
          annotate("text",x=c(1,2,3,4),y=1.05,ymax=1.10,label=c("a","a","ab","b"),size=4) 
          dev.off()  

## By Forest_Type ####

     pdf(file="./Figures/Exploratory/YesNo/06Forest_Type_yes_Awareness.pdf", width=6.5, height=4.5)
          chisq (dataframe=survey, dep="Awareness", indep="Forest_Type","n") +
          annotate("text",x=c(1,2,3),y=1.05,ymax=1.10,label=c("a","a","a"),size=4) 
          dev.off()     
     
     pdf(file="./Figures/Exploratory/YesNo/06Forest_Type_yes_Consider.pdf", width=6.5, height=4.5)
          chisq (dataframe=survey, dep="Consider", indep="Forest_Type","n") +
          annotate("text",x=c(1,2,3),y=1.05,ymax=1.10,label=c("a","ab","ab"),size=4) 
          dev.off()   
     
     pdf(file="./Figures/Exploratory/YesNo/06Forest_Type_yes_Priority.pdf", width=6.5, height=4.5)
          chisq (dataframe=survey, dep="Priority", indep="Forest_Type","n") +
          annotate("text",x=c(1,2,3),y=1.05,ymax=1.10,label=c("a","a","a"),size=4) 
          dev.off() 

## By Politics #### 

     pdf(file="./Figures/Exploratory/YesNo/07Politics_yes_Awareness.pdf","n", width=6.5, height=4.5)
          chisq (dataframe=survey, dep="Awareness", indep="Politics", "n") +
          annotate("text",x=c(1,2,3,4,5,6,7),y=1.05,ymax=1.10,label=c("a","a","a","a","a","a","a"),size=4) 
          dev.off()     
     
     pdf(file="./Figures/Exploratory/YesNo/07Politics_yes_Consider.pdf", width=6.5, height=4.5)
          chisq (dataframe=survey, dep="Consider", indep="Politics","n") +
          annotate("text",x=c(1,2,3,4,5,6,7),y=1.05,ymax=1.10,label=c("a","a","a","a","a","a","a"),size=4) 
          dev.off()   
     
     pdf(file="./Figures/Exploratory/YesNo/07Politics_yes_Priority.pdf", width=6.5, height=4.5)
          chisq (dataframe=survey, dep="Priority", indep="Politics","n") +
          annotate("text",x=c(1,2,3,4,5,6,7),y=1.05,ymax=1.10,label=c("a","a","a","a","a","a","a"),size=4) 
          dev.off() 