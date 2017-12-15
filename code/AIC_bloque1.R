rm(list=ls())
library(reshape2)

source("./Rcode/01Import_Clean_Data.R")
source("./Rcode/01bAssignWeights.R")
source("./Rcode/survey_functions.R")



### Anova con NEP ####

allvars <- complete.cases(survey[,c("Province","Stakeholder","nep")])
survey2<- survey[allvars,]

testAIC <- function (dependent) {
     
     aic_values <- data.frame (matrix (NA, nrow=1, ncol=9))
     temp_values <- data.frame (matrix (NA, nrow=1, ncol=9))
     names(aic_values) <- c("Question", "null", "nep", "Stakeholder", 'Province', "nep_Stake", "nep_Prov",
                              "Stake_Prov","Triple")
     names(temp_values) <- c("Question", "null", "nep", "Stakeholder", 'Province', "nep_Stake", "nep_Prov",
                             "Stake_Prov","Triple")
     
     for (i in unique(dependent)) {
          dep <- survey2[,i]
          model0 <-aov(dep~1, data=survey2)
          model1 <-aov(dep~nep, data=survey2)
          model2 <-aov(dep~Stakeholder, data=survey2)
          model3 <-aov(dep~Province, data=survey2)
          model4 <-aov(dep~nep*Stakeholder, data=survey2)
          model5 <-aov(dep~nep*Province, data=survey2)
          model6 <-aov(dep~Stakeholder*Province, data=survey2)
          model7 <-aov(dep~nep*Stakeholder*Province, data=survey2)
          
          
          AICs <- AIC(model0, model1, model2, model3, model4, model5, model6, model7)
          
          temp_values$Question <- (names(survey2[i]))
          temp_values$null <- AICs$AIC[1]
          temp_values$nep <- AICs$AIC[2]
          temp_values$Stakeholder <- AICs$AIC[3]
          temp_values$Province <- AICs$AIC[4]
          temp_values$nep_Stake <- AICs$AIC[5]
          temp_values$nep_Prov <- AICs$AIC[6]
          temp_values$Stake_Prov <- AICs$AIC[7]
          temp_values$Triple <- AICs$AIC[8]
          #print(temp_values)
          
          aic_values<- rbind(aic_values,temp_values)
     }
     aic_values <<- aic_values[-1,]
     return(aic_values)
}


# Bloque 1 and 2 
testAIC(c(19:24,25:31,32:36,37:42))
aic_dif <- aic_values
minims<- apply(aic_dif[,2:9],MARGIN=1,FUN=min)
aic_dif[,2:9] <- round(aic_dif[,2:9] - minims,digits=3)
write.table(aic_dif, "./Results/AICdif_bloque3.txt", sep="\t", row.names=F)


# Bloque 3
testAIC(c(46:61))
aic_dif <- aic_values
minims<- apply(aic_dif[,2:9],MARGIN=1,FUN=min)
aic_dif[,2:9] <- round(aic_dif[,2:9] - minims,digits=3)
write.table(aic_dif, "./Results/AICdif_bloque3.txt", sep="\t", row.names=F)

# Bloque 4
testAIC(c(63:77))
aic_dif <- aic_values
minims<- apply(aic_dif[,2:9],MARGIN=1,FUN=min)
aic_dif[,2:9] <- round(aic_dif[,2:9] - minims,digits=3)
write.table(aic_dif, "./Results/AICdif_bloque3.txt", sep="\t", row.names=F)



