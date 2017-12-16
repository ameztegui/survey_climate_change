rm(list=ls())
library(reshape2)

#setwd("~/Research/Activity/QUICCFOR/Ecosystem management")
source("./Rcode/01Import_Clean_Data.R")
source("./Rcode/01bAssignWeights.R")
source("./Rcode/survey_functions.R")

     survey$Stakeholder[survey$Stakeholder == "NGO"] <- NA
     survey<-subset(survey, !is.na(survey$Stakeholder))
     survey$Stakeholder <- factor(survey$Stakeholder)

general_impacts<-melt(data=survey,id.vars=c("Response.ID","Province","Stakeholder","Gender","Age","Education","Forest_Type","Politics","nep"),
                      measure.vars=19:24)
names(general_impacts)[names(general_impacts) == 'variable'] <- 'Statement'

impacts_forests<-melt(data=survey,id.vars=c("Response.ID","Province","Stakeholder","Gender","Age","Education","Forest_Type","Politics","nep"),
                      measure.vars=25:31)
names(impacts_forests)[names(impacts_forests) == 'variable'] <- 'Statement'

current_practices<-melt(data=survey,id.vars=c("Response.ID","Province","Stakeholder","Gender","Age","Education","Forest_Type","Politics","nep"),
                        measure.vars=32:36)
names(current_practices)[names(current_practices) == 'variable'] <- 'Statement'

ecosystem_management<-melt(data=survey,id.vars=c("Response.ID","Province","Stakeholder","Gender","Age","Education","Forest_Type","Politics","nep"),
                           measure.vars=37:42)
names(ecosystem_management)[names(ecosystem_management) == 'variable'] <- 'Statement'


pdf(file="./Figures/Exploratory/NEP/01CC_Impacts_NEP_Stake.pdf", width=12.5, height=6)
simplenep(general_impacts)
simplenep(impacts_forests)
simplenep(current_practices)

#### By NEP and Stakeholder #####

     pdf(file="./Figures/Exploratory/NEP/01CC_Impacts_NEP_Stake.pdf", width=12.5, height=6)
     nepfunction(general_impacts) +
     facet_wrap(~Stakeholder, ncol=3)
     dev.off()

     pdf(file="./Figures/Exploratory/NEP/02CC_Forests_NEP_Stake.pdf", width=11.5, height=7)
     nepfunction(impacts_forests) +
     facet_wrap(~Stakeholder, ncol=3)
     dev.off()

     pdf(file="./Figures/Exploratory/NEP/03Current_Practices_NEP_Stake.pdf", width=11.5, height=7)
     nepfunction(current_practices) +
     facet_wrap(~Stakeholder, ncol=3)
     dev.off()

     pdf(file="./Figures/Exploratory/NEP/04EcosystemManag_NEP_Stake.pdf", width=11.5, height=7)
     nepfunction(ecosystem_management) +
     facet_wrap(~Stakeholder, ncol=3)
     dev.off()





impacts_plot +
     facet_wrap(~Education, ncol=2)


rcorr(x=survey$Politics, y=survey$nep)


#### Efect of other variables on Nep  ####

## Province

impacts_plot +
     facet_wrap(~Province, ncol=3)

forest_plot +
     facet_wrap(~Province, ncol=3)

practices_plot +
     facet_wrap(~Province, ncol=3)

ecosystem_plot +
     facet_wrap(~Province, ncol=3)

## Stakeholder

impacts_plot +
     facet_wrap(~Stakeholder, ncol=3)

forest_plot +
     facet_wrap(~Stakeholder, ncol=3)

practices_plot +
     facet_wrap(~Stakeholder, ncol=3)

ecosystem_plot +
     facet_wrap(~Stakeholder, ncol=3)

## Education

impacts_plot +
     facet_wrap(~Education, ncol=2)

forest_plot +
     facet_wrap(~Education, ncol=2)

practices_plot +
     facet_wrap(~Education, ncol=2)

ecosystem_plot +
     facet_wrap(~Education, ncol=2)
