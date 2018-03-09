library(survey)
load("./data/SurveyData_Clean.Rdata")

survey.complete <- survey %>%
    filter(Status == "Complete")

survey.clean <- survey.complete %>%
    filter (!is.na(Stakeholder), !is.na(Province))


table(survey.clean$Province, survey.clean$Stakeholder, exclude = NULL)

svy.AB <- filter(survey.clean, Province=="Alberta")
svy.BC <- filter(survey.clean, Province=="British Columbia")
svy.ON <- filter(survey.clean, Province=="Ontario")
svy.QC <- filter(survey.clean, Province=="Quebec")
svy.NB <- filter(survey.clean, Province=="New Brunswick")

svy.FG <-  filter(survey.clean, Stakeholder == "Federal Gov.")
svy.PG <-  filter(survey.clean, Stakeholder == "Provincial Gov.")
svy.IN <-  filter(survey.clean, Stakeholder == "Industry")
svy.OP <-  filter(survey.clean, Stakeholder == "Other private")
svy.AC <-  filter(survey.clean, Stakeholder == "Academia")
svy.ST <-  filter(survey.clean, Stakeholder == "Student")


# Create unweighted survey object -----------------------------------------

    # Provinces
     svy.unw.AB <- svydesign(ids=~1, data=svy.AB)
     svy.unw.BC <- svydesign(ids=~1, data=svy.BC)
     svy.unw.ON <- svydesign(ids=~1, data=svy.ON)
     svy.unw.QC <- svydesign(ids=~1, data=svy.QC)
     svy.unw.NB <- svydesign(ids=~1, data=svy.NB)

     # Stakeholders
     svy.unw.FG <- svydesign(ids=~1, data=svy.FG)
     svy.unw.PG <- svydesign(ids=~1, data=svy.PG)
     svy.unw.IN <- svydesign(ids=~1, data=svy.IN)
     svy.unw.OP <- svydesign(ids=~1, data=svy.OP)
     svy.unw.AC <- svydesign(ids=~1, data=svy.AC)
     svy.unw.ST <- svydesign(ids=~1, data=svy.ST)
     

# Define population distributions of the variables to weight --------------
     
     # Provinces
     stake.dist.AB<- data.frame(Stakeholder = c("Federal Gov."  ,  "Provincial Gov." ,"Industry",  "Other private",  
                                             "Academia" ,  "Student" ),
                        Freq = nrow(svy.AB) * c(0.20,0.35,0.10,0.05,0.15,0.15))

     stake.dist.BC<- data.frame(Stakeholder = c("Federal Gov."  ,  "Provincial Gov." ,"Industry",  "Other private",  
                                                "Academia" ,  "Student" ),
                                Freq = nrow(svy.BC) * c(0.20,0.35,0.10,0.05,0.15,0.15))
     stake.dist.ON<- data.frame(Stakeholder = c("Federal Gov."  ,  "Provincial Gov." ,"Industry",  "Other private",  
                                                "Academia" ,  "Student" ),
                                Freq = nrow(svy.ON) * c(0.20,0.35,0.10,0.05,0.15,0.15))
     
     stake.dist.QC<- data.frame(Stakeholder = c("Federal Gov."  ,  "Provincial Gov." ,"Industry",  "Other private",  
                                                "Academia" ,  "Student" ),
                                Freq = nrow(svy.QC) * c(0.20,0.35,0.10,0.05,0.15,0.15))
     
     stake.dist.NB<- data.frame(Stakeholder = c("Federal Gov."  ,  "Provincial Gov." ,"Industry",  "Other private",  
                                                "Academia" ,  "Student" ),
                                Freq = nrow(svy.NB) * c(0.20,0.35,0.10,0.05,0.15,0.15))


     # Stakeholders
     stake.dist.FG<- data.frame(Province = c("British Columbia", "Alberta", "Ontario", "Quebec", "New Brunswick"),  
                                Freq = nrow(svy.FG) * c(0.27,0.12,0.21,0.34,0.06))
     
     stake.dist.PG<- data.frame(Province = c("British Columbia", "Alberta", "Ontario", "Quebec", "New Brunswick"),  
                                Freq = nrow(svy.PG) * c(0.27,0.12,0.21,0.34,0.06))
     
     stake.dist.IN<- data.frame(Province = c("British Columbia", "Alberta", "Ontario", "Quebec", "New Brunswick"),  
                                Freq = nrow(svy.IN) * c(0.27,0.12,0.21,0.34,0.06))
     
     stake.dist.OP<- data.frame(Province = c("British Columbia", "Alberta", "Ontario", "Quebec", "New Brunswick"),  
                                Freq = nrow(svy.OP) * c(0.27,0.12,0.21,0.34,0.06))
     
     stake.dist.AC<- data.frame(Province = c("British Columbia", "Alberta", "Ontario", "Quebec", "New Brunswick"),  
                                Freq = nrow(svy.AC) * c(0.27,0.12,0.21,0.34,0.06))
     
     stake.dist.ST<- data.frame(Province = c("British Columbia", "Alberta", "Ontario", "Quebec", "New Brunswick"),  
                                Freq = nrow(svy.ST) * c(0.27,0.12,0.21,0.34,0.06))


# Compute the weights -----------------------------------------------------


    # Provinces
     svy.rake.AB <- rake(design =svy.unw.AB, sample.margins = list( ~Stakeholder),
                         population.margins = list(stake.dist.AB))
     svy.rake.BC <- rake(design =svy.unw.BC, sample.margins = list( ~Stakeholder),
                         population.margins = list(stake.dist.BC))
     svy.rake.ON <- rake(design =svy.unw.ON, sample.margins = list( ~Stakeholder),
                         population.margins = list(stake.dist.ON))
     svy.rake.QC <- rake(design =svy.unw.QC, sample.margins = list( ~Stakeholder),
                         population.margins = list(stake.dist.QC))
     svy.rake.NB <- rake(design =svy.unw.NB, sample.margins = list( ~Stakeholder),
                         population.margins = list(stake.dist.NB))
     
     # Stakeholders
     svy.rake.FG <- rake(design =svy.unw.FG, sample.margins = list( ~Province),
                         population.margins = list(stake.dist.FG))
     svy.rake.PG <- rake(design =svy.unw.PG, sample.margins = list( ~Province),
                         population.margins = list(stake.dist.PG))
     svy.rake.IN <- rake(design =svy.unw.IN, sample.margins = list( ~Province),
                         population.margins = list(stake.dist.IN))
     svy.rake.OP <- rake(design =svy.unw.OP, sample.margins = list( ~Province),
                         population.margins = list(stake.dist.OP))
     svy.rake.AC <- rake(design =svy.unw.AC, sample.margins = list( ~Province),
                         population.margins = list(stake.dist.AC))
     svy.rake.ST <- rake(design =svy.unw.ST, sample.margins = list( ~Province),
                         population.margins = list(stake.dist.ST))
     
     
     svy.rake.trim.AB <- trimWeights(svy.rake.AB, lower=0.5, upper=2.5, strict=TRUE)
     svy.rake.trim.BC <- trimWeights(svy.rake.BC, lower=0.5, upper=2.5, strict=TRUE)
     svy.rake.trim.ON <- trimWeights(svy.rake.ON, lower=0.5, upper=2.5, strict=TRUE)
     svy.rake.trim.QC <- trimWeights(svy.rake.QC, lower=0.5, upper=2.5, strict=TRUE)
     svy.rake.trim.NB <- trimWeights(svy.rake.NB, lower=0.5, upper=2.5, strict=TRUE)

     svy.rake.trim.FG <- trimWeights(svy.rake.FG, lower=0.5, upper=2.5, strict=TRUE)
     svy.rake.trim.PG <- trimWeights(svy.rake.PG, lower=0.5, upper=2.5, strict=TRUE)
     svy.rake.trim.IN <- trimWeights(svy.rake.IN, lower=0.5, upper=2.5, strict=TRUE)
     svy.rake.trim.OP <- trimWeights(svy.rake.OP, lower=0.5, upper=2.5, strict=TRUE)
     svy.rake.trim.AC <- trimWeights(svy.rake.AC, lower=0.5, upper=2.5, strict=TRUE)
     svy.rake.trim.ST <- trimWeights(svy.rake.ST, lower=0.5, upper=2.5, strict=TRUE)
     
     
     summary(weights(svy.rake.trim.AB))
     summary(weights(svy.rake.trim.BC))
     summary(weights(svy.rake.trim.ON))
     summary(weights(svy.rake.trim.QC))
     summary(weights(svy.rake.trim.NB))
     
     summary(weights(svy.rake.trim.FG))
     summary(weights(svy.rake.trim.PG))
     summary(weights(svy.rake.trim.IN))
     summary(weights(svy.rake.trim.OP))
     summary(weights(svy.rake.trim.AC))
     summary(weights(svy.rake.trim.ST))
     
     svy.AB$Weights<-weights(svy.rake.trim.AB)
     svy.BC$Weights<-weights(svy.rake.trim.BC)
     svy.ON$Weights<-weights(svy.rake.trim.ON)
     svy.QC$Weights<-weights(svy.rake.trim.QC)
     svy.NB$Weights<-weights(svy.rake.trim.NB)

     svy.FG$Weights<-weights(svy.rake.trim.FG)
     svy.PG$Weights<-weights(svy.rake.trim.PG)
     svy.IN$Weights<-weights(svy.rake.trim.IN)
     svy.OP$Weights<-weights(svy.rake.trim.OP)
     svy.AC$Weights<-weights(svy.rake.trim.AC)
     svy.ST$Weights<-weights(svy.rake.trim.ST)
     
     
     
     
#survey<- rbind(svy.AB, svy.BC,svy.ON,svy.QC, svy.NB)
survey<- rbind(svy.FG, svy.PG,svy.IN,svy.OP, svy.AC, svy.ST)


# Save data ---------------------------------------------------------------

save(survey, file= "./data/SurveyData_Clean_Weighted.Rdata")
