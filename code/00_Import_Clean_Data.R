rm(list=ls())

library(survey)
library(psych)
library(agricolae)
library(tidyverse)
library (fifer) # post-hoc tests for chi-square test

load("./data/raw_SurveyData.Rdata")

# Rename variables --------------------------------------------------------

    # General Impacts
    colnames(survey)[19] <- "Human_Cause"
    colnames(survey)[20] <- "Exagerated"
    colnames(survey)[21] <- "Inconclusive"
    colnames(survey)[22] <- "Threat"
    colnames(survey)[23] <- "Understand"
    colnames(survey)[24] <- "Ample_time"
    general_impacts <- colnames(survey)[19:24]
    
    # Forest Impacts
    colnames(survey)[25] <- "Current_Impact"
    colnames(survey)[26] <- "Fiftyyears"
    colnames(survey)[27] <- "Hundredyears"
    colnames(survey)[28] <- "Predictable"
    colnames(survey)[29] <- "Certainty"
    colnames(survey)[30] <- "Managers_Understand"
    colnames(survey)[31] <- "Managers_Control"
    forest_impacts <- colnames(survey)[25:31]
    
    # Current Practices
    colnames(survey)[32] <- "Legislation"
    colnames(survey)[33] <- "Timber_Supply"
    colnames(survey)[34] <- "Current_Sufficient"
    colnames(survey)[35] <- "New_Practices"
    colnames(survey)[36] <- "Wait"
    current_practices <- colnames(survey)[32:36]
    
    # Ecosystem Management
    colnames(survey)[37] <- "Fully_Functioning"
    colnames(survey)[38] <- "Reduce_Discrepancy"
    colnames(survey)[39] <- "Lack_Info_Reference"
    colnames(survey)[40] <- "Past_is_no_goal"
    colnames(survey)[41] <- "Mimick_Disturbances"
    colnames(survey)[42] <- "Retention"
    ecosystem_management <- colnames(survey)[37:42]
    
    # CLIMATE CHANGE AWARENESS
    colnames(survey)[43]<- "Awareness"
    colnames(survey)[44]<- "Consider"
    colnames(survey)[45]<- "Priority"
    awarenes <- colnames(survey)[43:45]
    
    # ADAPTIVE PRACTICES
    colnames(survey)[46]<- "Natural_Regeneration"
    colnames(survey)[47]<- "Old_growth_forests"
    colnames(survey)[48]<- "Reduce_differences_natural"
    colnames(survey)[49]<- "Species_diversification"
    colnames(survey)[50]<- "Translocate_Populations"
    colnames(survey)[51]<- "Assisted_migration"
    colnames(survey)[52]<- "Seed_transfer"
    colnames(survey)[53]<- "Continuous_cover"
    colnames(survey)[54]<- "Provenance_tests"
    colnames(survey)[55]<- "Diverse_experiments"
    colnames(survey)[56]<- "Shorten_rotation"
    colnames(survey)[57]<- "Thinning"
    colnames(survey)[58]<- "Salvage_logging"
    colnames(survey)[59]<- "Promote_Retention"
    colnames(survey)[60]<- "Timber_supply"
    colnames(survey)[61]<- "Harvest_vulnerable_stands"
    colnames(survey)[62]<- "Practices_Comments"
    adaptive <- colnames(survey)[46:62]
    
    # BARRIERS FOR IMPLEMENTATION
    colnames(survey)[63]<- "Uncertainties"
    colnames(survey)[64]<- "Practitioners_impact_low"
    colnames(survey)[65]<- "Lack_scientific_support"
    colnames(survey)[66]<- "Lack_info_local_scale"
    colnames(survey)[67]<- "Lack_solutions"
    colnames(survey)[68]<- "Lack_tools"
    colnames(survey)[69]<- "Lack_workforce"
    colnames(survey)[70]<- "Costs"
    colnames(survey)[71]<- "Forest_companies_reluctant"
    colnames(survey)[72]<- "Lack_funding"
    colnames(survey)[73]<- "Forest_certification"
    colnames(survey)[74]<- "Rigid_legislation"
    colnames(survey)[75]<- "Lack_common_view"
    colnames(survey)[76]<- "Lack_of_policies"
    colnames(survey)[77]<- "Other_imminent_issues"
    colnames(survey)[78]<- "Barriers_Comments"
    barriers <- colnames(survey)[63:78]

# Transform and regroup variables ----------------------------------------------------

    ## Provinces (group Maritimes & Alberta + Saskatchewan)
    survey <- survey %>%
        mutate(Province=replace(Province, Province == "Nova Scotia","New Brunswick"),
               Province=replace(Province, Province == "Newfoundland and Labrador","New Brunswick"),
               Province=replace(Province, Province == "Saskatchewan","Alberta"),
               Province=replace(Province, Province == "Yukon",NA))
    survey$Province<- factor (survey$Province)
    survey$Province <- factor (survey$Province, levels=c("British Columbia", "Alberta", "Ontario",
                                                         "Quebec", "New Brunswick"))
     table(survey$Province)
     prop.table(table(survey$Province))

     ## Ages (regroup categories)
     survey <-  survey %>%
         mutate(Age= replace(Age,Age  == "65 to 74" | Age == "75 or older","55 to 64" ),
                Age = replace (Age, Age =="18 to 24", "25 to 34"))
     survey$Age[survey$Age == "18 to 24"] <- "25 to 34"
     survey$Age <-factor(survey$Age)
     survey$Age <- ordered (survey$Age)
     table(survey$Age)  
     
     ## Gender (regroup categories)
     survey$Gender <-factor(survey$Gender)
     table(survey$Gender)  
     
     ## Forest_Type (regroup categories)
     survey$Forest_Type <-factor(survey$Forest_Type)
     table(survey$Forest_Type)  

     ## Education (group College & High School & Trade School)
     table(survey$Education2)
     survey$Education2 <- factor (survey$Education, levels=c("Non Universitary", "Bachelors Degree", "Masters Degree","Doctorate"))
     survey <- survey %>%
         mutate(Education2 = replace(Education2, Education %in% c("Trade or Vocational School",
                                                                "College/ Cegep" ,
                                                                "High school"),"Non Universitary"  ))
         
     survey$Education <- factor (survey$Education2)
     table(survey$Education)
     survey$Education <- ordered (survey$Education)


     ## Organization
     table (survey$Organization)
     survey$Organization[survey$Organization=="Other"] <- "Provincial Government"
     survey$Organization[survey$Other..please.specify..Organization == "Academic Support"] <- "Academia (faculty, research associate, postdoctoral fellow)"
     survey$Organization[survey$Other..please.specify..Organization == "Acdi Fao; Foresterie Internationale; Avec Un Fort Lien Avec La Foresterie Québécoise Et Canadienne"] <- "Environmental non-governmental organization"
     survey$Organization[survey$Other..please.specify..Organization == "Agence D'aménagement Forestier Foret Privée"] <- "Industry / Industry Association"
     survey$Organization[survey$Other..please.specify..Organization == "And Academia "] <- "Academia (faculty, research associate, postdoctoral fellow)"
     survey$Organization[survey$Other..please.specify..Organization == "Centre De Recherche"] <- "Academia (faculty, research associate, postdoctoral fellow)"
     survey$Organization[survey$Other..please.specify..Organization == "Consulting, Soon To Rejoin Academia"] <- "Consulting"
     survey$Organization[survey$Other..please.specify..Organization == "Enseignement Collégial"] <- "Academia (faculty, research associate, postdoctoral fellow)"
     survey$Organization[survey$Other..please.specify..Organization == "Forest Modeling"] <- "Academia (faculty, research associate, postdoctoral fellow)"
     survey$Organization[survey$Other..please.specify..Organization == "Forestry Consulting/logging/renewables"] <- "Consulting"
     survey$Organization[survey$Other..please.specify..Organization == "Forestry Land Use Consulting"] <- "Industry / Industry Association"
     survey$Organization[survey$Other..please.specify..Organization == "Grande Forêt Privée"] <- "Industry / Industry Association"
     survey$Organization[survey$Other..please.specify..Organization == "I've Worked In All - Currently Semi-retired"] <- "Industry / Industry Association"
     survey$Organization[survey$Other..please.specify..Organization == "Industrial Research"] <- "Industry / Industry Association"
     survey$Organization[survey$Other..please.specify..Organization == "Instructor"] <- "Academia (faculty, research associate, postdoctoral fellow)"
     survey$Organization[survey$Other..please.specify..Organization == "Mise En Marché Des Bois En Forêt Privée"] <- "Industry / Industry Association"
     survey$Organization[survey$Other..please.specify..Organization == "Mrc"] <- "Provincial Government"
     survey$Organization[survey$Other..please.specify..Organization == "Municipal"] <- "Provincial Government"
     survey$Organization[survey$Other..please.specify..Organization == "Obnl Recherche/développement"] <- "Environmental non-governmental organization"
     survey$Organization[survey$Other..please.specify..Organization == "Organisme À But Non Lucratif Pour La Faune"] <- "Environmental non-governmental organization"
     survey$Organization[survey$Other..please.specify..Organization == "Organisme De Concertation Et De Développement"] <- "Provincial Government"
     survey$Organization[survey$Other..please.specify..Organization == "Parapublic"] <- NA
     survey$Organization[survey$Other..please.specify..Organization == "Prefer Not To Say"] <- NA
     survey$Organization[survey$Other..please.specify..Organization == "Private Woodlot Management"] <- "Industry / Industry Association"
     survey$Organization[survey$Other..please.specify..Organization == "Protection Des Forêts"] <- "Provincial Government"
     survey$Organization[survey$Other..please.specify..Organization == "Regroupement De Propriétaires Forestiers"] <- "Industry / Industry Association"
     survey$Organization[survey$Other..please.specify..Organization == "Stagiaire Post-doctoral En Sciences"] <- "Academia (faculty, research associate, postdoctoral fellow)"
     survey$Organization[survey$Other..please.specify..Organization == "Unemployed"] <- NA
     survey$Organization[survey$Other..please.specify..Organization == "Utilité Électrique"] <- "Provincial Government"
     survey$Organization <- factor(survey$Organization)
     table (survey$Organization)   

     survey$Stakeholder[survey$Organization=="Academia (faculty, research associate, postdoctoral fellow)" ] <- "Academia"
     survey$Stakeholder[survey$Organization=="Consulting" ] <- "Other private"
     survey$Stakeholder[survey$Organization=="Environmental non-governmental organization" ] <- "Other private"
     survey$Stakeholder[survey$Organization=="Federal Government" ] <- "Federal Gov."
     survey$Stakeholder[survey$Organization=="First Nations Organization" ] <- NA
     survey$Stakeholder[survey$Organization=="Graduate student"] <- "Student"
     survey$Stakeholder[survey$Organization=="Industry / Industry Association" ] <- "Industry"
     survey$Stakeholder[survey$Organization=="Provincial Government"] <- "Provincial Gov."
     survey$Stakeholder<- factor(survey$Stakeholder)
     survey$Stakeholder <- factor (survey$Stakeholder, levels=c("Federal Gov.", "Provincial Gov.",
                                                                                 "Industry", "Other private","Academia", "Student"))
     table (survey$Stakeholder)
     prop.table(table(survey$Stakeholder))

     ## Political views--> Politics
     levels(survey$Political_View) <- c(levels(survey$Political_View), "1", "7")
     survey$Political_View[survey$Political_View =="Very Liberal<br />1"] <- "1"
     survey$Political_View[survey$Political_View =="Very conservative<br />7"] <- "7"
     survey$Political_View<- factor(survey$Political_View)
     survey$Politics<-factor(survey$Political_View, levels=c("1","2","3","4","5","6","7",""))
     survey$Politics<-as.numeric(survey$Politics)
     survey$Politics[survey$Politics==8]<-NA
     survey$Political_View <- survey$Politics
     survey$Politics <- ordered (survey$Politics)
     table(survey$Political_View)
     table(survey$Politics)


     ## CC Awareness
     table(survey$Awareness)
     survey$Awareness <- factor (survey$Awareness)
     survey$Awareness <- factor (survey$Awareness, levels=c("Yes", "Don't know / Prefer not to say", "No"), 
                                 labels=c("Yes", "Don't know", "No"))
     table(survey$Awareness)

     ## CC Consider
     table(survey$Consider)
     survey$Consider <- factor (survey$Consider)
     survey$Consider <- factor (survey$Consider, levels=c("Yes", "Don't know / Prefer not to say", "No"),
                                labels=c("Yes", "Don't know", "No"))
     table(survey$Consider)
     
     ## CC Priority
     table(survey$Priority)
     survey$Priority <- factor (survey$Priority)
     survey$Priority <- factor (survey$Priority, levels=c("Yes", "Don't know / Prefer not to say", "No"),
                                labels=c("Yes", "Don't know", "No"))
     table(survey$Priority)

# New Ecological Paradigm -----------------------------------------------------------

# Odd numbered questions are scored from 1 (strongly disagree) to 7 (strongly agree), 
# while the even numbered questions are assigned values from 7 (strongly disagree)
# to (1 strongly agree) to correct for the wording reversal.
     
     survey[,"Nep01"]<-survey[,79]
     survey[,"Nep02"]<-8-survey[,80]
     survey[,"Nep03"]<-survey[,81]
     survey[,"Nep04"]<-8-survey[,82]
     survey[,"Nep05"]<-survey[,83]
     survey[,"Nep06"]<-8-survey[,84]
     survey[,"Nep07"]<-survey[,85]
     survey[,"Nep08"]<-8-survey[,86]
     survey[,"Nep09"]<-survey[,87]
     survey[,"Nep10"]<-8-survey[,88]
     survey[,"Nep11"]<-survey[,89]
     survey[,"Nep12"]<-8-survey[,90]
     survey[,"Nep13"]<-survey[,91]
     survey[,"Nep14"]<-8-survey[,92]
     survey[,"Nep15"]<-survey[,93]
     
     survey$nep<-rowSums(survey[,c("Nep01", "Nep02", "Nep03", "Nep04", "Nep05", "Nep06", "Nep07",
                                   "Nep08", "Nep09", "Nep10", "Nep11", "Nep12","Nep13", "Nep14", "Nep15")] )
     

# Save data ---------------------------------------------------------------
     
     survey<- survey %>%
         dplyr::select(-(Legacy.Comments:URL.Variable..snc),-X,
                       -Organization, -starts_with("Other..please"),-Education2,
                       -(starts_with("We.are.approaching"):starts_with("If.things")),-starts_with("Please.use"))

     save(survey, file = "./Data/SurveyData_Clean.Rdata")




