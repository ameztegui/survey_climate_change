
## Barriers to implementation ##

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
          
          
## Barriers to implementation (49-63) ######

# Provide the overall max-diff score and assess differences
    print(describe_simple(survey,49:63))
    # wtd_describe_simple(survey,49:63)

# Create dataframe with the score per province
    barriers_prov <- survey %>%
        gather(49:63,key="Barrier", value = "score") %>%
        group_by(Barrier, Province) %>%
            dplyr::summarise(score = mean(score, na.rm=T)) %>%
        group_by(Province) %>%
            dplyr::mutate(rank = dense_rank(-score))%>%
        mutate(Barriers = mf_labeller(Barrier),
               Provinces = fct_recode(Province, "British \nColumbia" = "BC",
                                      "Alberta" = "AB",
                                      "Ontario" = "ON",
                                      "Quebec" = "QC",
                                      "New \nBrunswick" = "NB") )
        
    barriers_stake <- survey %>%
        gather(49:63,key="Barrier", value = "score") %>%
        group_by(Barrier, Stakeholder) %>%
        dplyr::summarise(score = mean(score, na.rm=T)) %>%
        group_by(Stakeholder) %>%
        dplyr::mutate(rank = dense_rank(-score)) %>%
        mutate(Stakeholder = factor(Stakeholder,
                                    levels = c("Ind","P.Gov", "F.Gov", "Pri", "Aca", "Stu")),
               Barriers = mf_labeller(Barrier),
               Stakeholders = fct_recode(Stakeholder, "Industry" = "Ind",
                                      "Provintial\nGovernment" = "P.Gov",
                                      "Federal\nGovernment" = "F.Gov",
                                      "Private\ncompanies" = "Pri",
                                      "Academics &\n researchers" = "Aca",
                                      "Students" = "Stu") )
    
    
# Create bump charts for province and stakeholder -------------------------

    # Per province
    
    pdf("./figs/Barriers_Bump_Province.pdf", width=27, height=16)
        p <- ggplot(barriers_prov, aes(Provinces, rank,
                                  group = Barriers, 
                                  colour = fct_reorder2(Barriers, Provinces, -rank), 
                                  label = Barriers)) + 
            geom_line(size=3.5) + 
            geom_text(data = subset(barriers_prov,Province == "NB"), 
                      size=10, aes(x = Provinces, hjust = -0.1), lineheight=0.85) + 
            geom_point(size=8) +
            theme_bw() + 
            theme(legend.position = "none", 
                  panel.border = element_blank(),
                  axis.ticks = element_blank()) +
            scale_colour_manual(values = colors16) +
            scale_x_discrete(breaks = c(levels(barriers_prov$Provinces)), "") + 
            scale_y_continuous(breaks = NULL,trans = "reverse") +
            xlab(NULL) + ylab(NULL) +
            theme(axis.text=element_text(size=32),
                  plot.margin = unit(c(0,22,0.5,0), "cm"))
        
        # Code to override clipping
        gt <- ggplot_gtable(ggplot_build(p))
        gt$layout$clip[gt$layout$name == "panel"] <- "off"
        grid.draw(gt)
    dev.off()

    # Per stakeholder
    pdf("./figs/Barriers_Bump_Stakeholder.pdf", width=27, height=16)
    p <- ggplot(barriers_stake, aes(Stakeholders, rank,
                                   group = Barriers, 
                                   colour = fct_reorder2(Barriers, Stakeholders, -rank), 
                                   label = Barriers)) + 
        geom_line(size=3.5) + 
        geom_point(size=8) +
        geom_text(data = subset(barriers_stake,Stakeholder == "Stu"), 
                  size=10, aes(x = Stakeholders, hjust = -0.1), lineheight=0.85) + 
        theme_bw() + 
        theme(legend.position = "none", 
              panel.border = element_blank(),
              axis.ticks = element_blank()) +
        scale_colour_manual(values = colors16) +
        scale_x_discrete(breaks = c(levels(barriers_stake$Stakeholders), "")) + 
        scale_y_continuous(breaks = NULL,trans = "reverse") +
        xlab(NULL) + ylab(NULL) +
        theme(axis.text=element_text(size=32),
              plot.margin = unit(c(0,22,0.5,0), "cm"))
    
    # Code to override clipping
    gt <- ggplot_gtable(ggplot_build(p))
    gt$layout$clip[gt$layout$name == "panel"] <- "off"
    grid.draw(gt)
    dev.off()
