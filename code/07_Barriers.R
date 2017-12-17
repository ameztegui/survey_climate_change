
## Barriers to implementation ##

rm(list=ls())

source("./code/survey_functions.R")
source("./code/zz_AMZcolors.R")

colors16 <- AMZcolors[c(3,4,6,7,8,9,10,11,12,13,14,15,17,18,19,20)]

load("./data/SurveyData_Clean_Weighted.Rdata")

survey <- survey %>%
    mutate(Stakeholder = factor(Stakeholder, 
                                levels = c("Industry", "Provincial Gov.",
                                           "Federal Gov.", "Other private",
                                           "Academia", "Student")))

library(scales)
library(reshape2)
library(party)
library(tidyverse)
          
          
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
               Province = fct_recode(Province, 
                                     "British \n Columbia" = "British Columbia",
                                     "New\nBrunswick" = "New Brunswick"))
        
    barriers_stake <- survey %>%
        gather(49:63,key="Barrier", value = "score") %>%
        group_by(Barrier, Stakeholder) %>%
        dplyr::summarise(score = mean(score, na.rm=T)) %>%
        group_by(Stakeholder) %>%
        dplyr::mutate(rank = dense_rank(-score)) %>%
        mutate(Barriers = mf_labeller(Barrier),
               Stakeholder = fct_recode(Stakeholder, 
                                      "Industry" = "Industry",
                                      "Provincial\nGovernment" = "Provincial Gov.",
                                      "Federal\nGovernment" = "Federal Gov.",
                                      "Other\nPrivate" = "Other private",
                                      "Academics &\n researchers" = "Academia",
                                      "Students" = "Student"))
    
    
# Create bump charts for province and stakeholder -------------------------

    # Per province
    
    pdf("./figs/Barriers_Bump_Province.pdf", width=27, height=16)
        p <- ggplot(barriers_prov, aes(Province, rank,
                                  group = Barriers, 
                                  colour = fct_reorder2(Barriers, Province, -rank), 
                                  label = Barriers)) + 
            geom_line(size=3.5) + 
            geom_text(data = subset(barriers_prov,Province == "New\nBrunswick"), 
                      size=10, aes(x = Province, hjust = -0.1), lineheight=0.85) + 
            geom_point(size=8) +
            theme_bw() + 
            theme(legend.position = "none", 
                  panel.border = element_blank(),
                  axis.ticks = element_blank()) +
            scale_colour_manual(values = colors16) +
            scale_x_discrete(breaks = c(levels(barriers_prov$Province)), "") + 
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
        p <- ggplot(barriers_stake, aes(Stakeholder, rank,
                                       group = Barriers, 
                                       colour = fct_reorder2(Barriers, Stakeholder, -rank), 
                                       label = Barriers)) + 
            geom_line(size=3.5) + 
            geom_point(size=8) +
            geom_text(data = subset(barriers_stake,Stakeholder == "Students"), 
                      size=10, aes(x = Stakeholder, hjust = -0.1), lineheight=0.85) + 
            theme_bw() + 
            theme(legend.position = "none", 
                  panel.border = element_blank(),
                  axis.ticks = element_blank()) +
            scale_colour_manual(values = colors16) +
            scale_x_discrete(breaks = c(levels(barriers_stake$Stakeholder), "")) + 
            scale_y_continuous(breaks = NULL,trans = "reverse") +
            xlab(NULL) + ylab(NULL) +
            theme(axis.text=element_text(size=32),
                  plot.margin = unit(c(0,22,0.5,0), "cm"))
        
        # Code to override clipping
        gt <- ggplot_gtable(ggplot_build(p))
        gt$layout$clip[gt$layout$name == "panel"] <- "off"
        grid.draw(gt)
    dev.off()