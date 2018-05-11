rm(list=ls())

load("./data/SurveyData_Clean_Weighted.Rdata")
source("./code/survey_functions.R")
source("./code/02_Likert.R")



# Figure 1: Overall Responses ---------------------------------------------
    
    # Change names of the likert scale so they can be displayed in two rows (only for lower pannel)
      colnames(likert_current$results) <- c("Item", "Strongly\n disagree", "Disagree","Slightly\n disagree",
                               "Neutral", "Slightly\n agree", "Agree", "Strongly\n agree")

    Fig1a <- plot(likert_gen_imp, ordered=F,  wrap= 45,text.size=5) +
        ggtitle("Section 1: General beliefs about climate change") + 
        theme(axis.text.y = element_text(hjust=0, size=16),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size= 17, face = "bold", hjust=0)) +
        guides(fill=FALSE) 


    Fig1b <- plot(likert_for_imp, ordered=F,  wrap=45, text.size=5) +
        ggtitle("Section 2: Impacts of climate change on forest ecosystems") +
        theme(axis.text.y = element_text(hjust=0, size=16),
        axis.text.x =element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size= 17, face = "bold",hjust=0)) +
        guides(fill=FALSE)


    Fig1c <- plot(likert_current, ordered=F,  wrap=45,text.size=5) +
        ggtitle("Section 3: Current forest practices and need for adaptation") +
        theme(axis.text.y = element_text(hjust=0, size=16),
        axis.text.x = element_text(size=16),
        axis.title.x = element_text(size=14),
        plot.title = element_text(size= 17, face = "bold",hjust=0),
        legend.text = element_text(size = 15)) +
        guides( fill=guide_legend("",nrow=1))    

    tiff(file="./figs/Final/Figure_01.tif", width=12.2, height=18, res = 300, units = "in", compression = "lzw")
        multiplot (Fig1a, Fig1b, Fig1c, cols=1)
    dev.off()

  
  
# Figure 02: Impacts of Climate Change across stakeholders for a s --------
        # (The rest is in the Appendix S2)
    
    fig_2_data <- fct_survey[, c(5,8,10,11,12,16,20,21) ]
        colnames(fig_2_data) <- mf_label_number_complete(colnames(fig_2_data))
    
    fig2 <- likert(fig_2_data)
    fig2_stake <- likert(fig_2_data ,grouping = survey$Stakeholder)
    fig2_stake$results$Group <- factor (fig2_stake$results$Group, 
                                         levels = rev(levels(fig2_stake$results$Group)))
    colnames(fig2_stake$results) <- c("Group","Item", "Strongly\n disagree", "Disagree","Slightly\n disagree",
                                    "Neutral", "Slightly\n agree", "Agree", "Strongly\n agree")
  
    letters_fig2 <- mean_likert(survey,"Stakeholder", c(5,8,10,11,12,16,20,21)) %>%
        arrange(Stakeholder)
    
    tiff(file="./figs/Final/Figure_02.tif", width=9, height=16, res = 300, units = "in", compression = "lzw")
    # pdf(file="./figs/Final/Figure_02.pdf", width=9, height=16)
        plot(fig2_stake, ordered=F, text.size=4,plot.percents=T , wrap=160) +
            theme(axis.text.y = element_text(size = 14),
                axis.text.x = element_text (size = 12),
                strip.text = element_text(size = 12, face="bold", hjust=0),
                strip.background = element_rect (fill= "white", colour="white"),
                legend.text = element_text(size = 12)) +
            guides(fill=guide_legend("",nrow=1)) +
            annotate("text", x=6, y =-98, size=4, fontface=1,hjust=0,
                     label = unlist(letters_fig2[1,-1])) + #Federal
            annotate("text", x=5, y =-98, size=4, fontface=1,hjust=0,
                     label = unlist(letters_fig2[2,-1])) + #Provincial
            annotate("text", x=4, y =-98, size=4, fontface=1,hjust=0,
                     label = unlist(letters_fig2[3,-1])) + # Industry
            annotate("text", x=3, y =-98, size=4, fontface=1,hjust=0,
                     label = unlist(letters_fig2[4,-1])) + # Private
            annotate("text", x=2, y =-98, size=4, fontface=1,hjust=0,
                     label = unlist(letters_fig2[5,-1])) + # Academia
            annotate("text", x=1, y =-98, size=4, fontface=1,hjust=0,
                     label =unlist(letters_fig2[6,-1])) # Students
        dev.off()
  

   
        
# Figure 3 & 4: Conditional Interval Regression Trees (ctree) for a set of statements
     # (The rest is in the Appendix S3)

