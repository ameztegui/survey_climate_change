######################################################################################
############ SURVEY COMPARISON
############  Aitor Ameztegui, UQAM, 2015
############
############ This script contains some functions to analyze and represent the data obtained from the
############ survey  "Integration of Climate Change into Forest Management in Canada"


source("./code/zz_AMZcolors.R")

library(agricolae)
library(Hmisc)
library(reshape2)
library(weights)
library(fifer)
library(tidyverse)


         # ANOVA: comparison when dep. variables is continuous -------------------------------
         
              # We must define the dataframe, the dependent variable (which must be continuous), and the independent variable,
              # which must be categorical. First, there are some lines of code to subset the dataframe and merge some categories
              # of the indep. variables (this part is specific of this study)
              # 
              # Then, we perform an ANOVA and determine Tukey pairwise comparisons among levels of the categorical variable
              # (with Bonferroni corrections)
              # 
              # Finally, we plot the value of the dep. variable across the levels of the factor using boxplots, and add the mean
              # for each level at the bottom, and the letter from Tukey test at the top.
    
    
              survey_comparison <- function (dataframe, dep, indep) {    
                   # Subset data
                   data <- subset(dataframe, !is.na(dataframe[indep]))
                   data <- subset(data, !is.na(data[dep]))
                   
                   if (dep == "Gender" | indep == "Gender") {
                        data<-data[data$Gender == "Male" | data$Gender == "Female",]
                        data$Gender <- factor (data$Gender) }
                   if (dep == "Forest_Type" | indep == "Forest_Type") {
                        data<-data[data$Forest_Type == "Boreal forest" | data$Forest_Type == "Temperate forest" | data$Forest_Type == "Mixed / Both", ]
                        data$Forest_Type <- factor(data$Forest_Type)   }
                   if (dep == "Stakeholder" ){
                        #data$Stakeholder[data$Stakeholder == "NGO"] <- "Consulting"
                        #data$Stakeholder[data$Stakeholder == "Student"] <- NA               
                        #data$Stakeholder[data$Stakeholder == "Consulting"] <- "Industry"
                        data<-subset(data, !is.na(data$Stakeholder))
                        data$Stakeholder <- factor(data$Stakeholder) }
                   if (indep == "Politics") {
                        data$Politics <- factor(data$Politics)  }
                   if (dep == "Education" | indep == "Education") {
                        data$Education <- factor(data$Education)  }     
                   
                   # ANOVA and post-hoc comparison with Bonferroni corrections
                   frm <- paste(dep,indep,sep="~")
                   anova.test <- aov(formula(frm), data=data)
                   Tuk <- HSD.test (anova.test, trt=indep)
                   #print(Tuk$groups)
                   
                        if(indep=="Province"){
                        Tuk$groups$trt <- factor (Tuk$groups$trt, levels=c("British Columbia", "Alberta         ", "Ontario         ",
                                                                             "Quebec          ", "New Brunswick   "))}
                        
                        #if(indep=="Stakeholder"){
                        #Tuk$groups$trt <- factor (Tuk$groups$trt, levels=c("Federal Gov.   ", "Provincial Gov.", "Academia       ",
                         #                                                  "Industry       ","Other private", "Student        "))}
                        if(indep=="Education"){
                             Tuk$groups$trt <- factor (Tuk$groups$trt, levels=c("Non Universitary", "Bachelors Degree", "Masters Degree  ", "Doctorate       " ))}
                        
                   #print(Tuk$groups)
                   #print(levels(Tuk$groups$trt) )        
                   Tuk$groups<-Tuk$groups[order(Tuk$groups$trt,na.last=T),]
                  # print(Tuk$groups)
                   
                   # Plot results
                   ggplot(data,aes_string(x = indep,y = dep )) + 
                        geom_boxplot(notch=F,outlier.size=0) + 
                        #scale_colour_manual (values=rep(x=brewer_maroon,times=5)) +
                        scale_x_discrete((indep)) +
                        scale_y_continuous(mf_labeller(dep)) +
                        #theme_bw() +
                        theme (axis.text.x = element_text(angle = 0, hjust = 0.5)) +
                   #print(length(levels(Tuk$groups$trt)))
                    annotate("text", x=c(1:length(levels(Tuk$groups$trt))), y=-Inf,vjust=-1, label=round(x=Tuk$groups$means, digits=2),size=4) +
                    annotate("text", x=c(1:length(levels(Tuk$groups$trt))), y=Inf,vjust=1, label=Tuk$groups$M,size=4)
                   
              }
         
         
         # CHI-SQUARE: comparison when the dep. variable is categorical ----------------------
    
              # We must define the dataframe, the dependent variable (which must be categorical), and the independent variable,
              # which must also be categorical. First, there are some lines of code to subset the dataframe and merge some categories
              # of the indep. variables (this part is specific of this study)
              # 
              # Then, we generate a table with the frequency of observations across levels of the indep. variable and 
              # perform a Chi-square test to determine if distributions of the levels of the dependent are different across 
              # levels of the indep. NOTE: if some of the levels of the indep. has not enough values of any of the levels of     
              # the indep., the function will turn error. We must then comment the line in which we perform post-hoc pairwise comparisons,
              #  ("print(chisq.post.hoc(stats,control='bonferroni'))") and run the function again
              # 
              # Finally, we plot the repartition of the dep. variable across the levels of the factor using barplots. 
              # Since the chisq.post.hoc function doesn't automatically return letters, these must be added by hand, with the 'annotate' function  
    
              chisq <- function (dataframe, dep, indep, leyenda) {
                   library(scales)            
                   
                   # Subset data
                   data<-subset(dataframe, !is.na(dataframe[indep]))
                   data<-subset(data, !is.na(data[dep]))
                   
                   if (dep == "Gender" | indep == "Gender") {
                        data<-data[data$Gender == "Male" | data$Gender == "Female",]
                        data$Gender <- factor (data$Gender) }
                   if (dep == "Forest_Type" | indep == "Forest_Type") {
                        data<-data[data$Forest_Type == "Boreal forest" | data$Forest_Type == "Temperate forest" | data$Forest_Type == "Mixed / Both", ]
                        data$Forest_Type <- factor(data$Forest_Type)   }
                   if (dep == "Stakeholder" | indep == "Stakeholder" ){
                        #data$Stakeholder[data$Stakeholder == "NGO"] <- "Consulting"
                        #data$Stakeholder[data$Stakeholder == "Student"] <- NA               
                        #data$Stakeholder[data$Stakeholder == "Consulting"] <- "Industry"
                        data<-subset(data, !is.na(data$Stakeholder))
                        data$Stakeholder <- factor(data$Stakeholder) }
                   if (dep == "Politics" | indep == "Politics") {
                        data$Politics <- factor(data$Politics)  }
                   
    #                if (dep == "Awareness" | indep == "Awareness") {
    #                     data<-data[data$Awareness == "Yes" | data$Awareness == "No",] }
    #                
    #                if (dep == "Consider" | indep == "Consider") {
    #                     data<-data[data$Consider == "Yes" | data$Consider == "No",] }      
    # 
    #                if (dep == "Priority" | indep == "Priority") {
    #                     data<-data[data$Priority == "Yes" | data$Priority == "No",] }  
    #                
                   
                   # Chi square and post-hoc comparison with Bonferroni corrections
                   fr <- paste0("~",indep, "+",dep)
                   stats <- xtabs(fr, data=data)
                   #print(prop.table(stats,margin=1))
                   
                   #print(summary(stats))
                   if( (dep == "Stakeholder" & indep =="Province") | (indep == "Stakeholder" & dep =="Province"))
                        {   # print(summary(stats)) 
                   } else {
                        #print(summary(stats)) 
                        #print(chisq.post.hoc(stats,control='bonferroni')) 
                   }
                   
                   # Plot results
                   ggplot(data,aes_string(x = indep,fill = dep)) + 
                        geom_bar(position = "fill", stat="count") + 
                        scale_y_continuous(name="Percentage", expand = c(0,0), labels=percent_format()) +
                        scale_x_discrete((indep)) +
                        scale_fill_manual(values=brewercolors(length(levels(data[[dep]])))   ) +
                        theme_bw() +
                        theme (axis.text.x = element_text(angle = 0, hjust = 0.5, size=12),
                               axis.text.y = element_text(size=11),
                               axis.title=element_text(size = 14,face="bold"),
                               panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                               legend.position=leyenda)
              }  
        
              wtd_chisq <- function (dataframe, dep, indep, leyenda) {
              library(scales)            
              
              # Subset data
              data<-subset(dataframe, !is.na(dataframe[indep]))
              data<-subset(data, !is.na(data[dep]))
              
              if (dep == "Gender" | indep == "Gender") {
                   data<-data[data$Gender == "Male" | data$Gender == "Female",]
                   data$Gender <- factor (data$Gender) }
              if (dep == "Forest_Type" | indep == "Forest_Type") {
                   data<-data[data$Forest_Type == "Boreal forest" | data$Forest_Type == "Temperate forest" | data$Forest_Type == "Mixed / Both", ]
                   data$Forest_Type <- factor(data$Forest_Type)   }
              if (dep == "Stakeholder" | indep == "Stakeholder" ){
                   #data$Stakeholder[data$Stakeholder == "NGO"] <- "Consulting"
                   #data$Stakeholder[data$Stakeholder == "Student"] <- NA               
                   #data$Stakeholder[data$Stakeholder == "Consulting"] <- "Industry"
                   data<-subset(data, !is.na(data$Stakeholder))
                   data$Stakeholder <- factor(data$Stakeholder) }
              if (dep == "Politics" | indep == "Politics") {
                   data$Politics <- factor(data$Politics)  }
              
              #                if (dep == "Awareness" | indep == "Awareness") {
              #                     data<-data[data$Awareness == "Yes" | data$Awareness == "No",] }
              #                
              #                if (dep == "Consider" | indep == "Consider") {
              #                     data<-data[data$Consider == "Yes" | data$Consider == "No",] }      
              # 
              #                if (dep == "Priority" | indep == "Priority") {
              #                     data<-data[data$Priority == "Yes" | data$Priority == "No",] }  
              #                
              
              # Chi square and post-hoc comparison with Bonferroni corrections
              fr <- paste0("Weights~",indep, "+",dep)
              stats <- xtabs(fr, data=data)
              
              print(prop.table(stats,margin=1))
              #print(summary(stats))
              if( (dep == "Stakeholder" & indep =="Province") | (indep == "Stakeholder" & dep =="Province"))
              {    print(summary(stats)) 
                   
              } else {
                   print(summary(stats)) 
                   #print(chisq.post.hoc(stats,control='bonferroni')) 
              }
              
              # Plot results
              ggplot(data,aes_string(x = indep,fill = dep)) + 
                   geom_bar(aes(weight = Weights), position = "fill", stat="count") + 
                   scale_y_continuous(name="Percentage",expand = c(0,0), labels=percent_format()) +
                   scale_x_discrete((indep)) +
                   scale_fill_manual(values=brewercolors(length(levels(data[[dep]])))   ) +
                   theme_bw() +
                   theme (axis.text.x = element_text(angle = 0, hjust = 0.5, size=12),
                          axis.text.y = element_text(size=11),
                          axis.title=element_text(size = 14,face="bold"),
                          panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                          legend.position=leyenda, legend.title=element_blank())
         }  
    
    
    
    
              
              
              
    require(plyr)
    
              
              
              
# Describe statistics -----------------------------------------------------         
 
      describe_stats <- function(dataframe, indep, columns, leyenda) {
         
             df <- data.frame(indep=character(),
                          mean=numeric(), 
                          sd=numeric(), 
                          N=numeric(),
                          se=numeric(),
                          Question=factor(),
                          Letter=character()) 
         colnames(df)[1]<- indep
         
         for (i in columns) {
              dep<-colnames(dataframe)[i]
              print(dep)
              
              # Subset data
              data<-subset(dataframe, !is.na(dataframe[indep]))
              data<-subset(data, !is.na(data[dep]))
              
              if (indep == "Gender") {
                   data<-data[data$Gender == "Male" | data$Gender == "Female",]
                   data$Gender <- factor (data$Gender) }
              if (indep == "Forest_Type") {
                   data<-data[data$Forest_Type == "Boreal forest" | data$Forest_Type == "Temperate forest" | data$Forest_Type == "Mixed / Both", ]
                   data$Forest_Type <- factor(data$Forest_Type)   }
              if (indep == "Stakeholder") {
                   data$Stakeholder[data$Stakeholder == "NGO"] <- NA
                   #data$Stakeholder[data$Stakeholder == "Student"] <- NA               
                   #data$Stakeholder[data$Stakeholder == "Consulting"] <- "Industry"
                   data<-subset(data, !is.na(data$Stakeholder))
                   data$Stakeholder <- factor(data$Stakeholder) }
              if (indep == "Politics") {
                   data$Politics[data$Politics=="7"] <- "6"               
                   data$Politics <- factor(data$Politics)  }
                   
              
              
              
              
              # Create the dataset with the summarized data
         
              z <- ddply(data, indep, .fun = function(xx){
                   c(mean = mean(xx[,dep],na.rm=TRUE),
                     sd = sd(xx[,dep],na.rm=TRUE),
                     N= sum(!is.na(xx[,dep]))) })
              
                   z$se<- z$sd/sqrt(z$N)
                   z$Question <-colnames(dataframe)[i]  
                        
              ## Analyze the data 
              frm <- paste(dep,indep,sep="~")
              anova.test <- aov(formula(frm), data=data)
              print(summary(anova.test))
              Tuk <- HSD.test (anova.test, trt=indep)
              
    #           #if(indep=="Province"){
    #            #    Tuk$groups$trt <- factor (Tuk$groups$trt, levels=c("British Columbia", "Alberta         ", "Ontario         ",
    #             #                                                      "Quebec          ", "New Brunswick   "))}
    #           
    #           if(indep=="Stakeholder"){
    #                Tuk$groups$trt <- factor (Tuk$groups$trt, levels=c("Federal Gov.   ", "Provincial Gov.","Consulting     ",
    #                                                                   "Industry       ",  "Academia       ", "Student        ", "NGO            "))}
    #           if(indep=="Education"){
    #                Tuk$groups$trt <- factor (Tuk$groups$trt, levels=c("High school     ", "Bachelors Degree", "Masters Degree  ", "Doctorate       " ))}
    #           
              Tuk$groups<-Tuk$groups[order(Tuk$groups$trt,na.last=T),]
              print(Tuk$groups)
              z$Letter <- Tuk$groups$M
              
         df<-rbind(df,z)  }
         
         
         df$Question<- mf_label_number(df$Question)
         df$Question <- as.factor(df$Question)
         df$z <- seq(from=length(df$mean), to=1,by=-1)
         df$x3<-reorder(df$Question,df$z) 
         df$ymin <- df$mean - 1.5*df$se
         df$ymax <- df$mean + 1.5*df$se
         
         #write.table(df,file="./Data/dftest.txt",sep="\t", row.names=F)
         
         # Graphical representation
         p <- ggplot(df, aes_string(x="x3", y="mean", ymin="ymin", ymax="ymax", colour=indep)) +
              geom_pointrange(size=1.15,position=position_dodge(width=0.8)) +
              scale_y_continuous(breaks=1:7, name="",limits=c(0.8,7.2), 
                                 labels=(c("Strongly\ndisagree","2","3","Unsure", "5","6","Strongly\nagree"))) +
              scale_color_manual( values=brewercolors(length(levels(data[[indep]])))) +
              theme_bw() +
               theme(axis.text.x = element_text(size=13, color="black"),
                     axis.text.y = element_text( size=14, color="black", hjust=0), legend.position = leyenda) +
              coord_flip() +
              scale_x_discrete (name="")        
         
         return(p)
    }
         
      wtd_describe_stats <- function(dataframe, indep, columns, leyenda) {
         
              # Create an empty dataframe
              df <- data.frame(indep=character(),
                          mean=numeric(), 
                          sd=numeric(), 
                          N=numeric(),
                          se=numeric(),
                          Question=factor(),
                          Letter=character()) 
              colnames(df)[1]<- indep
         
             # Define the working dataset as a function of dependent and indep. variables
              for (i in columns) {
                   dep<-colnames(dataframe)[i]
                   print(dep)
                   
                   # Subset data
                   data<-subset(dataframe, !is.na(dataframe[indep]))
                   data<-subset(data, !is.na(data[dep]))
                   
                   if (indep == "Gender") {
                        data<-data[data$Gender == "Male" | data$Gender == "Female",]
                        data$Gender <- factor (data$Gender) }
                   if (indep == "Forest_Type") {
                        data<-data[data$Forest_Type == "Boreal forest" | data$Forest_Type == "Temperate forest" | data$Forest_Type == "Mixed / Both", ]
                        data$Forest_Type <- factor(data$Forest_Type)   }
                   if (indep == "Stakeholder") {
                        data$Stakeholder[data$Stakeholder == "NGO"] <- NA
                        #data$Stakeholder[data$Stakeholder == "Student"] <- NA               
                        #data$Stakeholder[data$Stakeholder == "Consulting"] <- "Industry"
                        data<-subset(data, !is.na(data$Stakeholder))
                        data$Stakeholder <- factor(data$Stakeholder) }
                   if (indep == "Politics") {
                        data$Politics[data$Politics=="7"] <- "6"               
                        data$Politics <- factor(data$Politics)  }
                   
                   
              # Create the dataset with the summarized data (weighted mean and variance)
              z <- ddply(data, indep, .fun = function(xx){
                   c(mean = wtd.mean(xx[,dep],w=xx[,"Weights"],na.rm=TRUE),
                     sd = sqrt(wtd.var(xx[,dep],w=xx[,"Weights"],na.rm=TRUE)),
                     N= sum(!is.na(xx[,dep]))) })
              
              z$se<- z$sd/sqrt(z$N)
              z$Question <-colnames(dataframe)[i] 
              
              
              ## Analyze the data 
              frm <- paste(dep,indep,sep="~")
              anova.test <- aov(formula(frm), data=data, weights=Weights)
              Tuk<-HSD.test (anova.test, trt=indep)
           
              Tuk$groups<-Tuk$groups[order(Tuk$groups$trt,na.last=T),]
              Tuk$groups$wtd.mean<-z$mean
              
              print(Tuk$groups)
              z$Letter <- Tuk$groups$M
              df<-rbind(df,z)  }
         
         df$Question<- mf_labeller(df$Question)
         df$Question <- as.factor(df$Question)
         df$z <- seq(from=length(df$mean), to=1,by=-1)
         df$x3<-reorder(df$Question,df$z) 
         df$ymin <- df$mean - 1.96*df$se
         df$ymax <- df$mean + 1.96*df$se
         
         # Graphical representation
        
          
          p<- ggplot(df, aes_string(x="x3", y="mean", ymin="ymin", ymax="ymax", colour=indep)) +
              geom_pointrange(size=1.15,position=position_dodge(width=0.8)) +
              scale_y_continuous(breaks=1:7, name="",limits=c(0.8,7.2), 
                                 labels=(c("Strongly\ndisagree","2","3","Unsure", "5","6","Strongly\nagree"))) +
              scale_color_manual( values=brewercolors(length(levels(data[[indep]])))) +
              theme_bw() +
              theme(axis.text.x = element_text(size=13, color="black"),
                    axis.text.y = element_text( size=14, color="black", hjust=0), legend.position = leyenda) +
              coord_flip() +
              scale_x_discrete (name="")    
          
    
         
         return(p)
    }
    
    
     describe_simple <- function(dataframe, columns) {
         df <- data.frame( mean=numeric(), 
                          sd=numeric(), 
                          N=numeric(),
                          se=numeric(),
                          Question=factor(),
                          Letter=character()) 
              
              # Create the dataset with the summarized data
             
              # Transpose the database from 'wide' to "long' format
              practices<-melt(data=dataframe,id.vars=c("Response.ID","Province","Stakeholder","Gender","Age","Education","Forest_Type","Politics","nep"),measure.vars=columns)
              names(practices)[names(practices) == 'variable'] <- 'Practice'         
    
              ## Analyze the data 
              anova.test<-aov(formula(value~Practice), data=practices)
              Tuk<-HSD.test (anova.test, trt='Practice')
              Tuk$groups$Practice = row.names(Tuk$groups)
              
              Tuk$groups <- Tuk$groups %>%
                  mutate(rank = dense_rank(-value))
              print(Tuk$groups)
    }
    
    wtd_describe_simple <- function(dataframe, columns) {
              df <- data.frame( mean=numeric(), 
                                sd=numeric(), 
                                N=numeric(),
                                se=numeric(),
                                Question=factor(),
                                Letter=character()) 
              
              # Create the dataset with the summarized data
              
              #z <- data.frame(describe(dataframe[,min(columns):max(columns)]))
              #z$Question <-colnames(dataframe)
              #print(z)
              
              #      for (i in min(columns): max(columns)) {
              #           dep<-colnames(dataframe)[i]
              #           print(dep)
              #      
              #           # Subset data
              #           data<-subset(dataframe, !is.na(dataframe[dep]))
              
              # Transpose the database from 'wide' to "long' format
              practices<-melt(data=dataframe,
                              id.vars=c("Response.ID","Province","Stakeholder","Gender","Age","Education","Forest_Type","Politics","nep", "Weights"),
                              measure.vars=columns,
                              variable.name="Practice",
                              value.name="value")
              
              ## Analyze the data 
              
              anova.test<-aov(formula(value~Practice), data=practices,weights=Weights)
              
                # Merge mean and tukey groups with weighted means
                   
                   # Tuk HSD groups (and delete white space in name for merging)
                   Tuk<-HSD.test (anova.test, trt='Practice')     
                   Tuk$groups$CleanTrat<-trimws(Tuk$groups$trt, which = "r")
              
                   # Weighted means
                   wtd_means<- ddply(practices, .(Practice), function(z) wtd.mean(z$value, z$Weights))
                   names(wtd_means)[names(wtd_means)=="V1"] <- "wtd_mean"          
                
                   # Merge both tables and print nice result
                   result<-merge(x=Tuk$groups,y=wtd_means, by.x="CleanTrat",by.y="Practice")
                   ordered_result <- result[with(result, order(-wtd_mean)),]
                   print(ordered_result[,c("CleanTrat","wtd_mean","M","means")])       
           
         }
         
    
              
         # NEPFUNCTION: variation of responses across NEP, faceted by categ. variable ----------------------
    
         
         simplenep <- function (data) {
         
         data_plot <- ggplot(na.exclude(data), aes(x=nep, y=value, color=factor(Statement),fill=factor(Statement))) +
              geom_smooth(method=lm, level=0.95,size=1.5) +
              scale_x_continuous(name="New Ecological Paradigm") +
              scale_y_continuous(name="",limits=c(0.5,7.5), breaks=c(1:7),
                                 labels=c("Strongly\ndisagree", "Disagree","Somewhat\ndisagree","Unsure",
                                          "Somewhat\nagree","Agree","Strongly\nagree")) +
              scale_color_manual(name="",labels=mf_labeller(levels(data$Statement)),
                                 values=brewercolors(length(levels(data$Statement)))) +
              scale_fill_manual(name="",labels=mf_labeller(levels(data$Statement)),
                                values=brewercolors(length(levels(data$Statement))))+
              theme_bw() +
              theme(axis.text.x = element_text(size=9, color="black"),
                    axis.text.y = element_text( size=8, color="black", hjust=0),
                    legend.text = element_text(size=8)) 
         return(data_plot)
         }
         
         
         nepfunction <- function (data) {
              data_plot <- ggplot(na.exclude(data), aes(x=nep, y=value, color=factor(Statement),fill=factor(Statement))) +
                   geom_smooth(method=lm, level=0.95,size=1.5) +
                   scale_x_continuous(name="New Ecological Paradigm",limits=c(45,105), breaks=seq(40,110,10)) +
                   scale_y_continuous(name="",limits=c(0.5,7.5), breaks=c(1:7),
                                      labels=c("Strongly\ndisagree", "Disagree","Somewhat\ndisagree","Unsure",
                                               "Somewhat\nagree","Agree","Strongly\nagree")) +
                   scale_color_manual(name="",labels=mf_labeller(levels(data$Statement)),
                                      values=brewercolors(length(levels(data$Statement)))) +
                   scale_fill_manual(name="",labels=mf_labeller(levels(data$Statement)),
                                     values=brewercolors(length(levels(data$Statement))))+
                   theme_bw() +
                   theme(axis.text.x = element_text(size=9, color="black"),
                         axis.text.y = element_text( size=8, color="black", hjust=0),
                         legend.text = element_text(size=8)) 
              return(data_plot)
         }
    
         
        ## T_TEST; to compare two samples via t-test based on mean, sd, and sample size  ######
    ## Used to compare our results with those from Williamson et al. 2005
    
    
    # m1, m2: the sample means
    # s1, s2: the sample standard deviations
    # n1, n2: the same sizes
    # m0: the null value for the difference in means to be tested for. Default is 0. 
    # equal.variance: whether or not to assume equal variance. Default is FALSE. 
         t.test2 <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
              {
                   if( equal.variance==FALSE ) 
                   {
                        se <- sqrt( (s1^2/n1) + (s2^2/n2) )
                        # welch-satterthwaite df
                        df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
                   } else
                   {
                        # pooled standard deviation, scaled by the sample sizes
                        se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
                        df <- n1+n2-2
                   }      
                   t <- (m1-m2-m0)/se 
                   dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))    
                   names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
                   return(dat) 
              }
    
    
    #t.test2(3.03, 2.40,1.30,1.02,n1=1121,n2=53 )
         

# Multiplot ---------------------------------------------------------------

 ## Combines several plots within the ggplot language
         
         multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
              library(grid)
              
              # Make a list from the ... arguments and plotlist
              plots <- c(list(...), plotlist)
              
              numPlots = length(plots)
              
              # If layout is NULL, then use 'cols' to determine layout
              if (is.null(layout)) {
                   # Make the panel
                   # ncol: Number of columns of plots
                   # nrow: Number of rows needed, calculated from # of cols
                   layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                    ncol = cols, nrow = ceiling(numPlots/cols))
              }
              
              if (numPlots==1) {
                   print(plots[[1]])
                   
              } else {
                   # Set up the page
                   grid.newpage()
                   pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                   
                   # Make each plot, in the correct location
                   for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                   }
              }
         }
     
     
    


# mf_labeller -------------------------------------------------------------

         mf_labeller <- function( value){
             value <- as.character(value)
             #cc_impacts
             value[value== "Human_Cause"] <-        "Human activities are the primary\n  cause of climate change"
             value[value=="Exagerated"] <-                           "Climate change impacts are  \n  exaggerated"
             value[value=="Inconclusive"] <-         "Generally, the science of climate \n  change is inconclusive"
             value[value== "Threat"] <-  "Climate change represents a \n  serious threat to my family and me"
             value[value=="Understand"] <-                "I do not understand the impacts\n  of climate change"
             value[value=="Ample_time" ] <-                  "There is ample time to adapt\n  to climate change"
             #cc_forests
             value[value=="Current_Impact"]<-                          "CC is currently having  a significant impact\n on forest ecosystems "
             value[value=="Fiftyyears" ] <- "Within the next 50 years CC is going to have\n a significant impact on forest ecosystems"
             value[value=="Hundredyears" ] <- "Within the next 100 years CC is going to have\n a significant impact on forest ecosystems"
             value[value=="Predictable" ] <-                                         "CC effects on forest ecosystems are \npredictable"
             value[value=="Certainty" ] <-                         "There is certainty about the effects of CC\n on forest ecosystems"
             value[value=="Managers_Understand" ]<-                 "The effects of CC on forest ecosystems\n are understood by forest managers"
             value[value=="Managers_Control" ]<-              "Forest managers have the ability to control\n CC impacts on forest ecosystems"
             #current_practices
             value[value=="Legislation" ] <-                     "Current forest legislation takes into account \nthe impacts of CC on forest ecosystems"
             value[value=="Timber_Supply" ]<-                                            "CC is properly incorporated into calculations \nof timber supply"
             value[value=="Current_Sufficient" ] <- "The forest practices currently implemented are\nsufficient to face the impacts of CC on forests"
             value[value=="New_Practices" ]<-     "We need to create and design new forest practices\n to deal with the impacts of CC on forests"
             value[value=="Wait" ] <-                                                "We should wait to see the impacts of CC on forests\n before implementing adaptive practices"
             #ecosystem
             value[value=="Fully_Functioning" ] <-                                           "A fully functioning ecosystem will be less vulnerable\nto the unknown risks of CC"
             value[value=="Reduce_Discrepancy"]<-        "Reduce the discrepancy between managed and natural\n forests is the best way to ensure the adaptation of forests\n to CC"
             value[value=="Lack_Info_Reference" ] <-                                                                                           "We lack enough information to define the natural\n forest of reference"
             value[value=="Past_is_no_goal" ] <-               "Given the expected impacts of CC, restoring the forests\n back to past conditions may not be an appropriate goal\n for forest management"
             value[value=="Mimick_Disturbances" ] <-     "Current species have co-evolved with the recent past\n disturbance regime and by mimicking these disturbances\n we can maintain their habitats in the future"
             value[value=="Retention" ] <-                   "Retention of structural elements is enough to ensure\n the adaptation of forests to CC"
             #organizations
             value[value=="Academia (faculty, research associate, postdoctoral fellow)" ] <- "Academia"
             value[value=="Consulting" ] <- "Consulting"
             value[value=="Environmental non-governmental organization" ] <- "NGO"
             value[value=="Federal Government" ] <- "Federal Gov."
             value[value=="First Nations Organization" ] <- "First Nations"
             value[value=="Graduate student" ] <- "Student"
             value[value=="Industry / Industry Association" ] <- "Industry"
             value[value=="Provincial Government" ] <- "Provincial Gov."
             #dependent variables
             value[value=="nep" ] <- "New Ecological Paradigm"
             value[value=="Politics" ] <- "Political View"
             #adaptive practices
             value[value=="Natural_Regeneration" ] <- "Allow forests to regenerate naturally \nfollowing disturbance"
             value[value=="Old_growth_forests" ] <- "Maintain large areas of old growth forests"
             value[value=="Reduce_differences_natural" ] <- "Reduce the differences between managed \nand natural forests"
             value[value=="Species_diversification" ] <- "Promote species diversification \n(either at the stand or landscape level)"
             value[value=="Translocate_Populations" ] <- "Active translocation of populations of a given species\n within that species' current range"
             value[value=="Assisted_migration" ] <- "Active translocation of species outside\n its current range (i.e., assisted migration)"
             value[value=="Seed_transfer" ] <- "Modify seed transfer zones/seed provenances"
             value[value=="Continuous_cover" ] <- "Modify management to maintain continuous tree cover"
             value[value=="Provenance_tests" ] <- "Implement long-term and multi-species provenance\n tests along latitudinal gradients"
             value[value=="Diverse_experiments" ] <- "Diversify risk by implementing a diversity \nof small-scale experiments"
             value[value=="Shorten_rotation" ] <- "Shorten silvicultural cycles (rotation length or \nmore frequent partial cutting cycle)"
             value[value=="Thinning" ] <- "Use thinning to reduce stand density and favor\n surviving individuals"
             value[value=="Salvage_logging" ] <- "Increase the amount of salvage logging \nafter disturbances"
             value[value=="Promote_Retention" ] <- "Increase the retention of structural elements\n (green trees, snags)"
             value[value=="Timber_supply" ] <- "Include climate change in timber supply analysis"
             value[value=="Harvest_vulnerable_stands" ] <- "Prioritize the harvesting of stands most vulnerable \nto natural disturbances"
             #Barriers to implementation
             value[value=="Uncertainties" ] <- "There are too many uncertainties on climate change impacts"
             value[value=="Practitioners_impact_low" ] <- "Most practitioners think that the likely impact of CC\n will be low"
             value[value=="Lack_scientific_support" ] <- "Lack of scientific support and knowledge transfer\n to forest practitioners"
             value[value=="Lack_info_local_scale" ] <- "There is lack of information about CC impacts\n at local and regional scales"
             value[value=="Lack_solutions" ] <- "Lack of apparent tangible adaptive solutions\n (ground level practices)"
             value[value=="Lack_tools" ] <- "Lack of adequate tools (procedures, techniques) for the \n inclusion of climate change in forest planning"
             value[value=="Lack_workforce" ] <- "Lack of skilled and specialized workforce \nto use the tools needed for the inclusion of CC \nin forest planning (e.g. models)"
             value[value=="Costs" ] <- "The cost of implementing adaptive measures weakens \nthe economic situation of enterprises"
             value[value=="Forest_companies_reluctant" ] <- "Forest companies are reluctant to change\n their current management"
             value[value=="Lack_funding" ] <- "Lack of funding for the development of adaptation strategies"
             value[value=="Forest_certification" ] <- "Excessive rigidity of most standards of forest certification"
             value[value=="Rigid_legislation" ] <- "Excessively rigid legislation and public policies"
             value[value=="Lack_common_view" ] <- "Lack of a common, coherent view for all the forest sector"
             value[value=="Lack_of_policies" ] <- "Lack of policies that support innovation/diversification of practices"
             value[value=="Other_imminent_issues" ] <- "An overwhelming collection of other imminent forestry issues\n compete with CC challenges (e.g. endangered species, competition for landbase, water values, etc.)"
             
             return(value)}
         
         
         
         
         mf_label_number <- function( value){
             value <- as.character(value)
             #cc_impacts
             value[value== "Human_Cause"] <-        "1.1 Human activities are the primary\n  cause of climate change"
             value[value=="Exagerated"] <-                           "1.2 Climate change impacts are  \n  exaggerated"
             value[value=="Inconclusive"] <-         "1.3 Generally, the science of climate \n  change is inconclusive"
             value[value== "Threat"] <-  "1.4 Climate change represents a \n  serious threat to my family and me"
             value[value=="Understand"] <-                "1.5 I do not understand the impacts\n  of climate change"
             value[value=="Ample_time" ] <-                  "1.6 There is ample time to adapt\n  to climate change"
             #cc_forests
             value[value=="Current_Impact"]<-                           "2.1 CC is currently having  a significant\n impact on forest ecosystems "
             value[value=="Fiftyyears" ] <-  "2.2 Within the next 50 years CC is going\n  to have a significant impact\n  on forest ecosystems"
             value[value=="Hundredyears" ] <- "2.3 Within the next 100 years CC is going\n  to have a significant impact\n  on forest ecosystems"
             value[value=="Predictable" ] <-                                          "2.4 CC effects on forest ecosystems are \npredictable"
             value[value=="Certainty" ] <-                          "2.5 There is certainty about the effects\n  of CC on forest ecosystems"
             value[value=="Managers_Understand" ]<-                  "2.6 The effects of CC on forest \n ecosystems are understood by \n forest managers"
             value[value=="Managers_Control" ]<-               "2.7 Forest managers have the ability\n  to control CC impacts on forest\n  ecosystems"
             #current_practices
             value[value=="Legislation" ] <-                     "3.1 Current forest legislation takes\n  into account the impacts\n  of CC on forest ecosystems"
             value[value=="Timber_Supply" ]<-                                            "3.2 CC is properly incorporated into\n  calculations of timber supply"
             value[value=="Current_Sufficient" ] <- "3.3 The forest practices currently \n implemented are sufficient to face \n the impacts of CC on forests"
             value[value=="New_Practices" ]<-     "3.4 We need to create and design \n new forest practices to deal with the\n impacts of CC on forests"
             value[value=="Wait" ] <-   "3.5 We should wait to see the impacts of CC on forests\n before implementing adaptive practices"
             #ecosystem
             value[value=="Fully_Functioning" ] <-                                           "A fully functioning ecosystem will be less vulnerable\nto the unknown risks of CC"
             value[value=="Reduce_Discrepancy"]<-        "Reduce the discrepancy between managed and natural\n forests is the best way to ensure the adaptation of forests\n to CC"
             value[value=="Lack_Info_Reference" ] <-                                                                                           "We lack enough information to define the natural\n forest of reference"
             value[value=="Past_is_no_goal" ] <-               "Given the expected impacts of CC, restoring the forests\n back to past conditions may not be an appropriate goal\n for forest management"
             value[value=="Mimick_Disturbances" ] <-     "Current species have co-evolved with the recent past\n disturbance regime and by mimicking these disturbances\n we can maintain their habitats in the future"
             value[value=="Retention" ] <-                   "Retention of structural elements is enough to ensure\n the adaptation of forests to CC"
             #organizations
             value[value=="Academia (faculty, research associate, postdoctoral fellow)" ] <- "Academia"
             value[value=="Consulting" ] <- "Consulting"
             value[value=="Environmental non-governmental organization" ] <- "NGO"
             value[value=="Federal Government" ] <- "Federal Gov."
             value[value=="First Nations Organization" ] <- "First Nations"
             value[value=="Graduate student" ] <- "Student"
             value[value=="Industry / Industry Association" ] <- "Industry"
             value[value=="Provincial Government" ] <- "Provincial Gov."
             #dependent variables
             value[value=="nep" ] <- "New Ecological Paradigm"
             value[value=="Politics" ] <- "Political View"
             
             return(value)}
         
         
         mf_label_number_complete <- function( value){
             value <- as.character(value)
             #cc_impacts
             value[value== "Human_Cause"] <-        "1.1 Human activities are the primary cause of climate change"
             value[value=="Exagerated"] <-                           "1.2 Climate change impacts are exaggerated"
             value[value=="Inconclusive"] <-         "1.3 Generally, the science of climate change is inconclusive"
             value[value== "Threat"] <-  "1.4 Climate change represents a  serious threat to my family and me"
             value[value=="Understand"] <-                "1.5 I do not understand the impacts of climate change"
             value[value=="Ample_time" ] <-                  "1.6 There is ample time to adapt to climate change"
             #cc_forests
             value[value=="Current_Impact"]<-                           "2.1 CC is currently having  a significant impact on forest ecosystems "
             value[value=="Fiftyyears" ] <-  "2.2 Within the next 50 years CC is going  to have a significant impact  on forest ecosystems"
             value[value=="Hundredyears" ] <- "2.3 Within the next 100 years CC is going  to have a significant impact  on forest ecosystems"
             value[value=="Predictable" ] <-                                          "2.4 CC effects on forest ecosystems are predictable"
             value[value=="Certainty" ] <-                          "2.5 There is certainty about the effects of CC on forest ecosystems"
             value[value=="Managers_Understand" ]<-                  "2.6 The effects of CC on forest  ecosystems are understood by  forest managers"
             value[value=="Managers_Control" ]<-               "2.7 Forest managers have the ability  to control CC impacts on forest  ecosystems"
             #current_practices
             value[value=="Legislation" ] <-                     "3.1 Current forest legislation takes  into account the impacts  of CC on forest ecosystems"
             value[value=="Timber_Supply" ]<-                                            "3.2 CC is properly incorporated into  calculations of timber supply"
             value[value=="Current_Sufficient" ] <- "3.3 The forest practices currently  implemented are sufficient to face the impacts of CC on forests"
             value[value=="New_Practices" ]<-     "3.4 We need to create and design  new forest practices to deal with the impacts of CC on forests"
             value[value=="Wait" ] <-   "3.5 We should wait to see the impacts of CC on forests before implementing adaptive practices"
             #ecosystem
             value[value=="Fully_Functioning" ] <-                                           "A fully functioning ecosystem will be less vulnerableto the unknown risks of CC"
             value[value=="Reduce_Discrepancy"]<-        "Reduce the discrepancy between managed and natural forests is the best way to ensure the adaptation of forests to CC"
             value[value=="Lack_Info_Reference" ] <-                                                                                           "We lack enough information to define the natural forest of reference"
             value[value=="Past_is_no_goal" ] <-               "Given the expected impacts of CC, restoring the forests back to past conditions may not be an appropriate goal for forest management"
             value[value=="Mimick_Disturbances" ] <-     "Current species have co-evolved with the recent past disturbance regime and by mimicking these disturbances we can maintain their habitats in the future"
             value[value=="Retention" ] <-                   "Retention of structural elements is enough to ensure the adaptation of forests to CC"
             #organizations
             value[value=="Academia (faculty, research associate, postdoctoral fellow)" ] <- "Academia"
             value[value=="Consulting" ] <- "Consulting"
             value[value=="Environmental non-governmental organization" ] <- "NGO"
             value[value=="Federal Government" ] <- "Federal Gov."
             value[value=="First Nations Organization" ] <- "First Nations"
             value[value=="Graduate student" ] <- "Student"
             value[value=="Industry / Industry Association" ] <- "Industry"
             value[value=="Provincial Government" ] <- "Provincial Gov."
             #dependent variables
             value[value=="nep" ] <- "New Ecological Paradigm"
             value[value=="Politics" ] <- "Political View"
             
             return(value)}
         
         

