

source("./Rcode/01Import_Clean_Data.R")
source("./Rcode/01bAssignWeights.R")
source("./Rcode/survey_functions.R")


# Define groups of variables and create tables with descriptive statistics -----------------------------------------------------------


##### By Stakeholder #####

# General Impacts of CC
pdf(file="./Figures/Exploratory/Likert/01CC_Impacts_02Stakeholder.pdf", width=11.5, height=6)
describe_stats(survey,"Stakeholder",19:24,  c(0.8,0.15)) +
     #last_plot() +
     annotate(geom="text",x=c(5.7,5.82,5.94,6.06,6.18,6.3),y=rep(6.8,6), 
              label=c("ab","b","a","c","ab","a"), size=3) +
     annotate(geom="text",x=c(4.7,4.82,4.94,5.06,5.18,5.3),y=rep(3.8,6),
              label=c("abc","ab","bc","a","bc","c"), size=3) +
     annotate(geom="text",x=c(3.7,3.82,3.94,4.06,4.18,4.3),y=rep(4.0,6), 
              label=c("b","a","b","a","ab","b"), size=3) +
     annotate(geom="text",x=c(2.7,2.82,2.94,3.06,3.18,3.3),y=rep(5.5,6), 
              label=c("ab","ab","a","b","ab","a"), size=3) +
     annotate(geom="text",x=c(1.7,1.82,1.94,2.06,2.18,2.3),y=rep(3.5,6), 
              label=c("b","a","b","a","ab","b"), size=3) +
     annotate(geom="text",x=c(0.7,0.82,0.94,1.06,1.18,1.3),y=rep(3.6,6),
              label=c("abc","ab","bc","a","abc","c"), size=3) 
dev.off()

# Impacts of CC on Forests
pdf(file="./Figures/Exploratory/Likert/02CC_Forests_02Stakeholder.pdf", width=11.5, height=6)
describe_stats(survey,"Stakeholder",25:31,c(0.8,0.15)) +
     #last_plot() +
     annotate(geom="text",x=c(6.7,6.82,6.94,7.06,7.18,7.3),y=rep(6,6), 
              label=c("ab","b","a","c","ab","a"), size=3) +
     annotate(geom="text",x=c(5.7,5.82,5.94,6.06,6.18,6.3),y=rep(6.5,6), 
              label=c("ab","b","a","c","ab","a"), size=3) +
     annotate(geom="text",x=c(4.7,4.82,4.94,5.06,5.18,5.3),y=rep(6.8,6),
              label=c("abc","bc","a","c","ab","a"), size=3) +
     annotate(geom="text",x=c(3.7,3.82,3.94,4.06,4.18,4.3),y=rep(4.5,6), 
              label=c("a","a","a","a","a","a"), size=3) +
     annotate(geom="text",x=c(2.7,2.82,2.94,3.06,3.18,3.3),y=rep(5,6), 
              label=c("ab","b","a","b","a","ab"), size=3) +
     annotate(geom="text",x=c(1.7,1.82,1.94,2.06,2.18,2.3),y=rep(4,6), 
              label=c("a","a","a","a","a","a"), size=3) +
     annotate(geom="text",x=c(0.7,0.82,0.94,1.06,1.18,1.3),y=rep(4,6),
              label=c("a","a","a","a","a","a"), size=3) 
dev.off()

# Current Practices
pdf(file="./Figures/Exploratory/Likert/03Current_Practices_02Stakeholder.pdf", width=11.5, height=6)
describe_stats(survey,"Stakeholder",32:36,  c(0.8,0.75)) +
     #last_plot() +
     annotate(geom="text",x=c(4.7,4.82,4.94,5.06,5.18,5.3),y=rep(3.6,6), 
              label=c("ab","ab","ab","ab","b","a"), size=3) +
     annotate(geom="text",x=c(3.7,3.82,3.94,4.06,4.18,4.3),y=rep(3.5,6), 
              label=c("ab","b","b","ab","ab","a"), size=3) +
     annotate(geom="text",x=c(2.7,2.82,2.94,3.06,3.18,3.3),y=rep(3.8,6), 
              label=c("b","b","b","a","b","b"), size=3) +
     annotate(geom="text",x=c(1.7,1.82,1.94,2.06,2.18,2.3),y=rep(6.5,6),
              label=c("ab","bc","a","c","abc","a"), size=3) +
     annotate(geom="text",x=c(0.7,0.82,0.94,1.06,1.18,1.3),y=rep(3.3,6),
              label=c("ab","ab","b","a","ab","b"), size=3) 
dev.off()

# Ecosystem
pdf(file="./Figures/Exploratory/Likert/04EcosystemManag_02Stakeholder.pdf", width=11.5, height=6)
describe_stats(survey,"Stakeholder",37:42,  c(0.8,0.15)) +
     #last_plot() +
     annotate(geom="text",x=c(5.7,5.82,5.94,6.06,6.18,6.3),y=rep(6.3,6), 
              label=c("ab","b","ab","b","b","a"), size=3) +
     annotate(geom="text",x=c(4.7,4.82,4.94,5.06,5.18,5.3),y=rep(5.4,6), 
              label=c("b","b","b","b","ab","a"), size=3) +
     annotate(geom="text",x=c(3.7,3.82,3.94,4.06,4.18,4.3),y=rep(5,6),
              label=c("a","a","a","a","a","a"), size=3) +
     annotate(geom="text",x=c(2.7,2.82,2.94,3.06,3.18,3.3),y=rep(6,6), 
              label=c("a","a","a","a","a","a"), size=3) +
     annotate(geom="text",x=c(1.7,1.82,1.94,2.06,2.18,2.3),y=rep(4.5,6), 
              label=c("a","a","a","a","a","a"), size=3) +
     annotate(geom="text",x=c(0.7,0.82,0.94,1.06,1.18,1.3),y=rep(3.4,6),
              label=c("a","a","a","a","a","a"), size=3) 
dev.off()


##### By Province #####

# General Impacts of CC
pdf(file="./Figures/Exploratory/Likert/01CC_Impacts_01Province.pdf", width=11.5, height=6)
wtd_describe_stats(survey,"Province",19:24, c(0.8,0.15)) +
     #last_plot() +
     annotate(geom="text",x=c(5.7,5.85,6,6.15,6.3),y=rep(7,5), 
              label=c("ab","b","a","a","b"), size=3.5) +
     annotate(geom="text",x=c(4.7,4.85,5,5.15,5.3),y=rep(4.7,5),
              label=c("a","a","a","a","a"), size=3.5) +
     annotate(geom="text",x=c(3.7,3.85,4,4.15,4.3),y=rep(5,5), 
              label=c("b","ab","ab","ab","a"), size=3.5) +
     annotate(geom="text",x=c(2.7,2.85,3,3.15,3.3),y=rep(7,5), 
              label=c("a","ab","a","b","ab"), size=3.5) +
     annotate(geom="text",x=c(1.7,1.85,2,2.15,2.3),y=rep(4.5,5), 
              label=c("a","a","a","a","a"), size=3.5) +
     annotate(geom="text",x=c(0.7,0.85,1,1.15,1.3),y=rep(4.5,5),
              label=c("b","a","ab","ab","a"), size=3.5) 
     dev.off()

# Impacts of CC on Forests
pdf(file="./Figures/Exploratory/Likert/02CC_Forests_01Province.pdf", width=11.5, height=6)
wtd_describe_stats(survey,"Province",25:31, c(0.8,0.15)) +
     #last_plot() +
     annotate(geom="text",x=c(6.7,6.85,7,7.15,7.3),y=rep(7,5),
              label=c("a","b","bc","bc","c"), size=3.5) +
     annotate(geom="text",x=c(5.7,5.85,6,6.15,6.3),y=rep(7,5),
              label=c("a","b","ab","b","c"), size=3.5) +
     annotate(geom="text",x=c(4.7,4.85,5,5.15,5.3),y=rep(7,5),
              label=c("a","bc","ab","ab","c"), size=3.5) +
     annotate(geom="text",x=c(3.7,3.85,4,4.15,4.3),y=rep(5.5,5), 
              label=c("a","a","a","a","a"), size=3.5) +
     annotate(geom="text",x=c(2.7,2.85,3,3.15,3.3),y=rep(6.5,5), 
              label=c("b","b","b","a","b"), size=3.5) +
     annotate(geom="text",x=c(1.7,1.85,2,2.15,2.3),y=rep(4.5,5), 
              label=c("a","a","a","a","a"), size=3.5) +
     annotate(geom="text",x=c(0.7,0.85,1,1.15,1.3),y=rep(5.1,5), 
              label=c("a","a","a","a","a"), size=3.5)
dev.off()

# Current Practices
pdf(file="./Figures/Exploratory/Likert/03Current_Practices_01Province.pdf", width=11.5, height=6)
wtd_describe_stats(survey,"Province",32:36, c(0.8,0.15)) +
     #last_plot() +
     annotate(geom="text",x=c(4.7,4.85,5,5.15,5.3),y=rep(4.8,5), 
              label=c("a","a","a","a","b"), size=3.5) +
     annotate(geom="text",x=c(3.7,3.85,4,4.15,4.3),y=rep(4.3,5), 
              label=c("a","a","a","a","a"), size=3.5) +
     annotate(geom="text",x=c(2.7,2.85,3,3.15,3.3),y=rep(4.3,5), 
              label=c("a","a","a","a","a"), size=3.5) +
     annotate(geom="text",x=c(1.7,1.85,2,2.15,2.3),y=rep(7,5), 
              label=c("a","ab","ab","ab","b"), size=3.5) +
     annotate(geom="text",x=c(0.7,0.85,1,1.15,1.3),y=rep(4.8,5), 
              label=c("b","ab","ab","b","a"), size=3.5) 
dev.off()

# Ecosystem
pdf(file="./Figures/Exploratory/Likert/04EcosystemManag_01Province.pdf", width=11.5, height=6)
wtd_describe_stats(survey,"Province",37:42, c(0.8,0.15)) +
     #last_plot() +
     annotate(geom="text",x=c(5.7,5.85,6,6.15,6.3),y=rep(7,5),
              label=c("ab","ab","a","b","b"), size=3.5) +
     annotate(geom="text",x=c(4.7,4.85,5,5.15,5.3),y=rep(6,5),
              label=c("a","a","a","a","a"), size=3.5) +
     annotate(geom="text",x=c(3.7,3.85,4,4.15,4.3),y=rep(6.5,5), 
              label=c("b","b","b","a","ab"), size=3.5) +
     annotate(geom="text",x=c(2.7,2.85,3,3.15,3.3),y=rep(7,5), 
              label=c("a","ab","ab","b","b"), size=3.5) +
     annotate(geom="text",x=c(1.7,1.85,2,2.15,2.3),y=rep(5.8,5), 
              label=c("b","a","ab","ab","ab"), size=3.5) +
     annotate(geom="text",x=c(0.7,0.85,1,1.15,1.3),y=rep(4,5), 
              label=c("a","a","a","a","a"), size=3.5)
dev.off()



##### By Gender #####

# General Impacts of CC
pdf(file="./Figures/Exploratory/Likert/01CC_Impacts_03Gender.pdf", width=11.5, height=6)
describe_stats(survey,"Gender",19:24) +
     #last_plot() +
     annotate(geom="text",x=c(5.8,6.2),y=rep(7,2), 
              label=c("a","b"), size=3.5) +
     annotate(geom="text",x=c(4.8,5.2),y=rep(4.5,2),
              label=c("b","a"), size=3.5) +
     annotate(geom="text",x=c(3.8,4.2),y=rep(4.5,2), 
              label=c("b","a"), size=3.5) +
     annotate(geom="text",x=c(2.8,3.2),y=rep(6.7,2), 
              label=c("a","b"), size=3.5) +
     annotate(geom="text",x=c(1.8,2.2),y=rep(4.3,2), 
              label=c("b","a"), size=3.5) +
     annotate(geom="text",x=c(0.8, 1.2),y=rep(4.5,2),
              label=c("b","a"), size=3.5) 
dev.off()

# Impacts of CC on Forests
pdf(file="./Figures/Exploratory/Likert/02CC_Forests_03Gender.pdf", width=11.5, height=6)
describe_stats(survey,"Gender",25:31) +
     #last_plot() +
     annotate(geom="text",x=c(6.8,7.2),y=rep(6.8,2), 
              label=c("a","b"), size=3.5) +
     annotate(geom="text",x=c(5.8,6.2),y=rep(7,2), 
              label=c("a","b"), size=3.5) +
     annotate(geom="text",x=c(4.8,5.2),y=rep(7,2),
              label=c("a","b"), size=3.5) +
     annotate(geom="text",x=c(3.8,4.2),y=rep(5.5,2), 
              label=c("a","a"), size=3.5) +
     annotate(geom="text",x=c(2.8,3.2),y=rep(5.7,2), 
              label=c("a","a"), size=3.5) +
     annotate(geom="text",x=c(1.8,2.2),y=rep(4.6,2), 
              label=c("a","a"), size=3.5) +
     annotate(geom="text",x=c(0.8, 1.2),y=rep(4.9,2),
              label=c("a","a"), size=3.5) 
dev.off()

# Current Practices
pdf(file="./Figures/Exploratory/Likert/03Current_Practices_03Gender.pdf", width=11.5, height=6)
describe_stats(survey,"Gender",32:36) +
     #last_plot() +
     annotate(geom="text",x=c(4.8,5.2),y=rep(4.7,2), 
              label=c("a","a"), size=3.5) +
     annotate(geom="text",x=c(3.8,4.2),y=rep(4.3,2), 
              label=c("a","b"), size=3.5) +
     annotate(geom="text",x=c(2.8,3.2),y=rep(4.1,2), 
              label=c("b","a"), size=3.5) +
     annotate(geom="text",x=c(1.8, 2.2),y=rep(7,2),
              label=c("a","b"), size=3.5) +
     annotate(geom="text",x=c(0.8, 1.2),y=rep(3.8,2),
              label=c("b","a"), size=3.5) 
dev.off()

# Ecosystem
pdf(file="./Figures/Exploratory/Likert/04EcosystemManag_03Gender.pdf", width=11.5, height=6)
describe_stats(survey,"Gender",37:42) +
     #last_plot() +
     annotate(geom="text",x=c(5.8,6.2),y=rep(7,2), 
              label=c("a","b"), size=3.5) +
     annotate(geom="text",x=c(4.8,5.2),y=rep(5.9,2), 
              label=c("a","a"), size=3.5) +
     annotate(geom="text",x=c(3.8,4.2),y=rep(5.9,2),
              label=c("a","a"), size=3.5) +
     annotate(geom="text",x=c(2.8,3.2),y=rep(6.7,2), 
              label=c("a","a"), size=3.5) +
     annotate(geom="text",x=c(1.8,2.2),y=rep(5.6,2), 
              label=c("a","a"), size=3.5) +
     annotate(geom="text",x=c(0.8, 1.2),y=rep(4.0,2),
              label=c("a","a"), size=3.5) 
dev.off()

##### By Age #####

# General Impacts of CC
pdf(file="./Figures/Exploratory/Likert/01CC_Impacts_04Age.pdf", width=11.5, height=6)
describe_stats(survey,"Age",19:24) +
     #last_plot() +
     annotate(geom="text",x=c(5.7,5.9,6.1,6.3),y=rep(7,4), 
              label=c("a","ab","bc","c"), size=3.5) +
     annotate(geom="text",x=c(4.7,4.9,5.1,5.3),y=rep(4.5,4),
              label=c("a","a","a","a"), size=3.5) +
     annotate(geom="text",x=c(3.7,3.9,4.1,4.3),y=rep(4.7,4), 
              label=c("ab","b","ab","a"), size=3.5) +
     annotate(geom="text",x=c(2.7,2.9, 3.1, 3.3),y=rep(6.5,4), 
              label=c("a","a","a","a"), size=3.5) +
     annotate(geom="text",x=c(1.7,1.9,2.1,2.3),y=rep(4.5,4), 
              label=c("b","ab","ab","a"), size=3.5) +
     annotate(geom="text",x=c(0.7,0.9,1.1,1.3),y=rep(4.5,4),
              label=c("b","a","ab","ab"), size=3.5) 
dev.off()

# Impacts of CC on Forests
pdf(file="./Figures/Exploratory/Likert/02CC_Forests_04Age.pdf", width=11.5, height=6)
describe_stats(survey,"Age",25:31) +
     #last_plot() +
     annotate(geom="text",x=c(6.7,6.9,7.1,7.3),y=rep(6.8,4),
              label=c("a","b","b","b"), size=3.5) +
     annotate(geom="text",x=c(5.7,5.9,6.1,6.3),y=rep(7,4), 
              label=c("a","ab","ab","b"), size=3.5) +
     annotate(geom="text",x=c(4.7,4.9,5.1,5.3),y=rep(7,4),
              label=c("a","b","b","b"), size=3.5) +
     annotate(geom="text",x=c(3.7,3.9,4.1,4.3),y=rep(5.3,4), 
              label=c("a","a","a","a"), size=3.5) +
     annotate(geom="text",x=c(2.7,2.9, 3.1, 3.3),y=rep(6.1,4), 
              label=c("a","ab","ab","b"), size=3.5) +
     annotate(geom="text",x=c(1.7,1.9,2.1,2.3),y=rep(4.7,4), 
              label=c("a","b","ab","ab"), size=3.5) +
     annotate(geom="text",x=c(0.7,0.9,1.1,1.3),y=rep(5,4),
              label=c("a","ab","ab","b"), size=3.5) 
dev.off()

# Current Practices
pdf(file="./Figures/Exploratory/Likert/03Current_Practices_04Age.pdf", width=11.5, height=6)
describe_stats(survey,"Age",32:36) +
     #last_plot() +
     annotate(geom="text",x=c(4.7,4.9,5.1,5.3),y=rep(4.7,4), 
              label=c("a","a","a","a"), size=3.5) +
     annotate(geom="text",x=c(3.7,3.9, 4.1, 4.3),y=rep(4.3,4), 
              label=c("a","ab","b","b"), size=3.5) +
     annotate(geom="text",x=c(2.7,2.9,3.1,3.3),y=rep(4.2,4), 
              label=c("a","a","a","a"), size=3.5) +
     annotate(geom="text",x=c(1.7,1.9,2.1,2.3),y=rep(7,4),
              label=c("a","ab","b","b"), size=3.5) +
     annotate(geom="text",x=c(0.7,0.9,1.1,1.3),y=rep(4.5,4),
              label=c("a","a","a","a"), size=3.5)
dev.off()

# Ecosystem
pdf(file="./Figures/Exploratory/Likert/04EcosystemManag_04Age.pdf", width=11.5, height=6)
describe_stats(survey,"Age",37:42) +
     #last_plot() +
     annotate(geom="text",x=c(5.7,5.9,6.1,6.3),y=rep(7,4),
              label=c("a","ab","b","b"), size=3.5) +
     annotate(geom="text",x=c(4.7,4.9,5.1,5.3),y=rep(6.1,4), 
              label=c("a","b","b","b"), size=3.5) +
     annotate(geom="text",x=c(3.7,3.9,4.1,4.3),y=rep(6.3,4),
              label=c("a","a","a","a"), size=3.5) +
     annotate(geom="text",x=c(2.7,2.9,3.1,3.3),y=rep(6.8,4), 
              label=c("a","a","a","a"), size=3.5) +
     annotate(geom="text",x=c(1.7,1.9,2.1,2.3),y=rep(5.7,4), 
              label=c("a","a","b","ab"), size=3.5) +
     annotate(geom="text",x=c(0.7,0.9,1.1,1.3),y=rep(4,4),
              label=c("a","a","a","a"), size=3.5) 
dev.off()

##### By Education #####

# General Impacts of CC
pdf(file="./Figures/Exploratory/Likert/01CC_Impacts_05_Education.pdf", width=11.5, height=6)
describe_stats(survey,"Education",19:24) +
     #last_plot() +
     annotate(geom="text",x=c(5.7,5.9,6.1,6.3),y=rep(7,4), 
              label=c("c","b","a","a"), size=3.5) +
     annotate(geom="text",x=c(4.7,4.9,5.1,5.3),y=rep(5,4),
              label=c("a","b","b","b"), size=3.5) +
     annotate(geom="text",x=c(3.7,3.9,4.1,4.3),y=rep(6,4), 
              label=c("a","b","c","c"), size=3.5) +
     annotate(geom="text",x=c(2.7,2.9, 3.1, 3.3),y=rep(6.5,4), 
              label=c("b","ab","a","ab"), size=3.5) +
     annotate(geom="text",x=c(1.7,1.9,2.1,2.3),y=rep(5,4), 
              label=c("a","a","b","b"), size=3.5) +
     annotate(geom="text",x=c(0.7,0.9,1.1,1.3),y=rep(5,4),
              label=c("a","ab","b","b"), size=3.5) 
dev.off()

# Impacts of CC on Forests
pdf(file="./Figures/Exploratory/Likert/02CC_Forests_05_Education.pdf", width=11.5, height=6)
describe_stats(survey,"Education",25:31) +
     #last_plot() +
     annotate(geom="text",x=c(6.7,6.9,7.1,7.3),y=rep(7,4),
              label=c("b","b","a","a"), size=3.5) +
     annotate(geom="text",x=c(5.7,5.9,6.1,6.3),y=rep(7,4), 
              label=c("b","b","a","a"), size=3.5) +
     annotate(geom="text",x=c(4.7,4.9,5.1,5.3),y=rep(7,4),
              label=c("c","b","a","a"), size=3.5) +
     annotate(geom="text",x=c(3.7,3.9,4.1,4.3),y=rep(5.8,4), 
              label=c("ab","b","a","ab"), size=3.5) +
     annotate(geom="text",x=c(2.7,2.9, 3.1, 3.3),y=rep(6.1,4), 
              label=c("a","a","a","a"), size=3.5) +
     annotate(geom="text",x=c(1.7,1.9,2.1,2.3),y=rep(5,4), 
              label=c("a","a","a","aa"), size=3.5) +
     annotate(geom="text",x=c(0.7,0.9,1.1,1.3),y=rep(5,4),
              label=c("b","b","a","ab"), size=3.5) 
dev.off()

# Current Practices
pdf(file="./Figures/Exploratory/Likert/03Current_Practices_05_Education.pdf", width=11.5, height=6)
describe_stats(survey,"Education",32:36) +
     #last_plot() +
     annotate(geom="text",x=c(4.7,4.9,5.1,5.3),y=rep(5,4), 
              label=c("a","a","a","a"), size=3.5) +
     annotate(geom="text",x=c(3.7,3.9, 4.1, 4.3),y=rep(4.6,4), 
              label=c("a","a","a","a"), size=3.5) +
     annotate(geom="text",x=c(2.7,2.9,3.1,3.3),y=rep(4.6,4), 
              label=c("a","a","b","b"), size=3.5) +
     annotate(geom="text",x=c(1.7,1.9,2.1,2.3),y=rep(7,4),
              label=c("ab","b","ab","a"), size=3.5) +
     annotate(geom="text",x=c(0.7,0.9,1.1,1.3),y=rep(5,4),
              label=c("a","b","bc","c"), size=3.5) 
dev.off()

# Ecosystem
pdf(file="./Figures/Exploratory/Likert/04EcosystemManag_05_Education.pdf", width=11.5, height=6)
describe_stats(survey,"Education",37:42) +
     #last_plot() +
     annotate(geom="text",x=c(5.7,5.9,6.1,6.3),y=rep(7,4),
              label=c("b","a","a","a"), size=3.5) +
     annotate(geom="text",x=c(4.7,4.9,5.1,5.3),y=rep(6.1,4), 
              label=c("a","a","a","a"), size=3.5) +
     annotate(geom="text",x=c(3.7,3.9,4.1,4.3),y=rep(6.3,4),
              label=c("a","a","a","a"), size=3.5) +
     annotate(geom="text",x=c(2.7,2.9,3.1,3.3),y=rep(6.8,4), 
              label=c("b","ab","ab","a"), size=3.5) +
     annotate(geom="text",x=c(1.7,1.9,2.1,2.3),y=rep(5.7,4), 
              label=c("a","a","a","a"), size=3.5) +
     annotate(geom="text",x=c(0.7,0.9,1.1,1.3),y=rep(4.5,4),
              label=c("a","ab","ab","b"), size=3.5) 
dev.off()

##### By Forest_Type #####

# General Impacts of CC
pdf(file="./Figures/Exploratory/Likert/01CC_Impacts_06Forest_Type.pdf", width=11.5, height=6)
describe_stats(survey,"Forest_Type",19:24) +
     #last_plot() +
     annotate(geom="text",x=c(5.75,6,6.25),y=rep(7,3), 
              label=c("a","a","a"), size=3.5) +
     annotate(geom="text",x=c(4.75,5,5.25),y=rep(4.5,3),
              label=c("a","a","a"), size=3.5) +
     annotate(geom="text",x=c(3.75,4,4.25),y=rep(4.5,3), 
              label=c("a","a","a"), size=3.5) +
     annotate(geom="text",x=c(2.75,3,3.25),y=rep(6.7,3), 
              label=c("a","a","a"), size=3.5) +
     annotate(geom="text",x=c(1.75,2,2.25),y=rep(4.3,3), 
              label=c("a","a","a"), size=3.5) +
     annotate(geom="text",x=c(0.75,1,1.25),y=rep(4.5,3),
              label=c("a","ab","b"), size=3.5) 
dev.off()

# Impacts of CC on Forests
pdf(file="./Figures/Exploratory/Likert/02CC_Forests_06Forest_Type.pdf", width=11.5, height=6)
describe_stats(survey,"Forest_Type",25:31) +
     #last_plot() +
     annotate(geom="text",x=c(6.75,7,7.25),y=rep(7,3), 
              label=c("a","a","a"), size=3.5) +
     annotate(geom="text",x=c(5.75,6,6.25),y=rep(7,3), 
              label=c("b","ab","a"), size=3.5) +
     annotate(geom="text",x=c(4.75,5,5.25),y=rep(7,3),
              label=c("b","ab","a"), size=3.5) +
     annotate(geom="text",x=c(3.75,4,4.25),y=rep(5.5,3), 
              label=c("a","a","a"), size=3.5) +
     annotate(geom="text",x=c(2.75,3,3.25),y=rep(5.7,3), 
              label=c("a","a","a"), size=3.5) +
     annotate(geom="text",x=c(1.75,2,2.25),y=rep(4.6,3), 
              label=c("a","a","a"), size=3.5) +
     annotate(geom="text",x=c(0.75,1,1.25),y=rep(5.3,3),
              label=c("b","a","ab"), size=3.5) 
dev.off()

# Current Practices
pdf(file="./Figures/Exploratory/Likert/03Current_Practices_06Forest_Type.pdf", width=11.5, height=6)
describe_stats(survey,"Forest_Type",32:36) +
     #last_plot() +
     annotate(geom="text",x=c(4.75,5,5.25),y=rep(4.5,3), 
              label=c("a","a","a"), size=3.5) +
     annotate(geom="text",x=c(3.75,4,4.25),y=rep(4.1,3), 
              label=c("a","a","a"), size=3.5) +
     annotate(geom="text",x=c(2.75,3,3.25),y=rep(4,3), 
              label=c("a","a","a"), size=3.5) +
     annotate(geom="text",x=c(1.75,2,2.25),y=rep(7,3),
              label=c("a","a","a"), size=3.5) +
     annotate(geom="text",x=c(0.75,1,1.25),y=rep(4.0,3),
              label=c("ab","a","b"), size=3.5)
dev.off()

# Ecosystem
pdf(file="./Figures/Exploratory/Likert/04EcosystemManag_06Forest_Type.pdf", width=11.5, height=6)
describe_stats(survey,"Forest_Type",37:42) +
     #last_plot() +
     annotate(geom="text",x=c(5.75,6,6.25),y=rep(7,3), 
              label=c("b","ab","a"), size=3.5) +
     annotate(geom="text",x=c(4.75,5,5.25),y=rep(6,3), 
              label=c("a","ab","b"), size=3.5) +
     annotate(geom="text",x=c(3.75,4,4.25),y=rep(6,3),
              label=c("a","a","a"), size=3.5) +
     annotate(geom="text",x=c(2.75,3,3.25),y=rep(6.8,3), 
              label=c("a","a","a"), size=3.5) +
     annotate(geom="text",x=c(1.75,2,2.25),y=rep(5.7,3), 
              label=c("a","a","a"), size=3.5) +
     annotate(geom="text",x=c(0.75,1,1.25),y=rep(4,3),
              label=c("a","a","a"), size=3.5)
dev.off()

##### By Politics #####

# General Impacts of CC
pdf(file="./Figures/Exploratory/Likert/01CC_Impacts_07Politics.pdf", width=11.5, height=6)
describe_stats(survey,"Politics",19:24) +
     #last_plot() +
     annotate(geom="text",x=c(5.7,5.82,5.94,6.06,6.18,6.3),y=rep(7,6), 
              label=c("a","a","a","b","c","c"), size=3) +
     annotate(geom="text",x=c(4.7,4.82,4.94,5.06,5.18,5.3),y=rep(6,6), 
              label=c("c","bc","b","a","a","a"), size=3) +
     annotate(geom="text",x=c(3.7,3.82,3.94,4.06,4.18,4.3),y=rep(6.5,6), 
              label=c("d","cd","c","b","a","a"), size=3) +
     annotate(geom="text",x=c(2.7,2.82,2.94,3.06,3.18,3.3),y=rep(6.8,6), 
              label=c("ab","a","ab","bc","c","c"), size=3) +
     annotate(geom="text",x=c(1.7,1.82,1.94,2.06,2.18,2.3),y=rep(5,6), 
              label=c("b","b","ab","ab","a","ab"), size=3) +
     annotate(geom="text",x=c(0.7,0.82,0.94,1.06,1.18,1.3),y=rep(5.1,6), 
              label=c("b","b","b","a","a","a"), size=3) 
dev.off()

# Impacts of CC on Forests
pdf(file="./Figures/Exploratory/Likert/02CC_Forests_07Politics.pdf", width=11.5, height=6)
describe_stats(survey,"Politics",25:31) +
     #last_plot() +
     annotate(geom="text",x=c(6.7,6.82,6.94,7.06,7.18,7.3),y=rep(7,6), 
              label=c("a","a","ab","bc","cd","d"), size=3) +
     annotate(geom="text",x=c(5.7,5.82,5.94,6.06,6.18,6.3),y=rep(7,6), 
              label=c("a","a","a","b","bc","c"), size=3) +
     annotate(geom="text",x=c(4.7,4.82,4.94,5.06,5.18,5.3),y=rep(7,6), 
              label=c("a","a","a","b","c","c"), size=3) +
     annotate(geom="text",x=c(3.7,3.82,3.94,4.06,4.18,4.3),y=rep(5.5,6), 
              label=c("a","a","a","a","a","a"), size=3) +
     annotate(geom="text",x=c(2.7,2.82,2.94,3.06,3.18,3.3),y=rep(6.3,6), 
              label=c("a","ab","ab","ab","b","b"), size=3.5) +
     annotate(geom="text",x=c(1.7,1.82,1.94,2.06,2.18,2.3),y=rep(5,6), 
              label=c("a","a","a","a","a","a"), size=3) +
     annotate(geom="text",x=c(0.7,0.82,0.94,1.06,1.18,1.3),y=rep(6,6), 
              label=c("a","a","a","a","a","a"), size=3) 
dev.off()

# Current Practices
pdf(file="./Figures/Exploratory/Likert/03Current_Practices_07Politics.pdf", width=11.5, height=6)
describe_stats(survey,"Politics",32:36) +
     #last_plot() +
     annotate(geom="text",x=c(4.7,4.82,4.94,5.06,5.18,5.3),y=rep(5,6), 
              label=c("a","a","a","a","a","a"), size=3) +
     annotate(geom="text",x=c(3.7,3.82,3.94,4.06,4.18,4.3),y=rep(5.3,6), 
              label=c("a","a","a","a","a","a"), size=3) +
     annotate(geom="text",x=c(2.7,2.82,2.94,3.06,3.18,3.3),y=rep(5.5,6), 
              label=c("b","b","b","a","a","a"), size=3) +
     annotate(geom="text",x=c(1.7,1.82,1.94,2.06,2.18,2.3),y=rep(7,6), 
              label=c("a","a","a","b","bc","c"), size=3) +
     annotate(geom="text",x=c(0.7,0.82,0.94,1.06,1.18,1.3),y=rep(5.5,6), 
              label=c("c","c","bc","bc","b","a"), size=3) 
dev.off()

# Ecosystem
pdf(file="./Figures/Exploratory/Likert/04EcosystemManag_07Politics.pdf", width=11.5, height=6)
describe_stats(survey,"Politics",37:42) +
     #last_plot() +
     annotate(geom="text",x=c(5.7,5.82,5.94,6.06,6.18,6.3),y=rep(7,6), 
              label=c("a","a","ab","b","ab","b"), size=3) +
     annotate(geom="text",x=c(4.7,4.82,4.94,5.06,5.18,5.3),y=rep(7,6), 
              label=c("a","a","a","a","a","a"), size=3) +
     annotate(geom="text",x=c(3.7,3.82,3.94,4.06,4.18,4.3),y=rep(6.2,6), 
              label=c("a","a","a","a","a","a"), size=3) +
     annotate(geom="text",x=c(2.7,2.82,2.94,3.06,3.18,3.3),y=rep(7,6), 
              label=c("a","a","a","a","a","a"), size=3) +
     annotate(geom="text",x=c(1.7,1.82,1.94,2.06,2.18,2.3),y=rep(5.7,6), 
              label=c("a","a","a","a","a","a"), size=3) +
     annotate(geom="text",x=c(0.7,0.82,0.94,1.06,1.18,1.3),y=rep(4.5,6), 
              label=c("b","b","b","ab","a","ab"), size=3) 
dev.off()


