load("./data/SurveyData_Clean_Weighted.Rdata")
source("./code/survey_functions.R")


# General Impacts (5-10)
survey %>%
    summarize_at(vars(Human_Cause:Ample_time), 
                 funs(mean(., na.rm = TRUE), sd(., na.rm=TRUE)))


t.test2(5.54, 5.815005, 1.69, 1.397639, 53, 974)
t.test2(2.81, 2.576132, 1.40, 1.505009, 53, 974)
t.test2(3.47, 2.625646, 1.73, 1.537147, 53, 974)
t.test2(3.92, 4.632780, 1.59, 1.632994, 53, 974)
t.test2(3.22, 2.503106, 1.62, 1.351066, 53, 974)
t.test2(2.85, 2.612203, 1.45, 1.391923, 53, 974)

# Forest Impacts (11-17)
survey %>%
    summarize_at(vars(Current_Impact:Managers_Control), 
                 funs(mean(., na.rm = TRUE), sd(., na.rm=TRUE)))

t.test2(5.32, 5.317575, 1.37, 1.364176, 53, 974)
t.test2(5.96, 5.951596, 1.19, 1.142761, 53, 974)
t.test2(2.94, 3.721649, 1.24, 1.42325, 53, 974)
t.test2(3.41, 3.830228, 1.50, 1.725805, 53, 974)
t.test2(2.40, 2.961856, 1.02, 1.286425, 53, 974)


# Current Practices (18-22)
survey %>%
    summarize_at(vars(Legislation:Wait), 
                 funs(mean(., na.rm = TRUE), sd(., na.rm=TRUE)))

# Ecosystem Management (23-28)
survey %>%
    summarize_at(vars(Fully_Functioning:Retention), 
                 funs(mean(., na.rm = TRUE), sd(., na.rm=TRUE)))





t.test2(5.54, 5.81,1.69, 1.40, 53, 974)
t.test2(5.54, 5.81,1.69, 1.40, 53, 974)
t.test2(5.54, 5.81,1.69, 1.40, 53, 974)
t.test2(5.54, 5.81,1.69, 1.40, 53, 974)
t.test2(5.54, 5.81,1.69, 1.40, 53, 974)
