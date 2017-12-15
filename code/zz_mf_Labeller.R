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

