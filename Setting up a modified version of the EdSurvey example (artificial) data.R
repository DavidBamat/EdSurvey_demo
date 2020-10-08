#####################################################################################
### Setting up a modified version of the EdSurvey example (artificial) NAEP data ####
#####################################################################################

###Load packages###

#By loading "tidyverse," a bunch of popular R packages are loaded-- including dplyr and ggplot2
if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")

#for the example data
if(!require(EdSurvey)) install.packages("EdSurvey", 
                                        repos = "http://cran.us.r-project.org")


###Unpack artificial data set from EdSurvey package###

#import data and save to object named "df"
df <- readNAEP(system.file("extdata/data", "M36NT2PM.dat", package = "NAEPprimer"))

#reformat data into a data frame
df <- getData(df, c('composite', 'scrpsu', 'pared', 'sdracem', 'lep'), 
              omittedLevels = FALSE, addAttributes = TRUE)

#perform some data wrangling to ready data for analysis
df <- df %>%
  mutate(college = if_else(pared == "Graduated college", 1, 0),
         white = if_else(sdracem == "White", 1, 0),
         el = if_else(lep == "Yes", 1, 0)) %>%
  select("scrpsu", "mrpcm1", "college", "white", "el")

names(df) <- c("school", "score", "college", "white", "el")

###create a contextual/school-level variable from the "college" variable representing the proportion of ###
###  students in schools whose parents graduated from college ###

tabulation <- df %>%
  group_by(school) %>%
  count(college) %>%
  mutate(total = sum(n)) %>%
  filter(college==1) %>%
  mutate(prop_college = n/total) %>%
  select("school", "prop_college")
  
### merge with prop_college variable into data set ###

df <- full_join(df, tabulation, by = "school") 

### remove cases with missing values (about 3% of cases overall) ###

df <- na.omit(df)

###Export data as .csv###
write.csv(df,  "~/R/EdSurvey_demo/artificial-NAEP-data.csv")


