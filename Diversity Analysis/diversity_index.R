#Diversity Index Analayis

#Import Libraries
require(tidyverse)
require(stringr)
require(forcats)
require(ggplot2)
require(choroplethr)
require(choroplethrMaps)
require(dplyr)

# SET WORKING DIRECTORY
setwd("C:/Users/kenny/OneDrive/Documents/Fall 2016/STAT 405- R for Data Science/STAT_405_Diversity_Project/IPUMS Data/Education Data")

#Read in File
read.csv("ipums_educ.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  tbl_df() ->
  A

A_df = tbl_df(A)

#RACE CODES
# 1 = white
# 2 = black
# 3 = American Indian/Alaskan Native
# 4 = Chinese
# 5 = Japanese
# 6 = Other Asian / Pacific Islander
# 7 = Othe Race, nec
# 8 = Two major races
# 9 = Three or more races

head(A_df)

#HAVE TO ADD COLUMNS FOR EACH RACE, WHICH WILL MAKE SUMMARIZING EASIER
A2=A_df %>% mutate(W = ifelse(RACE==1,1,0))
A2 = A2 %>% mutate(B = ifelse(RACE==2,1,0))
A2 = A2 %>% mutate(AI = ifelse(RACE==3,1,0))
A2 = A2 %>% mutate(C = ifelse(RACE==4,1,0))
A2 = A2 %>% mutate(J = ifelse(RACE==5,1,0))
A2 = A2 %>% mutate(PI = ifelse(RACE==6,1,0))
A2 = A2 %>% mutate(O = ifelse(RACE==7 | RACE == 8 | RACE == 9 ,1,0))

A2
head(A2)
A2


by_county = A2 %>% group_by(COUNTYFIPS) %>% dplyr::summarise( tot = n(), W = sum(W)/tot, B = sum(B)/tot, AI = sum (AI)/tot, C = sum(C)/tot, J = sum(J)/tot, PI = sum(PI)/tot, O = sum(O)/tot)

by_county = mutate(by_county,TOT = W+B+AI+C+J+PI+O)

by_county = by_county %>% mutate(DiversityIndex = 1 - W^2-B^2-AI^2-C^2 -J^2 - PI^2 - O^2)

by_county = arrange(by_county, desc(DiversityIndex))

by_county

mapping = select(by_county, COUNTYFIPS, DiversityIndex)

mapping = dplyr::rename(mapping, region=COUNTYFIPS, value=DiversityIndex)


county_choropleth(mapping)


data(df_pop_county)
head(df_pop_county)

county_choropleth(df_pop_county)
