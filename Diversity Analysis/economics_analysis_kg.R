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
#setwd("C:/Users/kenny/OneDrive/Documents/Fall 2016/STAT 405- R for Data Science/STAT_405_Diversity_Project/Diversity Analysis")
#alternatively
setwd("C:/Users/kenny/OneDrive/Documents/Fall 2016/STAT 405- R for Data Science/STAT_405_Diversity_Project/IPUMS Data/Economics Data")

#Read in diversity database
#read.csv("ipums_educ.csv", header = TRUE, stringsAsFactors = FALSE) %>%
read.csv("economics_ipums_extract.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  tbl_df() ->
  A

#Read in County FIPS Key
read.csv("../../Diversity Analysis/fips_codes_website.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  tbl_df() ->
  B

#convert to tibble so dplyr can work its magic
A_df = tbl_df(A)

#format FIPs in source file!
head(A_df)
A_df_char = mutate(A_df, SFIP_char = as.character(STATEFIP), CFIP_char = as.character(COUNTYFIPS))
A_df_char$SFIP_char = str_pad(A_df_char$SFIP_char, 2, pad = "0")
A_df_char$CFIP_char = str_pad(A_df_char$CFIP_char, 3, pad = "0")
head(A_df_char)
A_df_char = mutate(A_df_char, FIPS = paste(SFIP_char, CFIP_char, sep = ""))

head(B)
B2 = filter (B, Entity.Description == "County")
head(B2)
B2$State.FIPS.Code = as.character(B2$State.FIPS.Code)
B2$County.FIPS.Code = as.character(B2$County.FIPS.Code)
B2$State.FIPS.Code = str_pad(B2$State.FIPS.Code, 2, pad = "0")
B2$County.FIPS.Code = str_pad(B2$County.FIPS.Code, 3, pad = "0")
B2 = mutate(B2,FIPS = paste(State.FIPS.Code,County.FIPS.Code, sep = ""))

#HAVE TO ADD COLUMNS FOR EACH RACE, WHICH WILL MAKE SUMMARIZING EASIER
A2=A_df_char %>% mutate(W = ifelse(RACE==1,1,0))
A2 = A2 %>% mutate(B = ifelse(RACE==2,1,0))
A2 = A2 %>% mutate(AI = ifelse(RACE==3,1,0))
A2 = A2 %>% mutate(C = ifelse(RACE==4,1,0))
A2 = A2 %>% mutate(J = ifelse(RACE==5,1,0))
A2 = A2 %>% mutate(PI = ifelse(RACE==6,1,0))
A2 = A2 %>% mutate(O = ifelse(RACE==7 | RACE == 8 | RACE == 9 ,1,0))


#summarise race rsults and create diversity index
by_county = A2 %>% group_by(FIPS) %>% dplyr::summarise( tot = n(), W = sum(W)/tot, B = sum(B)/tot, AI = sum (AI)/tot, C = sum(C)/tot, J = sum(J)/tot, PI = sum(PI)/tot, O = sum(O)/tot)
by_county = mutate(by_county,TOT = W+B+AI+C+J+PI+O)
by_county = by_county %>% mutate(DiversityIndex = 1 - W^2-B^2-AI^2-C^2 -J^2 - PI^2 - O^2) #calculate diversity indec
by_county = arrange(by_county, desc(DiversityIndex)) #arrange in descenting order
mapping = select(by_county, FIPS, DiversityIndex) 


A_df_char %>% dplyr::group_by(FIPS) %>% 
  dplyr::summarise(q1 = quantile(POVERTY, probs = .25),
                   q3= quantile(POVERTY, probs = .75),
                   IQR = q3-q1) -> C
C
mapping

D = dplyr::inner_join(C, mapping, by = c("FIPS"))

fit = lm(formula =D$IQR ~ D$DiversityIndex)
summary(fit)

ggplot(D, aes(x=DiversityIndex, y=IQR)) + geom_point() + #Create point graph 
labs(title="Income Gap Varies with Diversity", x="Diversity Index", y="IQR") +
geom_smooth(method=lm, se=TRUE, formula = y ~ x) + 
  annotate("text", x = 4, y = 25, label = "y = 290.994 + 56.665x")

  
linegraph
