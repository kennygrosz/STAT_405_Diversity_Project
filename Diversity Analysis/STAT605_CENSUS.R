require(tidyverse)
require(stringr)
require(forcats)
require(ggplot2)
require(choroplethr)
require(choroplethrMaps)
require(dplyr)
library(tidyverse)

dat<-read.csv("CC-EST2015-ALLDATA.csv",header = TRUE)
head(dat)

##FOR ALL AGE GROUPS AND FOR YEAR 2015
dat2015<-dat %>% 
  filter(AGEGRP==0 & YEAR==8)

##DO NOT COUNT HISPANIC
##SIMPLY USING TWO 6 TYPES OF RACES AND THIS COULD BE REVISED 
##H=H_MALE+H_FEMALE,
dat2015<-dat2015 %>% 
  transmute(STATE=STATE,
            COUNTY=COUNTY,
            TOT=TOT_POP,
            WA=WA_MALE+WA_FEMALE,
            BA=BA_MALE+BA_FEMALE,
            IA=IA_MALE+IA_FEMALE,
            AA=AA_MALE+AA_FEMALE,
            NHA=NA_MALE+NA_FEMALE,
            MORE=TOT-WA-BA-IA-AA-NHA,
            WAP=WA/TOT,
            BAP=BA/TOT,
            IAP=IA/TOT,
            AAP=AA/TOT,
            NHAP=NHA/TOT,
            MOREP=MORE/TOT,
            INDEX=1-WAP^2-BAP^2-IAP^2-AAP^2-NHAP^2-MOREP^2)

dat2015<-dat2015 %>% 
  mutate(STATE_char = as.character(STATE), COUNTY_char = as.character(COUNTY))
dat2015$STATE_char = str_pad(dat2015$STATE_char, 2, pad = "0")
dat2015$COUNTY_char = str_pad(dat2015$COUNTY_char, 3, pad = "0")
head(dat2015)
dat2015<-dat2015 %>% 
  mutate(FIPS = paste(STATE_char, COUNTY_char, sep = ""))
head(dat2015)

mapping <- select(dat2015, FIPS, INDEX) 
mapping <- dplyr::rename(mapping, region=FIPS, value=INDEX) 
mapping$region <- as.numeric(mapping$region)

county_choropleth(mapping, title = "Diversity Index in Counties Across the US")

read.csv("Education.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  tbl_df() ->
  edu

dat2015$FIPS<-as.numeric(dat2015$FIPS)
edu_comb<-left_join(dat2015, edu, by=c("FIPS"="FIPS.Code"))

edu_comb %>% 
  ggplot(aes(x=INDEX,
             y=Percent.of.adults.with.less.than.a.high.school.diploma..2010.2014))+
  geom_point()+geom_smooth()+
  ylab("Percentage")+
  xlab("Diversity Index")+
  ggtitle("Diversity vs No High School Diploma Percentage")

edu_comb %>% 
  ggplot(aes(x=INDEX,
             y=Percent.of.adults.with.a.high.school.diploma.only..2010.2014))+
  geom_point()+geom_smooth()+
  ylab("Percentage")+
  xlab("Diversity Index")+
  ggtitle("Diversity vs High School Diploma Only Percentage")


edu_comb %>% 
  ggplot(aes(x=INDEX,
             y=Percent.of.adults.completing.some.college.or.associate.s.degree..2010.2014))+
  geom_point()+geom_smooth()+
  ylab("Percentage")+
  xlab("Diversity Index")+
  ggtitle("Diversity vs Completing College or Associate Percentage")

edu_comb %>% 
  ggplot(aes(x=INDEX,
             y=Percent.of.adults.with.a.bachelor.s.degree.or.higher..2010.2014))+
  geom_point()+geom_smooth()+
  ylab("Percentage")+
  xlab("Diversity Index")+
  ggtitle("Diversity vs Bachelor Degree or Higher Percentage")

##Linear Regression Analysis
l1<-lm(Percent.of.adults.with.less.than.a.high.school.diploma..2010.2014~INDEX, data = edu_comb)
##summary(l1)
l2<-lm(Percent.of.adults.with.a.high.school.diploma.only..2010.2014~INDEX, data = edu_comb)
##summary(l2)
l3<-lm(Percent.of.adults.completing.some.college.or.associate.s.degree..2010.2014~INDEX, data = edu_comb)
##summary(l3)
l4<-lm(Percent.of.adults.with.a.bachelor.s.degree.or.higher..2010.2014~INDEX, data = edu_comb)
##summary(l4)

dat2015 %>% 
  ggplot(aes(x=INDEX))+geom_histogram()
