######### Trends in prevalence of ACEs among US children and adolescents #####
##### NSCH 2016-2020 data #####

library(tidyverse); library(haven)
#Import NSCH 2016 to 2020 data, subset needed variables and combine
nsch16 <- read_csv("drc-2016-nsch-csv-v3/2016NSCHTopical_DRCv3_Sep2018.csv")
nsch16a <- nsch16 %>% select(YEAR, HHID, FIPSST, STRATUM, FWC, SC_AGE_YEARS, age3_16, SC_SEX, raceASIA_16,
                            ACEincome2_16:ACE4more_16)
names(nsch16a) <- gsub("_16", "", names(nsch16a), fixed = TRUE)

nsch17 <- read_csv("DRC_2017_NSCH_Topical_CSV/DRC_2017NCSH_Topical.csv")
nsch17a <- nsch17 %>% select(YEAR, HHID, FIPSST, STRATUM, FWC, SC_AGE_YEARS, age3_17, SC_SEX, raceASIA_17,
                             ACEincome2_17:ACE2more_17) %>%
  mutate(ACE4more = ifelse(ACEct_17==0,0, ifelse(ACEct_17==1, 1, ifelse(ACEct_17==2|ACEct_17==3, 2, ifelse(ACEct_17==99, 99, 3)))),
         ACE1more = ifelse(ACEct_17==0, 1, ifelse(ACEct_17==99, 99, 2)))
names(nsch17a) <- gsub("_17", "", names(nsch17a), fixed = TRUE)

nsch18 <- read_csv("2018NSCH_Topical_CSV_DRC_May2020/2018 NSCH_Topical_DRC_5.7.20.csv")
nsch18a <- nsch18 %>% select(YEAR, HHID, FIPSST, STRATUM, FWC, SC_AGE_YEARS, age3_18, SC_SEX, raceASIA_18,
                             ACEincome2_18:ACE2more_18) %>%
  mutate(ACE4more = ifelse(ACEct_18==0,0, ifelse(ACEct_18==1, 1, ifelse(ACEct_18==2|ACEct_18==3, 2, ifelse(ACEct_18==99, 99, 3)))),
         ACE1more = ifelse(ACEct_18==0, 1, ifelse(ACEct_18==99, 99, 2)))
names(nsch18a) <- gsub("_18", "", names(nsch17a), fixed = TRUE)

nsch19 <- read_csv("2019 NSCH_CSV_CAHMI_DRC/2019 NSCHTopical_CAHMI DRC.csv")
nsch19a <- nsch19 %>% select(YEAR, HHID, FIPSST, STRATUM, FWC, SC_AGE_YEARS, age3_19, SC_SEX, raceASIA_19,
                             ACEincome2_19:ACE2more_19) %>%
  mutate(ACE4more = ifelse(ACEct_19==0,0, ifelse(ACEct_19==1, 1, ifelse(ACEct_19==2|ACEct_19==3, 2, ifelse(ACEct_19==99, 99, 3)))),
         ACE1more = ifelse(ACEct_19==0, 1, ifelse(ACEct_19==99, 99, 2)))
names(nsch19a) <- gsub("_19", "", names(nsch17a), fixed = TRUE)

nsch20 <- read_stata("nsch_2020_topical_Stata/nsch_2020_topical.dta")
nsch20a <- nsch20 %>% select(year, hhid, fipsst, stratum, fwc, sc_age_years, sc_sex, sc_race_r, sc_hispanic_r,
                             ace3:ace10, ace1) %>%
  mutate(age3 = ifelse(sc_age_years<=5, 1, ifelse(sc_age_years>5 & sc_age_years<=11, 2, 3)),
         raceASIA=ifelse(sc_hispanic_r==1,1,ifelse(sc_hispanic_r==2 & sc_race_r==1,2,
                                                      ifelse(sc_hispanic_r==2 & sc_race_r==2,3,
                                                             ifelse(sc_hispanic_r==2 & sc_race_r==4,4,5)))),
         ACEincome2=ifelse(ace1==3|ace1==4, 1, ifelse(ace1==1|ace1==2, 2, ace1))) %>%
  rename(ACEdivorce=ace3, ACEdeath=ace4, ACEjail=ace5, ACEdomviol=ace6, ACEneighviol=ace7,
         ACEmhealth=ace8, ACEdrug=ace9, ACEdiscrim=ace10)
nsch20a$ACEct=apply(X=nsch20a[,c(10:17,21)],1,FUN=function(x) length(which(x==1)))
nsch20a <- nsch20a %>%
  mutate(ACE4more = ifelse(ACEct==0,0, ifelse(ACEct==1, 1, ifelse(ACEct==2|ACEct==3, 2, ifelse(ACEct>=4, 3, ACEct)))),
         ACE1more = ifelse(ACEct==0, 1, ifelse(ACEct>=1, 2, ACEct)),
         ACE2more = ifelse(ACEct==0, 1, ifelse(ACEct==1, 2, ifelse(ACEct>=2, 3, ACEct))),
         stratum= as.numeric(ifelse(stratum=='1', 1, ifelse(stratum=='2A', 2, stratum)))) %>%
  rename(YEAR=year, HHID=hhid, FIPSST=fipsst, STRATUM=stratum, FWC=fwc, SC_AGE_YEARS=sc_age_years, SC_SEX=sc_sex) %>%
  select(-sc_race_r, -sc_hispanic_r, -ace1)

nsch16_20 <- bind_rows(nsch16a, nsch17a, nsch18a, nsch19a, nsch20a)
nsch16_20$FWC16_20=nsch16_20$FWC/5
nsch16_20[nsch16_20==99] <- NA

options(digits = 3)

#National-Level Analysis
#2016
#Survey weights
library(survey); library(jtools)
nsch16a <- svydesign(id=~HHID, weights=~FWC,strata=~STRATUM+FIPSST, data=nsch16a)
summary(nsch16a)
#Descriptive statistics
svymean(~SC_AGE_YEARS, design=nsch16a, na=TRUE)
svysd(~SC_AGE_YEARS, design=nsch16a, na=TRUE)
prop.table(svytable(~age3, nsch16a))
prop.table(svytable(~SC_SEX, nsch16a))
prop.table(svytable(~raceASIA, nsch16a))
#Proportion of children and adolescents with 4 or more ACEs
#Overall
prop.table(svytable(~ACE4more, nsch16a))
attr(svyciprop(~I(ACE4more==3), nsch16a, method = "li"), "ci") #remove attr for estimate and 95% CI
#By age group
attr(svyciprop(~I(ACE4more==3), subset(nsch16a, age3==1), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16a, age3==2), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16a, age3==3), method = "li"), "ci")
#By sex
attr(svyciprop(~I(ACE4more==3), subset(nsch16a, SC_SEX==1), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16a, SC_SEX==2), method = "li"), "ci")
#By race/ethnicity
attr(svyciprop(~I(ACE4more==3), subset(nsch16a, raceASIA==1), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16a, raceASIA==2), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16a, raceASIA==3), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16a, raceASIA==4), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16a, raceASIA==5), method = "li"), "ci")

#2017
#Survey weights
library(survey); library(jtools)
nsch17a <- svydesign(id=~HHID, weights=~FWC,strata=~STRATUM+FIPSST, data=nsch17a)
summary(nsch17a)
#Descriptive statistics
svymean(~SC_AGE_YEARS, design=nsch17a, na=TRUE)
svysd(~SC_AGE_YEARS, design=nsch17a, na=TRUE)
prop.table(svytable(~age3, nsch17a))
prop.table(svytable(~SC_SEX, nsch17a))
prop.table(svytable(~raceASIA, nsch17a))
#Proportion of children and adolescents with 4 or more ACEs
#Overall
prop.table(svytable(~ACE4more, nsch17a))
attr(svyciprop(~I(ACE4more==3), nsch17a, method = "li"), "ci")
#By age group
attr(svyciprop(~I(ACE4more==3), subset(nsch17a, age3==1), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch17a, age3==2), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch17a, age3==3), method = "li"), "ci")
#By sex
attr(svyciprop(~I(ACE4more==3), subset(nsch17a, SC_SEX==1), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch17a, SC_SEX==2), method = "li"), "ci")
#By race/ethnicity
attr(svyciprop(~I(ACE4more==3), subset(nsch17a, raceASIA==1), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch17a, raceASIA==2), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch17a, raceASIA==3), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch17a, raceASIA==4), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch17a, raceASIA==5), method = "li"), "ci")

#2018
#Survey weights
library(survey); library(jtools)
nsch18a <- svydesign(id=~HHID, weights=~FWC,strata=~STRATUM+FIPSST, data=nsch18a)
summary(nsch18a)
#Descriptive statistics
svymean(~SC_AGE_YEARS, design=nsch18a, na=TRUE)
svysd(~SC_AGE_YEARS, design=nsch18a, na=TRUE)
prop.table(svytable(~age3, nsch18a))
prop.table(svytable(~SC_SEX, nsch18a))
prop.table(svytable(~raceASIA, nsch18a))
#Proportion of children and adolescents with 4 or more ACEs
#Overall
prop.table(svytable(~ACE4more, nsch18a))
attr(svyciprop(~I(ACE4more==3), nsch18a, method = "li"), "ci")
#By age group
attr(svyciprop(~I(ACE4more==3), subset(nsch18a, age3==1), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch18a, age3==2), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch18a, age3==3), method = "li"), "ci")
#By sex
attr(svyciprop(~I(ACE4more==3), subset(nsch18a, SC_SEX==1), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch18a, SC_SEX==2), method = "li"), "ci")
#By race/ethnicity
attr(svyciprop(~I(ACE4more==3), subset(nsch18a, raceASIA==1), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch18a, raceASIA==2), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch18a, raceASIA==3), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch18a, raceASIA==4), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch18a, raceASIA==5), method = "li"), "ci")

#2019
#Survey weights
library(survey); library(jtools)
nsch19a <- svydesign(id=~HHID, weights=~FWC,strata=~STRATUM+FIPSST, data=nsch19a)
summary(nsch19a)
#Descriptive statistics
svymean(~SC_AGE_YEARS, design=nsch19a, na=TRUE)
svysd(~SC_AGE_YEARS, design=nsch19a, na=TRUE)
prop.table(svytable(~age3, nsch19a))
prop.table(svytable(~SC_SEX, nsch19a))
prop.table(svytable(~raceASIA, nsch19a))
#Proportion of children and adolescents with 4 or more ACEs
#Overall
prop.table(svytable(~ACE4more, nsch19a))
attr(svyciprop(~I(ACE4more==3), nsch19a, method = "li"), "ci")
#By age group
attr(svyciprop(~I(ACE4more==3), subset(nsch19a, age3==1), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch19a, age3==2), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch19a, age3==3), method = "li"), "ci")
#By sex
attr(svyciprop(~I(ACE4more==3), subset(nsch19a, SC_SEX==1), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch19a, SC_SEX==2), method = "li"), "ci")
#By race/ethnicity
attr(svyciprop(~I(ACE4more==3), subset(nsch19a, raceASIA==1), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch19a, raceASIA==2), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch19a, raceASIA==3), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch19a, raceASIA==4), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch19a, raceASIA==5), method = "li"), "ci")

#2020
#Survey weights
library(survey); library(jtools)
nsch20a <- svydesign(id=~HHID, weights=~FWC,strata=~STRATUM+FIPSST, data=nsch20a)
summary(nsch20a)
#Descriptive statistics
svymean(~SC_AGE_YEARS, design=nsch20a, na=TRUE)
svysd(~SC_AGE_YEARS, design=nsch20a, na=TRUE)
prop.table(svytable(~age3, nsch20a))
prop.table(svytable(~SC_SEX, nsch20a))
prop.table(svytable(~raceASIA, nsch20a))
#Proportion of children and adolescents with 4 or more ACEs
#Overall
prop.table(svytable(~ACE4more, nsch20a))
attr(svyciprop(~I(ACE4more==3), nsch20a, method = "li"), "ci")
#By age group
attr(svyciprop(~I(ACE4more==3), subset(nsch20a, age3==1), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch20a, age3==2), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch20a, age3==3), method = "li"), "ci")
#By sex
attr(svyciprop(~I(ACE4more==3), subset(nsch20a, SC_SEX==1), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch20a, SC_SEX==2), method = "li"), "ci")
#By race/ethnicity
attr(svyciprop(~I(ACE4more==3), subset(nsch20a, raceASIA==1), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch20a, raceASIA==2), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch20a, raceASIA==3), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch20a, raceASIA==4), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch20a, raceASIA==5), method = "li"), "ci")

library(readxl)
aces4more_national = read_excel("data.xlsx", sheet = "ACES4more_national")
ggplot(aces4more_national, aes(x=Year, y=ACE4more_prop)) +
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), width=0.1) +
  geom_line() +
  geom_point() +
  xlab("Year of survey") +
  ylab("% with 4 or more ACEs") +
  theme_bw() +
  ggtitle("Proportion (95% CI) of children and adolescents aged less \nthan 18 years who reported at least 4 ACEs") +
  theme(plot.title = element_text(hjust = 0.5))

aces4more_nationalbyAge = read_excel("data.xlsx", sheet = "ACES4more_nationalbyAge")
aces4more_nationalbyAge$age_group <- factor(aces4more_nationalbyAge$age_group,
                                            levels = c("0-5 years", "6-11 years", "12-17 years"),
                                            labels = c("0-5 years", "6-11 years", "12-17 years"))
ggplot(aces4more_nationalbyAge, aes(x=Year, y=ACE4more_prop, color=age_group)) +
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), width=0.1) +
  geom_line() +
  geom_point() +
  xlab("Year of survey") +
  ylab("% with 4 or more ACEs") +
  ylim(0,14) +
  theme_bw() +
  theme(legend.justification = c(1.2,1.2), legend.position = c(1,1)) +
  ggtitle("Proportion (95% CI) of children and adolescents aged less \nthan 18 years who reported at least 4 ACEs by age group") +
  theme(plot.title = element_text(hjust = 0.5))

aces4more_nationalbySex = read_excel("data.xlsx", sheet = "ACES4more_nationalbySex")
ggplot(aces4more_nationalbySex, aes(x=Year, y=ACE4more_prop, color=Gender)) +
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), width=0.1, position = position_dodge(0.1)) +
  geom_line(position = position_dodge(0.1)) +
  geom_point(position = position_dodge(0.1)) +
  xlab("Year of survey") +
  ylab("% with 4 or more ACEs") +
  ylim(0,10) +
  theme_bw() +
  theme(legend.justification = c(1.2,1.2), legend.position = c(1,1)) +
  ggtitle("Proportion (95% CI) of children and adolescents aged less \nthan 18 years who reported at least 4 ACEs by gender") +
  theme(plot.title = element_text(hjust = 0.5))

aces4more_nationalbyRaceeth = read_excel("data.xlsx", sheet = "ACES4more_nationalbyraceth")
aces4more_nationalbyRaceeth$race_ethnicity <- factor(aces4more_nationalbyRaceeth$race_ethnicity,
                                            levels = c("Hispanic", "White, Non-Hispanic", "Black, Non-Hispanic", "Asian, Non-Hispanic", "Other, Non-Hispanic"),
                                            labels = c("Hispanic", "White, Non-Hispanic", "Black, Non-Hispanic", "Asian, Non-Hispanic", "Other, Non-Hispanic"))
ggplot(aces4more_nationalbyRaceeth, aes(x=Year, y=ACE4more_prop, color=race_ethnicity)) +
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), width=0.1, position = position_dodge(0.1)) +
  geom_line(position = position_dodge(0.1)) +
  geom_point(position = position_dodge(0.1)) +
  xlab("Year of survey") +
  ylab("% with 4 or more ACEs") +
  ylim(0,18) +
  theme_bw() +
  theme(legend.justification = c(1.2,1.2), legend.position = c(1,1)) +
  ggtitle("Proportion (95% CI) of children and adolescents aged less \nthan 18 years who reported at least 4 ACEs by race/ethnicity") +
  theme(plot.title = element_text(hjust = 0.5))

#State-Level Analysis
#2016/17 data
nsch16_17 <- bind_rows(nsch16a, nsch17a)
nsch16_17$FWC16_17=nsch16_17$FWC/2
nsch16_17[nsch16_17==99] <- NA
nsch16_17 <- svydesign(id=~HHID, weights=~FWC16_17,strata=~STRATUM+FIPSST, data=nsch16_17)
summary(nsch16_17)
#2018/19 data
nsch18_19 <- bind_rows(nsch18a, nsch19a)
nsch18_19$FWC18_19=nsch18_19$FWC/2
nsch18_19[nsch18_19==99] <- NA
nsch18_19 <- svydesign(id=~HHID, weights=~FWC18_19,strata=~STRATUM+FIPSST, data=nsch18_19)
summary(nsch18_19)

#State-level prevalence of children and adolescents with 4 or more ACEs
#2016/17 #change year 2018/19 and 2020
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==1), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==2), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==4), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==5), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==6), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==8), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==9), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==10), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==11), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==12), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==13), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==15), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==16), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==17), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==18), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==19), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==20), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==21), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==22), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==23), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==24), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==25), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==26), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==27), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==28), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==29), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==30), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==31), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==32), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==33), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==34), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==35), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==36), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==37), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==38), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==39), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==40), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==41), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==42), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==44), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==45), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==46), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==47), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==48), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==49), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==50), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==51), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==53), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==54), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==55), method = "li"), "ci")
attr(svyciprop(~I(ACE4more==3), subset(nsch16_20, FIPSST==56), method = "li"), "ci")

#MULTI-YEAR ANALYSIS
#Descriptive statistics
svymean(~SC_AGE_YEARS, design=nsch16_20, na=TRUE)
svysd(~SC_AGE_YEARS, design=nsch16_20, na=TRUE)
prop.table(svytable(~age3, nsch16_20))*100
prop.table(svytable(~SC_SEX, nsch16_20))*100
prop.table(svytable(~raceASIA, nsch16_20))*100
#Overall in 2016-2020 (multi-year estimate) #Change for 2+, 4+ ACEs
nsch16_20 <- svydesign(id=~HHID, weights=~FWC16_20,strata=~STRATUM+FIPSST, data=nsch16_20)
summary(nsch16_20)
#ACEdivorce=ace3, ACEdeath=ace4, ACEjail=ace5, ACEdomviol=ace6, ACEneighviol=ace7,
#ACEmhealth=ace8, ACEdrug=ace9, ACEdiscrim=ace10
#Overall in 2016-20
prop.table(svytable(~ACEdiscrim, nsch16_20))
attr(svyciprop(~I(ACEdiscrim==1), nsch16_20, method = "li"), "ci")
#By age group
svyciprop(~I(ACEdiscrim==1), subset(nsch16_20, age3==1), method = "li")
svyciprop(~I(ACEdiscrim==1), subset(nsch16_20, age3==2), method = "li")
svyciprop(~I(ACEdiscrim==1), subset(nsch16_20, age3==3), method = "li")
#By sex
svyciprop(~I(ACEdomviol==1), subset(nsch16_20, SC_SEX==1), method = "li")
svyciprop(~I(ACEdomviol==1), subset(nsch16_20, SC_SEX==2), method = "li")
#By race/ethnicity
svyciprop(~I(ACEdomviol==1), subset(nsch16_20, raceASIA==1), method = "li")
svyciprop(~I(ACEdomviol==1), subset(nsch16_20, raceASIA==2), method = "li")
svyciprop(~I(ACEdomviol==1), subset(nsch16_20, raceASIA==3), method = "li")
svyciprop(~I(ACEdomviol==1), subset(nsch16_20, raceASIA==4), method = "li")
svyciprop(~I(ACEdomviol==1), subset(nsch16_20, raceASIA==5), method = "li")

#Mean ACEs score
#Overall in 2016-20
prop.table(svytable(~ACEct, nsch16_20))*100
svymean(~ACEct, nsch16_20, na=TRUE)
confint(svymean(~ACEct, nsch16_20, na=TRUE))
#By age group
svymean(~ACEct, subset(nsch16_20, age3==1), na=TRUE)
confint(svymean(~ACEct, subset(nsch16_20, age3==1), na=TRUE))
svymean(~ACEct, subset(nsch16_20, age3==2), na=TRUE)
confint(svymean(~ACEct, subset(nsch16_20, age3==2), na=TRUE))
svymean(~ACEct, subset(nsch16_20, age3==3), na=TRUE)
confint(svymean(~ACEct, subset(nsch16_20, age3==3), na=TRUE))
#By sex
svymean(~ACEct, subset(nsch16_20, SC_SEX==1), na=TRUE)
confint(svymean(~ACEct, subset(nsch16_20, SC_SEX==1), na=TRUE))
svymean(~ACEct, subset(nsch16_20, SC_SEX==2), na=TRUE)
confint(svymean(~ACEct, subset(nsch16_20, SC_SEX==2), na=TRUE))
#By race/ethnicity
svymean(~ACEct, subset(nsch16_20, raceASIA==1), na=TRUE)
confint(svymean(~ACEct, subset(nsch16_20, raceASIA==1), na=TRUE))
svymean(~ACEct, subset(nsch16_20, raceASIA==2), na=TRUE)
confint(svymean(~ACEct, subset(nsch16_20, raceASIA==2), na=TRUE))
svymean(~ACEct, subset(nsch16_20, raceASIA==3), na=TRUE)
confint(svymean(~ACEct, subset(nsch16_20, raceASIA==3), na=TRUE))
svymean(~ACEct, subset(nsch16_20, raceASIA==4), na=TRUE)
confint(svymean(~ACEct, subset(nsch16_20, raceASIA==4), na=TRUE))
svymean(~ACEct, subset(nsch16_20, raceASIA==5), na=TRUE)
confint(svymean(~ACEct, subset(nsch16_20, raceASIA==5), na=TRUE))



#svyciprop(~I(ACE4more==3), nsch16_20, method = "li") #5.41, used as a vertical dotted line in forest plots
#attr(svyciprop(~I(ACE4more==3), nsch16_20, method = "li"), "ci")
#aces4more16_20 = data.frame(value=5.41, aces4more_16_20="% 4 or more ACEs, \n2016-2020 5-year estimate")
#Overall in 2016/17
svyciprop(~I(ACE4more==3), nsch16_17, method = "li")
attr(svyciprop(~I(ACE4more==3), nsch16_17, method = "li"), "ci")

#Forest plots
aces4more_State = read_excel("data copy.xlsx", sheet = "ACES4more_State")
aces4more_State$State <- factor(aces4more_State$State, levels=rev(aces4more_State$State))
#first 10 states
aces4more_State1 = subset(aces4more_State, aces4more_State$FIPSST<=12)

ggplot(aces4more_State, aes(x=State, y=ACE4more_prop, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(size=0.1) +
  geom_hline(data=aces4more16_20, aes(yintercept=value, linetype=aces4more_16_20), key_glyph=draw_key_vline) +
  coord_flip() +
  scale_linetype_manual(values = "dashed", name = "") +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci)) +
  xlab("State") + 
  ylab("% with 4 or more ACEs") +
  ggtitle("Proportion (95% CI) of children and adolescents aged less \nthan 18 years who reported at least 4 ACEs by State") +
  theme_bw() +
  theme(plot.title=element_text(size=12, face = "bold"),
        axis.text.y=element_text(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(),
        axis.title.y=element_text(size=12))+
  scale_y_continuous(breaks = c(2,4,6,8,10,12,14),position = "right",limits=c(1,15))

aces_indvAll = read_excel("data copy.xlsx", sheet = "ACEindv_all")
ggplot(aces_indvAll, aes(x=ace, y=estimate, ymin=lower_ci, ymax=upper_ci)) +
  geom_bar(aes(x=reorder(ace, estimate), y=estimate), col="black", fill="white", stat="identity") +
  geom_errorbar( aes(ymin=lower_ci, ymax=upper_ci), width=0.4) +
  coord_flip() +
  theme_bw() +
  xlab("Adverse Childhood Experiences") +
  ylab("% of Children and Adolescents") +
  ggtitle("")

aces_indvbyAge = read_excel("data copy.xlsx", sheet = "ACEindv_byAge")
aces_indvbyAge$age_group <- factor(aces_indvbyAge$age_group, levels=c("0-5 years", "6-11 years", "12-17 years"), ordered = T)
ggplot(aces_indvbyAge, aes(y=estimate, x=age_group)) + 
  geom_bar(position="dodge", col="black", fill="white", stat="identity") +
  geom_errorbar( aes(ymin=lower_ci, ymax=upper_ci), width=0.4) +
  facet_wrap(~ace) +
  coord_flip() +
  theme_bw() +
  theme(legend.position="none") +
  xlab("Age group") +
  ylab("% of Children and Adolescents") +
  ggtitle("")

aces_indvbySex = read_excel("data copy.xlsx", sheet = "ACEindv_bySex")
ggplot(aces_indvbySex, aes(y=estimate, x=sex)) + 
  geom_bar(position="dodge", col="black", fill="white", stat="identity") +
  geom_errorbar( aes(ymin=lower_ci, ymax=upper_ci), width=0.4) +
  facet_wrap(~ace) +
  coord_flip() +
  theme_bw() +
  theme(legend.position="none") +
  xlab("Sex") +
  ylab("% of Children and Adolescents") +
  ggtitle("")

aces_indvbyRaceEth = read_excel("data copy.xlsx", sheet = "ACEindv_byRaceEth")
ggplot(aces_indvbyRaceEth, aes(y=estimate, x=race_ethn)) + 
  geom_bar(position="dodge", col="black", fill="white", stat="identity") +
  geom_errorbar( aes(ymin=lower_ci, ymax=upper_ci), width=0.4) +
  facet_wrap(~ace) +
  coord_flip() +
  theme_bw() +
  theme(legend.position="none") +
  xlab("Race/ethnicity") +
  ylab("% of Children and Adolescents") +
  ggtitle("")

#next 10 states
aces4more_State2 = subset(aces4more_State, aces4more_State$FIPSST>12 & aces4more_State$FIPSST<=23)
ggplot(aces4more_State2, aes(x=Year, y=ACE4more_prop, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(aes(col=Year), size=0.1) +
  geom_hline(data=aces4more16_20, aes(yintercept=value, linetype=aces4more_16_20), key_glyph=draw_key_vline) +
  scale_linetype_manual(values = "dashed", name = "") +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci, col=Year)) +
  facet_wrap(~State, strip.position = "left", nrow = 10, scales = "free_y") +
  xlab("State") + 
  ylab("% with 4 or more ACEs") +
  ggtitle("Proportion (95% CI) of children and adolescents aged less \nthan 18 years who reported at least 4 ACEs by State") +
  theme_bw() +
  theme(plot.title=element_text(size=14, face = "bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold"))+
  scale_y_continuous(breaks = c(2,4,6,8,10,12,14),position = "right",limits=c(1,15)) +
  coord_flip()
#next 10 states
aces4more_State3 = subset(aces4more_State, aces4more_State$FIPSST>23 & aces4more_State$FIPSST<=33)
ggplot(aces4more_State3, aes(x=Year, y=ACE4more_prop, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(aes(col=Year), size=0.1) +
  geom_hline(data=aces4more16_20, aes(yintercept=value, linetype=aces4more_16_20), key_glyph=draw_key_vline) +
  scale_linetype_manual(values = "dashed", name = "") +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci, col=Year)) +
  facet_wrap(~State, strip.position = "left", nrow = 10, scales = "free_y") +
  xlab("State") + 
  ylab("% with 4 or more ACEs") +
  ggtitle("Proportion (95% CI) of children and adolescents aged less \nthan 18 years who reported at least 4 ACEs by State") +
  theme_bw() +
  theme(plot.title=element_text(size=14, face = "bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold"))+
  scale_y_continuous(breaks = c(2,4,6,8,10,12,14),position = "right",limits=c(1,15)) +
  coord_flip()
#next 10 states
aces4more_State4 = subset(aces4more_State, aces4more_State$FIPSST>33 & aces4more_State$FIPSST<=44)
ggplot(aces4more_State4, aes(x=Year, y=ACE4more_prop, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(aes(col=Year), size=0.1) +
  geom_hline(data=aces4more16_20, aes(yintercept=value, linetype=aces4more_16_20), key_glyph=draw_key_vline) +
  scale_linetype_manual(values = "dashed", name = "") +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci, col=Year)) +
  facet_wrap(~State, strip.position = "left", nrow = 10, scales = "free_y") +
  xlab("State") + 
  ylab("% with 4 or more ACEs") +
  ggtitle("Proportion (95% CI) of children and adolescents aged less \nthan 18 years who reported at least 4 ACEs by State") +
  theme_bw() +
  theme(plot.title=element_text(size=14, face = "bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold"))+
  scale_y_continuous(breaks = c(2,4,6,8,10,12,14),position = "right",limits=c(1,15)) +
  coord_flip()
#last 11 states
aces4more_State5 = subset(aces4more_State, aces4more_State$FIPSST>44)
ggplot(aces4more_State5, aes(x=Year, y=ACE4more_prop, ymin=lower_ci, ymax=upper_ci)) +
  geom_pointrange(aes(col=Year), size=0.1) +
  geom_hline(data=aces4more16_20, aes(yintercept=value, linetype=aces4more_16_20), key_glyph=draw_key_vline) +
  scale_linetype_manual(values = "dashed", name = "") +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci, col=Year)) +
  facet_wrap(~State, strip.position = "left", nrow = 11, scales = "free_y") +
  xlab("State") + 
  ylab("% with 4 or more ACEs") +
  ggtitle("Proportion (95% CI) of children and adolescents aged less \nthan 18 years who reported at least 4 ACEs by State") +
  theme_bw() +
  theme(plot.title=element_text(size=14, face = "bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(face="bold"),
        axis.title=element_text(size=12,face="bold"),
        strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold"))+
  scale_y_continuous(breaks = c(2,4,6,8,10,12,14),position = "right",limits=c(1,15)) +
  coord_flip()


