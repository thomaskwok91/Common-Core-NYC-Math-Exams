setwd("C:/Users/thoma/Desktop/Thomas Research Set/")
ethnicity <- read.csv("Ethnicity_Borough.csv")
library(tidyverse)
library(dplyr)
library(car)
library(lattice)
library(lme4)
library(nlme)
library(MASS)
library(lmerTest)
library(geepack)
library(ggthemes)
library(ggplot2)
library(gridExtra)
library(gee)

##########################################    Introduction
#Number of Proficient
number_proficient_borough <- ethnicity %>%
  group_by(Borough, Group, Grade) %>%
  summarise(Sum=sum(X..Level.3.4, na.rm = TRUE))

#lattice plot Number
xyplot(Sum~Grade | factor(Group)+factor(Borough), data=number_proficient_borough,
       scales=list(x=list(at=c(6,7,8))),
       xlab = "Grade",
       ylab = "Total Students Proficient",
       main = "Number of students ranked as proficient by Borough and Year across Grade",
       type="b",
       as.table=TRUE)


#Percent of Proficient
percent_proficient_borough <- ethnicity %>%
  group_by(Borough, Group, Grade) %>%
  summarise(Avg=mean(X..Level.3.4.1, na.rm = TRUE))

#lattice plot Percent
xyplot(Avg~Grade | factor(Group)+factor(Borough), data=percent_proficient_borough,
       scale=list(x=list(at=c(6,7,8))),
       xlab = "Grade",
       ylab = "Percentage of Students Proficient",
       main = "Percentage of students ranked as proficient by Borough and Year across Grade",
       type="b",
       as.table=TRUE)

################################################   Materials

#Number Students Tested
number_tested_borough <- ethnicity %>%
  group_by(Borough, Group, Grade) %>%
  summarise(Sum=sum(Number.Tested, na.rm = TRUE))

#Lattice Plot number Tested
xyplot(Sum~Grade | factor(Group)+factor(Borough), data=number_tested_borough,
       scale=list(x=list(at=c(6,7,8))),
       xlab = "Grade",
       ylab = "Students Tested",
       main = "Total number of students tested by Borough and Year across Grade. ",
       type="b",
       as.table=TRUE)

#Testers
df <- matrix(c(70305, 59875, 68091, 56999, 66259, 54762, 65371, 53102), 4, 2, byrow=TRUE)
colnames(df) <- c("Grade 7", "Grade 8")
rownames(df) <- c("2013-2014", "2014-2015", "2015-2016", "2016-2017")
df

########################################################  Modeling

#Average Score Overall per Group
ethnicity_lattice <- ethnicity %>%
  group_by(Group, Grade) %>%
  summarise(Avg=mean(Mean.Scale.Score, na.rm = TRUE))
#Average Score Per Borough per Group
ethnicity_lat_borough <- ethnicity %>%
  group_by(Borough, Group, Grade) %>%
  summarise(Avg=mean(Mean.Scale.Score, na.rm = TRUE))

#Lattice Plot Average Score Overall per Group
xyplot(Avg~Grade | factor(Group), data=ethnicity_lattice,
       scale=list(x=list(at=c(6,7,8))),
       xlab = "Grade",
       ylab = "Average Mean Score",
       main = "Mean score of all students in NYC by grade",
       type="b",
       layout=c(7,1))

#Lattice Plot Average Score per Borough per Group
xyplot(Avg~Grade | factor(Group)+factor(Borough), data=ethnicity_lat_borough,
       scale=list(x=list(at=c(6,7,8))),
       xlab = "Grade",
       ylab = "Average Mean Score",
       main = "Mean score of students by Borough and Year across Grade",
       type="b",
       as.table=TRUE)

#Borough trend
Bronx <- read.csv("Bronx.csv")
Brooklyn <- read.csv("Brooklyn.csv")
Manhattan <- read.csv("Manhattan.csv")
Queens <- read.csv("Queens.csv")
Staten_Island <- read.csv("Staten Island.csv")

ggplot(data=Bronx, aes(x=Year, y=Mean.Scale.Score, color=factor(Grade)))+geom_line()+geom_point()+ggtitle("Bronx Data")
ggplot(data=Brooklyn, aes(x=Year, y=Mean.Scale.Score, color=factor(Grade)))+geom_point()+geom_line()+ggtitle("Brooklyn Data")
ggplot(data=Manhattan, aes(x=Year, y=Mean.Scale.Score, color=factor(Grade)))+geom_point()+geom_line()+ggtitle("Manhattan Data")
ggplot(data=Queens, aes(x=Year, y=Mean.Scale.Score, color=factor(Grade)))+geom_point()+geom_line()+ggtitle("Queens Data")
ggplot(data=Staten_Island, aes(x=Year, y=Mean.Scale.Score, color=factor(Grade)))+geom_point()+geom_line()+ggtitle("Staten Island Data")


#corr matrix
mat <- with(eth_model, matrix(c(Mean.Scale.Score[Group==0], Mean.Scale.Score[Group==1], Mean.Scale.Score[Group==2], Mean.Scale.Score[Group==3], Mean.Scale.Score[Group==4]), ncol=5))
dr <- cor(mat)
colnames(dr) <- c("Group 0", "Group 1", "Group 2", "Group 3", "Group 4")
rownames(dr) <- c("Group 0", "Group 1", "Group 2", "Group 3", "Group 4")
dr

######################## Ethnicity Model

#convert into factors
ethnicity <- read.csv("Ethnicity_Borough2.csv")

ethnicity <- ethnicity %>% 
  mutate(
    Borough = as.factor(Borough),
    Ethnicity = as.factor(Ethnicity),
    Grade = as.factor(Grade),
    Year_ = as.factor(Year))
eth_model <- ethnicity %>%
  dplyr::select(Group, Borough, Grade, Year, Year_, Ethnicity, Mean.Scale.Score)

#With Year
mixed_eth_model1 <- lmerTest::lmer(Mean.Scale.Score~Borough+Ethnicity+Grade+Year+(1|Group), data=eth_model, REML=FALSE)
mixed_eth_model2 <- lmerTest::lmer(Mean.Scale.Score~Borough+Ethnicity+Grade+Year_+(1|Group), data=eth_model, REML=FALSE)

#eth_model1
summary(mixed_eth_model1)
print(mixed_eth_model1,correlation=TRUE)
VarCorr(mixed_eth_model1)

#eth_model2
summary(mixed_eth_model2)
VarCorr(mixed_eth_model2)

##################################### Model #2

#model 2 - Compound Symmetry
eth_cs_model2 <- gls(Mean.Scale.Score~Ethnicity+Borough+Grade+Year_,
                     correlation = corCompSymm(form= ~1|Group),
                     data=eth_model)

#model 2 results
summary(eth_cs_model2)

#correlation matrix
VarCov <- getVarCov(eth_cs_model2, individual="Asian_Bronx_2013-2015")
Corr<-VarCov/VarCov[1,1]
VarCov
Corr


##################################### Model #3

#model 3 - Autoregressive model
eth_ar_model2 <- gls(Mean.Scale.Score~Ethnicity+Borough+Grade+Year_, data=eth_model,
                     correlation = corAR1(form= ~1|Group))

#model 3 results
summary(eth_ar_model2)

#correlation matrix
VarCov <- getVarCov(eth_ar_model2, individual="Asian_Bronx_2013-2015")
Corr<-VarCov/VarCov[1,1]
VarCov
Corr
##################################### GEE model

#gee
eth_mgee <- gee(Mean.Scale.Score~Ethnicity+Borough+Grade+Year_, data=eth_model, id=Group, corstr = "exchangeable")

#gee results
summary(eth_mgee)

#gee
eth_mgee2 <- geese(Mean.Scale.Score~Ethnicity+Borough+Grade+Year_, data=eth_model, id=Group, corstr="ar1")

#gee results
summary(eth_mgee2)

######################## Gender Model

gender <- read.csv("Gender_Borough.csv")

#convert into factors
gender <- gender %>% 
  mutate(
    Borough = as.factor(Borough),
    Gender = as.factor(Gender),
    Grade = as.factor(Grade),
    Year = as.factor(Year))
gend_model <- gender %>%
  dplyr::select(Borough, Grade, Year, Gender, Group, Mean.Scale.Score)

#model 1 - Mixed Model

#With Year
mixed_gend_model2 <- lmerTest::lmer(Mean.Scale.Score~Borough+Gender+Grade+Year+(1|Group), data=gend_model, REML=FALSE)


#model 1 results
summary(mixed_gend_model2)

##################################### Model #2

#model 2 - Compound Symmetry
gend_cs_model <- gls(Mean.Scale.Score~Gender+Borough+Grade+Year, data=gend_model,
                corr = corCompSymm(0, form= ~1|Group))

#model 2 results
summary(gend_cs_model)

##################################### Model #3

#model 3 - Autoregressive model
gend_ar_model <- gls(Mean.Scale.Score~Gender+Borough+Grade+Year, data=gend_model,
                corr = corAR1(0, form= ~1|Group))

#model 3 results
summary(gend_ar_model)

##################################### GEE model

#gee
gend_mgee <- geeglm(Mean.Scale.Score~Gender+Borough+Grade+Year, data=gend_model, id=Group, corstr="ar1")

summary(gend_mgee)



######################## ELL Model

ell <- read.csv("ELL_Borough.csv")

#convert into factors
ell <- ell %>% 
  mutate(
    Borough = as.factor(Borough),
    ELL = as.factor(ELL),
    Grade = as.factor(Grade),
    Year=as.factor(Year))
ell_model <- ell %>%
  dplyr::select(Borough, Grade, Year, ELL, Group, Mean.Scale.Score)

#model 1 - Mixed Model

#With Year
mixed_ell_model2 <- lmerTest::lmer(Mean.Scale.Score~Borough+ELL+Grade+Year+(1|Group), data=ell_model, REML=FALSE)


#model 1 results
summary(mixed_ell_model2)

##################################### Model #2

#model 2 - Compound Symmetry
ell_cs_model <- gls(Mean.Scale.Score~ELL+Borough+Grade+Year, data=ell_model,
                     corr = corCompSymm(0, form= ~1|Group))

#model 2 results
summary(ell_cs_model)

##################################### Model #3

#model 3 - Autoregressive model
ell_ar_model <- gls(Mean.Scale.Score~ELL+Borough+Grade+Year, data=ell_model,
                     corr = corAR1(0, form= ~1|Group))

#model 3 results
summary(ell_ar_model)

##################################### GEE model

#gee
ell_mgee <- geeglm(Mean.Scale.Score~ELL+Borough+Grade+Year, data=ell_model, id=Group, corstr="ar1")

summary(ell_mgee)

######################## SWD Model

swd <- read.csv("SWD_Borough.csv")

#convert into factors
swd <- swd %>% 
  mutate(
    Borough = as.factor(Borough),
    SWD = as.factor(SWD),
    Grade = as.factor(Grade),
    Year = as.factor(Year))
swd_model <- swd %>%
  dplyr::select(Borough, Grade, Year, SWD, Group, Mean.Scale.Score)

#model 1 - Mixed Model

#With Year
mixed_swd_model2 <- lmerTest::lmer(Mean.Scale.Score~Borough+SWD+Grade+Year+(1|Group), data=swd_model, REML=FALSE)


#model 1 results
summary(mixed_swd_model2)

##################################### Model #2

#model 2 - Compound Symmetry
swd_cs_model <- gls(Mean.Scale.Score~SWD+Borough+Grade+Year, data=swd_model,
                     corr = corCompSymm(0, form= ~1|Group))

#model 2 results
summary(swd_cs_model)

##################################### Model #3

#model 3 - Autoregressive model
swd_ar_model <- gls(Mean.Scale.Score~SWD+Borough+Grade+Year, data=swd_model,
                     corr = corAR1(0, form= ~1|Group))

#model 3 results
summary(swd_ar_model)

##################################### GEE model

#gee
swd_mgee <- geeglm(Mean.Scale.Score~SWD+Borough+Grade+Year, data=swd_model, id=Group, corstr="ar1")

summary(swd_mgee)

######################## ECON Model

econ <- read.csv("Econ_Borough.csv")

#convert into factors
econ <- econ %>% 
  mutate(
    Borough = as.factor(Borough),
    ECON = as.factor(ECON),
    Grade = as.factor(Grade),
    Year = as.factor(Year))
econ_model <- econ %>%
  dplyr::select(Borough, Grade, Year, ECON, Group, Mean.Scale.Score)

#model 1 - Mixed Model

#With Year
mixed_econ_model2 <- lmerTest::lmer(Mean.Scale.Score~Borough+ECON+Grade+Year+(1|Group), data=econ_model, REML=FALSE)


#model 1 results
summary(mixed_econ_model2)

##################################### Model #2

#model 2 - Compound Symmetry
econ_cs_model <- gls(Mean.Scale.Score~ECON+Borough+Grade+Year, data=econ_model,
                     corr = corCompSymm(0, form= ~1|Group))

#model 2 results
summary(econ_cs_model)

##################################### Model #3

#model 3 - Autoregressive model
econ_ar_model <- gls(Mean.Scale.Score~ECON+Borough+Grade+Year, data=econ_model,
                     corr = corAR1(0, form= ~1|Group))

#model 3 results
summary(econ_ar_model)

##################################### GEE model

#gee
econ_mgee <- geeglm(Mean.Scale.Score~ECON+Borough+Grade+Year, data=econ_model, id=Group, corstr="exchangeable")

summary(econ_mgee)

