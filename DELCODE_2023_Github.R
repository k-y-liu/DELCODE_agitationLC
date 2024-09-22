#Load data file
ERdata<-read.csv(file="/Users/kathyliu/Dropbox/MRC_study/DELCODE/Data/ERdata309.csv",  header = T, sep = ",")
library(lavaan)
library(dplyr)

#new dataframe scaled to mean 5 SD 2
colnames(ERdata)
scaled <- ERdata%>%
  mutate_at(.vars = c(8,15:22,24,63:290,292:316, 324:354,356:361, 363:379),
            .funs = function(x) scale(x) * 2 + 5)

#emotion regulation functional network
#CFA
model<-'
fER=~ mPFC_ACCsubg+ACCsubg_Ramyg+ACCsubg_Lamyg+mPFC_Lamyg+mPFC_Ramyg+EXEC
'
 
model.fit <- cfa(model, data=scaled,estimator="ML", missing="fiml")
summary(model.fit,fit.measures=TRUE, standardized=TRUE, rsquare=TRUE, ci=TRUE)

fitMeasures(model.fit,c("cfi","rmsea","srmr")) 
MI<-modificationIndices(model.fit,sort=TRUE)
MI
 
#after MIs
model<-'
fER=~ mPFC_ACCsubg+ACCsubg_Ramyg+ACCsubg_Lamyg+mPFC_Lamyg+mPFC_Ramyg+EXEC
ACCsubg_Ramyg ~~    mPFC_Lamyg
ACCsubg_Lamyg ~~    mPFC_Ramyg
ACCsubg_Ramyg ~~ ACCsubg_Lamyg 
mPFC_Lamyg ~~    mPFC_Ramyg
'
model.fit <- cfa(model, data=scaled,estimator="ML", missing="fiml")
summary(model.fit,fit.measures=TRUE, standardized=TRUE, rsquare=TRUE, ci=TRUE)
fitMeasures(model.fit,c("cfi","rmsea","srmr")) 

#loadings of mPFC-ACCsub and EXEC onto ER were 0.13-0.18
#try remove low loadings: covariances led to model identification issues

#structural ER regions latent factor
#CFA
model<-'
sEReg=~Amy_TIVavg +lMedFroCbr_TIV +rMedFroCbr_TIV +ACC_TIVavg
#lMedFroCbr_TIV ~~ rMedFroCbr_TIV
#lMedFroCbr_TIV ~~     ACC_TIVavg
'

model.fit <- cfa(model, data=scaled,estimator="ML", missing="fiml" ) 
summary(model.fit,fit.measures=TRUE, standardized=TRUE, rsquare=TRUE, ci=TRUE)
fitMeasures(model.fit,c("cfi","rmsea","srmr")) 
MI<-modificationIndices(model.fit,sort=TRUE) 
MI

# Extract the factor scores for the fER and sER latent factor and merge with ERdata and scaled 
factor_scores <- lavPredict(model.fit, type = "lv")
scaled <- cbind(scaled, factor_scores)
ERdata <- cbind(ERdata, factor_scores)

#is there a relationship between agitation presence or severity with fER?
#SEM
model<-'
sEReg=~Amy_TIVavg +lMedFroCbr_TIV +rMedFroCbr_TIV +ACC_TIVavg
lMedFroCbr_TIV ~~ rMedFroCbr_TIV
lMedFroCbr_TIV ~~     ACC_TIVavg

fER=~ mPFC_ACCsubg+ACCsubg_Ramyg+ACCsubg_Lamyg+mPFC_Lamyg+mPFC_Ramyg+EXEC
ACCsubg_Ramyg ~~    mPFC_Lamyg
ACCsubg_Lamyg ~~    mPFC_Ramyg
ACCsubg_Ramyg ~~ ACCsubg_Lamyg 
mPFC_Lamyg ~~    mPFC_Ramyg

npiq03a~sEReg+fER+age+cdrtot+edyears

#npiq03b~sEReg+fER+age+cdrtot+edyears
#npiq03b~0*sEReg+ER+age+cdrtot+edyears

cdrtot~~age
cdrtot~~edyears
age~~edyears
'

#for agitation presence
model.fit <- cfa(model, data=scaled,estimator="WLSMV") 
summary(model.fit,fit.measures=TRUE, standardized=TRUE, rsquare=TRUE, ci=TRUE)
fitMeasures(model.fit,c("cfi","rmsea","srmr")) #not great fit. 
constrained <- cfa(model, data=scaled,estimator="WLSMV") # 
anova(model.fit,constrained) 

#for agitation severity
model.fit <- cfa(model, data=scaled,estimator="ML", missing="fiml" ) 
summary(model.fit,fit.measures=TRUE, standardized=TRUE, rsquare=TRUE, ci=TRUE)
constrained <- cfa(model, data=scaled,estimator="ML", missing="fiml") 
anova(model.fit,constrained) 

#LC subsample analysis
#Is there a relationship between BL agitation and LC signal intensity?  - ns
model <- glm(npiq03a ~LC_Max_ratio_bilat,family=binomial(link='logit'),data=ERdata)
model <- glm(npiq03a ~LC_Max_ratio_bilat+age+cdrtot,family=binomial(link='logit'),data=ERdata)

model <- glm(npiq03a ~rost_LC_Max_ratio_bilat,family=binomial(link='logit'),data=ERdata) 
model <- glm(npiq03a ~rost_LC_Max_ratio_bilat+age+cdrtot,family=binomial(link='logit'),data=ERdata) #

model <- glm(npiq03a ~mid_LC_Max_ratio_bilat,family=binomial(link='logit'),data=ERdata) 
model <- glm(npiq03a ~mid_LC_Max_ratio_bilat+age+cdrtot,family=binomial(link='logit'),data=ERdata) #

model <- glm(npiq03a ~Caud_LC_Max_ratio_bilat,family=binomial(link='logit'),data=ERdata) 
model <- glm(npiq03a ~Caud_LC_Max_ratio_bilat+age+cdrtot,family=binomial(link='logit'),data=ERdataLC) #

summary(model)
confint(model)

# Is there a relationship between agitation severity and LC signal intensity?
model <- lm(npiq03b ~ LC_Max_ratio_bilat, data=ERdata) #ns
model <- lm(npiq03b ~ LC_Max_ratio_bilat+age+cdrtot, data=ERdata)  #  

summary(model)
confint(model)["LC_Max_ratio_bilat", ]

model <- lm(npiq03b ~ rost_LC_Max_ratio_bilat, data=ERdata) # positive
model <- lm(npiq03b ~ rost_LC_Max_ratio_bilat+age+cdrtot, data=ERdata)  #  +ve
summary(model)
confint(model)["rost_LC_Max_ratio_bilat", ]

model <- lm(npiq03b ~ mid_LC_Max_ratio_bilat, data=ERdata) # ns
model <- lm(npiq03b ~ mid_LC_Max_ratio_bilat+age+cdrtot, data=ERdata)  #
summary(model)
confint(model)["mid_LC_Max_ratio_bilat", ]

model <- lm(npiq03b ~ Caud_LC_Max_ratio_bilat, data=ERdata) # 
model <- lm(npiq03b ~ Caud_LC_Max_ratio_bilat+age+cdrtot, data=ERdata)  # 
summary(model)
confint(model)["Caud_LC_Max_ratio_bilat", ]

#Is there a relationship between latent ER factor scores and LC signal intenisty?
model <- lm(ER ~ LC_Max_ratio_bilat, data=ERdata) #  
model <- lm(ER ~ LC_Max_ratio_bilat+sER+age+cdrtot+edyears, data=ERdata) #
model <- lm(ER ~ rost_LC_Max_ratio_bilat, data=ERdata) # 
model <- lm(ER ~ rost_LC_Max_ratio_bilat+age+cdrtot+sER+edyears, data=ERdata) # 
model <- lm(ER ~ mid_LC_Max_ratio_bilat, data=ERdata) # 
model <- lm(ER ~ mid_LC_Max_ratio_bilat+age+cdrtot+sER+edyears, data=ERdata) # 
model <- lm(ER ~ Caud_LC_Max_ratio_bilat, data=ERdata) # 
model <- lm(ER ~ Caud_LC_Max_ratio_bilat+age+cdrtot+sER+edyears, data=ERdata) # 

summary(model)
confint(model)["LC_Max_ratio_bilat", ]
confint(model)["rost_LC_Max_ratio_bilat", ]
confint(model)["mid_LC_Max_ratio_bilat", ]
confint(model)["Caud_LC_Max_ratio_bilat", ]

#exec function with LC
model <- lm(EXEC ~ LC_Max_ratio_bilat, data=ERdataLC) #  
model <- lm(EXEC ~ LC_Max_ratio_bilat+age, data=ERdataLC) # 
model <- lm(EXEC ~ LC_Max_ratio_left+age+cdrtot, data=ERdataLC) #  
summary(model)

#mpFC function with LC
model <- lm(mPFC_Lamyg ~ LC_Max_ratio_bilat, data=ERdata) #  negative relatioship
model <- lm(mPFC_Lamyg ~ LC_Max_ratio_bilat+age+cdrtot+sER+edyears, data=ERdata) # negative relationship
model <- lm(mPFC_Lamyg ~ rost_LC_Max_ratio_bilat+age+cdrtot+sER+edyears, data=ERdata) # negative relationship
model <- lm(mPFC_Lamyg ~ mid_LC_Max_ratio_bilat+age+cdrtot+sER+edyears, data=ERdata) # ns relationship
model <- lm(mPFC_Lamyg ~ Caud_LC_Max_ratio_bilat+age+cdrtot+sER+edyears, data=ERdata) # ns relationship

model <- lm(mPFC_Ramyg ~ LC_Max_ratio_bilat, data=ERdataLC) #  
model <- lm(mPFC_Ramyg ~ LC_Max_ratio_right+age+cdrtot, data=ERdataLC) # 
model <- lm(mPFC_Ramyg ~ LC_Max_ratio_bilat, data=ERdataLC) #  
model <- lm(mPFC_Ramyg ~ LC_Max_ratio_right+age+cdrtot, data=ERdataLC) # 

model <- lm(ACCsubg_Lamyg ~ LC_Max_ratio_bilat, data=ERdataLC) #  
model <- lm(ACCsubg_Lamyg ~ LC_Max_ratio_bilat+age+cdrtot, data=ERdataLC) # 
model <- lm(ACCsubg_Lamyg ~ rost_LC_Max_ratio_bilat+age+cdrtot, data=ERdataLC) # 

summary(model)
confint(model)["LC_Max_ratio_bilat", ]
confint(model)["rost_LC_Max_ratio_bilat", ]
confint(model)["mid_LC_Max_ratio_bilat", ]
confint(model)["Caud_LC_Max_ratio_bilat", ]


