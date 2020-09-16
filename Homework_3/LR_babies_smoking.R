library(arm)
library(pROC)
library(e1071)
library(caret)
library(ggplot2)
require(gridExtra)
data<-read.csv('smoking.csv',header=T)
data


data$mrace[data$mrace<6]<-0
data$inc[data$inc==7]<-6
data$smoke<-factor(data$smoke)
#data$inc<-factor(data$inc)
data$med<-factor(data$med)
data$mrace<-factor(data$mrace)

data$premature<-0
data$premature[data$gestation<270]<-1
#data$premature[data$gestation>=270]<-0
data$premature_fac<-factor(data$premature)





str(data)
table(data$premature)
# EXPLORATORY DATA ANALYSIS

##### MPREGWT #########

# IMPORTANT
ggplot(data,aes(x=mpregwt, y=premature_fac)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Premature vs MPREGWT",
       x="Premature",y="Pre Pregnancy Weight") + 
  theme_classic() + theme(legend.position="none") 

##### Parity ############

# NOT IMPORTANT
ggplot(data,aes(x=parity, y=premature_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Premature vs Parity",
       x="Parity",y="Premature") + 
  theme_classic() + theme(legend.position="none") 


##### MAGE ############

# NOT IMPORTANT (LOOKS LIKE)
ggplot(data,aes(x=mage, y=premature_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Premature vs Age",
       x="Mother Age",y="Premature") + 
  theme_classic() + theme(legend.position="none") 


####### MOTHERS HEIGHT #######

#DOESNT SEEM AS IMPORTANT

ggplot(data,aes(x=mht, y=premature_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Premature vs Mother Height",
       x="Mother Height",y="Premature") + 
  theme_classic() + theme(legend.position="none") 



# MOTHERS INCOME
ggplot(data,aes(x=inc, y=premature_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Premature vs Income",
       x="Mother Height",y="Premature") + 
  theme_classic() + theme(legend.position="none") 





binnedplot(y=data$premature,data$mage,xlab="Age",ylim=c(0,1),col.pts="navy",
           ylab ="Premature",main="Binned Mother's Age and Premature",
           col.int="white") # this is to set the SD lines to white and ignore them

binnedplot(y=data$premature,data$mht,xlab="Height",ylim=c(0,1),col.pts="navy",
           ylab ="Premature",main="Binned Mother's Height and Premature",
           col.int="white") # this is to set the SD lines to white and ignore them


binnedplot(y=data$premature,data$mpregwt,xlab="Pre-Pregnancy weight",ylim=c(0,1),col.pts="navy",
           ylab ="Premature",main="Binned Mother's Pre-pregnancy Weight and Premature",
           col.int="white") # this is to set the SD lines to white and ignore them

binnedplot(y=data$premature,data$parity,xlab="Parity",ylim=c(0,1),col.pts="navy",
           ylab ="Premature",main="Binned Mother's Parity and Premature",
           col.int="white") # this is to set the SD lines to white and ignore them



######## SMOKE ###############
table(data[,c("premature","smoke")])
table(data[,c("premature","smoke")])/sum(table(data[,c("premature","smoke")]))

apply(table(data[,c("premature_fac","smoke")])/sum(table(data[,c("premature_fac","smoke")])),
      2,function(x) x/sum(x))

# NOT AS SIGNIFICANT


tapply(data$premature, data$smoke, function(x) table(x)/sum(table(x)))
# Finally, we can even try a chi-squared test for independence.
chisq.test(table(data[,c("premature_fac","smoke")]))


###### INC #########

apply(table(data[,c("premature_fac","inc")])/sum(table(data[,c("premature_fac","inc")])),
      2,function(x) x/sum(x))

# NOT AS SIGNIFICANT
# Finally, we can even try a chi-squared test for independence.
chisq.test(table(data[,c("premature_fac","inc")]))


##### RACE ##############

# SEEMS SIGNIFICANT
tapply(data$premature, data$mrace, function(x) table(x)/sum(table(x)))
apply(table(data[,c("premature_fac","mrace")])/sum(table(data[,c("premature_fac","mrace")])),
      2,function(x) x/sum(x)) 

# Finally, we can even try a chi-squared test for independence.
chisq.test(table(data[,c("premature_fac","mrace")]))



########## MED #############

# SEEMS SIGNIFICANT

apply(table(data[,c("premature_fac","med")])/sum(table(data[,c("premature_fac","med")])),
      2,function(x) x/sum(x)) 

# Finally, we can even try a chi-squared test for independence.
chisq.test(table(data[,c("premature_fac","med")]))

######### INTERACTIONS ###########

# PREMATURE VS SMOKE VS RACE - TREND REMAINS THE SAME
library('dplyr')
race0<-data %>%
  filter(mrace==0)
race6<-data %>%
  filter(mrace==6)
race7 <-data %>%
  filter(mrace==7)
race8 <-data %>%
  filter(mrace==8)
race9 <-data %>%
  filter(mrace==9)

apply(table(race0[,c("premature_fac","smoke")])/sum(table(race0[,c("premature_fac","smoke")])),
      2,function(x) x/sum(x))

apply(table(race6[,c("premature_fac","smoke")])/sum(table(race6[,c("premature_fac","smoke")])),
      2,function(x) x/sum(x))

apply(table(race7[,c("premature_fac","smoke")])/sum(table(race7[,c("premature_fac","smoke")])),
      2,function(x) x/sum(x))

apply(table(race8[,c("premature_fac","smoke")])/sum(table(race8[,c("premature_fac","smoke")])),
      2,function(x) x/sum(x))

apply(table(race9[,c("premature_fac","smoke")])/sum(table(race9[,c("premature_fac","smoke")])),
      2,function(x) x/sum(x))


plot(apply(table(race9[,c("premature_fac","smoke")])/sum(table(race9[,c("premature_fac","smoke")])),
           2,function(x) x/sum(x)), col="red")




# SMOKE VS MED VS PREMATURE -- SEEMS TO BE INTERESTING


med0<-data %>%
  filter(med==0)
med1<-data %>%
  filter(med==1)
med2 <-data %>%
  filter(med==2)
med3 <-data %>%
  filter(med==3)
med4 <-data %>%
  filter(med==4)
med5 <-data %>%
  filter(med==5)
med7 <-data %>%
  filter(med==7)


apply(table(med0[,c("premature_fac","smoke")])/sum(table(med0[,c("premature_fac","smoke")])),
      2,function(x) x/sum(x))

apply(table(med1[,c("premature_fac","smoke")])/sum(table(med1[,c("premature_fac","smoke")])),
      2,function(x) x/sum(x))

apply(table(med2[,c("premature_fac","smoke")])/sum(table(med2[,c("premature_fac","smoke")])),
      2,function(x) x/sum(x))

apply(table(med3[,c("premature_fac","smoke")])/sum(table(med3[,c("premature_fac","smoke")])),
      2,function(x) x/sum(x))

apply(table(med4[,c("premature_fac","smoke")])/sum(table(med4[,c("premature_fac","smoke")])),
      2,function(x) x/sum(x))

apply(table(med5[,c("premature_fac","smoke")])/sum(table(med5[,c("premature_fac","smoke")])),
      2,function(x) x/sum(x))

apply(table(med7[,c("premature_fac","smoke")])/sum(table(med7[,c("premature_fac","smoke")])),
      2,function(x) x/sum(x))



# SMOKE AGE PREMATURE - Seems interesting 
ggplot(data,aes(x=premature_fac, y=mage, fill=premature_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="PREMATURE VS AGE by SMOKE",
       x="PREMATURe",y="AGE") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ smoke)

#SMOKE HEIGHT PREMATURE - Seems Interesting

ggplot(data,aes(x=premature_fac, y=mht, fill=premature_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="PREMATURE VS HEIGHT by SMOKE",
       x="PREMATURE",y="HEIGHT") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ smoke)

#SMOKE WEIGHT PREMATURE - Seems Interesting

ggplot(data,aes(x=premature_fac, y=mpregwt, fill=premature_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="PREMATURE VS PRE PREGNANT WEIGHT by SMOKE",
       x="PREMATURE",y="Pre-Preg Weight") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ smoke)

#SMOKE PARITY PREMATURE - Seems Interesting

ggplot(data,aes(x=premature_fac, y=parity, fill=premature_fac)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="PREMATURE VS PARITY by SMOKE",
       x="PREMATURE",y="Pre-Preg Weight") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ smoke)


# INTERESTING INTERACTION
# PARITY AND SMOKE
# WEIGHT AND SMOKE
# HEIGHT AND SMOKE
# AGE AND SMOKE
# MED AND SMOKE

########## MODEL CREATION #################

u#Centering
data$magec <- data$mage - mean(data$mage)
data$mhtc <- data$mht - mean(data$mht)
data$mpregwtc <- data$mpregwt - mean(data$mpregwt)
data$parityc <- data$parity - mean(data$parity)

data$incc <- data$inc - mean(data$inc)

AIC_model<-glm(formula = premature ~  med+mrace + mpregwtc + smoke + smoke:mrace, family = binomial(link = logit), data = data)



model<- glm(premature ~ magec + mhtc + mpregwtc + parityc+ med+ incc +mrace+ smoke
            +parityc:smoke + mpregwtc:smoke + mhtc:smoke+
              magec:smoke +med:smoke +mrace:smoke +inc:smoke,
               data = data, family = binomial(link=logit))
summary(model)

m<- glm(premature ~ magec + mhtc + mpregwtc + parityc+ med+ incc +mrace+ smoke
            +parityc:smoke + mpregwtc:smoke + mhtc:smoke+
              magec:smoke +med:smoke +mrace:smoke +inc:smoke,
            data = data, family = binomial(link=logit))

model1<- glm(premature ~ smoke+magec + mhtc + mpregwtc +med+mrace,
            data = data, family = binomial(link=logit))




anova(model,model1,test='Chisq')
summary(model1)
summary(model)


#save the raw residuals
rawresid1 <- residuals(model,"resp")

#binned residual plots
binnedplot(x=fitted(model),y=rawresid1,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#looks good

binnedplot(x=data$magec,y=rawresid1,xlab="Age centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

binnedplot(x=data$mhtc,y=rawresid1,xlab="Height centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

binnedplot(x=data$mpregwtc,y=rawresid1,xlab="Pre Pregnant Weight centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

binnedplot(x=data$parityc,y=rawresid1,xlab="Parity centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

binnedplot(x=data$mhtc,y=rawresid1,xlab="Height centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")



summary(data$mage)

head(data)



tapply(rawresid1, data$smoke, mean) 


###### Model validation

#let's do the confusion matrix with .5 threshold
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(model) >= 0.5, "1","0")),
                            as.factor(data$premature),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")] #True positive rate and True negative rate
#Maybe we can try to increase that accuracy.
#Also, the TNR looks low here.

#first, let's repeat with the marginal percentage in the data
mean(data$premature)
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(model) >= mean(data$premature), "1","0")),
                            as.factor(data$premature),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")]
#huge difference!  seems a lot of predicted probabilities are in the .5 yo .58  range, so cutoff matters.
#either way, we have large off-diagonal numbers. specificity is sensitive to the cutoff

#look at ROC curve
roc(data$premature,fitted(model),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")

roc(data$premature,fitted(AIC_model),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")


roc(data$premature,fitted(model1),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")

#pretty tight to the line -- not a strongly predictive logistic regression

#let's see if we can improve the model.

#AIC FORWARD
n <- nrow(data)
null_model <- glm(premature~smoke,data=data,family=binomial(link=logit))
AIC<-step(null_model,scope=formula(model),direction="both",
     trace=0)

AIC$call
AIC_model<-glm(formula = premature ~ med + mrace + mpregwtc+smoke+smoke:med, 
               family = binomial(link = logit), data = data)

model_w<-glm(formula = premature ~smoke+mrace + mpregwtc+smoke:med, 
                   family = binomial(link = logit), data = data)


summary(AIC_model)

anova(model_w,AIC_model,test="Chisq")

confint(AIC_model)


RSS <- c(crossprod(AIC_model$residuals))
MSE <- RSS / length(AIC_model$residuals)
RSME <-sqrt(MSE)


AIC_model1<-glm(formula = premature ~  mrace+  med + mpregwtc +smoke:med , family = binomial(link = logit), data = data)
pander(anova(AIC_model1,AIC_model, test="Chisq"))




anova(test,AIC_model,test="Chisq")


# RACE SMOKE INTERACTION IS NOT SIGNIFICANT
# SMOKE MED INTERACTION IS SIGNIFICANT

rawresid <- residuals(AIC_model,"resp")

binnedplot(x=fitted(AIC_model),y=rawresid,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#looks good

binnedplot(x=data$magec,y=rawresid,xlab="Age centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

binnedplot(x=data$mhtc,y=rawresid,xlab="Height centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

binnedplot(x=data$mpregwtc,y=rawresid,xlab="Pre Pregnant Weight centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

binnedplot(x=data$parityc,y=rawresid1,xlab="Parity centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

binnedplot(x=data$mhtc,y=rawresid1,xlab="Height centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(AIC_model) >= mean(data$premature), "1","0")),
                            as.factor(data$premature),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")]
#huge difference!  seems a lot of predicted probabilities are in the .5 yo .58  range, so cutoff matters.
#either way, we have large off-diagonal numbers. specificity is sensitive to the cutoff

#look at ROC curve
roc(data$premature,fitted(AIC_model),plot=T,print.thres=mean(data$premature),legacy.axes=T,
    print.auc =T,col="red3")
mean(data$premature)

plot(AIC_model)


n <- nrow(model.matrix(AIC_model))
p <- ncol(model.matrix(AIC_model)) 
lev_scores <- hatvalues(AIC_model)
plot(lev_scores,col=ifelse(lev_scores > (2*p/n), 'red2', 'navy'),type="h",
     ylab="Leverage score",xlab="Index",main="Leverage Scores for all observations") 
text(x=c(1:n)[lev_scores > (2*p/n)]+c(rep(2,4),-2,2),y=lev_scores[lev_scores > (2*p/n)],
     labels=c(1:n)[lev_scores > (2*p/n)])

plot(AIC_model,which=4,col=c("blue4"))

