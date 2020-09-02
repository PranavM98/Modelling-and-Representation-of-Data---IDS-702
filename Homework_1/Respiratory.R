data <- read.csv("Respiratory.csv")

#EDA
summary(data)
plot(data$Age,log(data$Rate))
plot(data$Age, data$Rate)
cor(data$Age,data$Rate)

                                                                                        colors = c("#6D9EC1", "white", "#E46726"))

hist(log(data$Rate))
hist(data$Rate)


hist(data$Age)

ggplot(data,aes(x=Age, y=model$residual)) +
  geom_point(alpha = .7) + geom_hline(yintercept=0,col="red3") + theme_classic() + labs(title="Residuals vs Age",x="Age",y="Residuals")

# Normal -> YES
model<-lm(log(Rate)~Age,data=data)


plot(model, which=2)
#CENTERING
data$agec <- c(scale(data$Age,scale=F))
modelc<-lm(log(Rate)~agec,data=data)


pander(summary(model))

#plot for linearity
#Pattern if there are more data points in a particular location
plot(data$Age,model$residuals)


plot(model)

#plot 1 - independence and equal variance

plt()

confint(model,level=0.95)



new <- data.frame(Age=c(1,18,29))


preds<-predict(model,new,interval = "prediction")
f<-data.frame(exp(preds))
final<-cbind(new,f)
pander(final)



