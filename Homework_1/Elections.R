data<-read.csv('Elections.csv')


summary(data)

plot(d)
# Outlier on the X Axis. Most points are within 1000, except for Palm
ggplot(data, aes(Buchanan2000,Bush2000)) +geom_point()

data<- data[!(data$County=='Palm Beach'),]

#TRY LOG ON BOTH

hist(log(data$Buchanan2000))

plot(data$Bush2000,model$residuals)




model<-lm(log(Buchanan2000)~log(Bush2000),data=data)
plot(model)


#Question 4 - At maximum, Buchanan would have recieved 830 votes. The remaning votes were meant for Gore
new=data.frame(Bush2000=152846)
exp(predict(model,new,interval = 'prediction'))



summary(data$Buchanan2000)

