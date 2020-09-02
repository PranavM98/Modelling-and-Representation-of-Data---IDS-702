data<-read.table("Listings_QueenAnne.txt", header = TRUE, sep = " ")

data$room_type<-factor(data$room_type)

g1<-ggplot(data, aes(x=bedrooms, y=price)) + geom_boxplot()
g2<-ggplot(data, aes(x=accommodates, y=price)) + geom_boxplot()
g3<-ggplot(data, aes(x=host_identity_verified, y=price)) + geom_boxplot()
g4<-ggplot(data, aes(x=host_is_superhost, y=price)) + geom_boxplot()
g5<-ggplot(data, aes(x=room_type, y=price)) + geom_boxplot()
g6<-ggplot(data, aes(x=bathrooms, y=price)) + geom_boxplot()


chisq.test(data$room_type,data$accommodates)




ggarrange(g1,g2,g3,g4,g5,g6, 
          ncol = 3, nrow = 2)



plot(data$price, data$bedrooms)
plot(data$price, data$accommodates)
plot(data$price, data$bathrooms)
plot(data$price, data$host_identity_verified)
plot(data$price, data$host_is_superhost)
plot(data$price, data$room_type)


model<-lm(log(price) ~ bedrooms+accommodates+bathrooms+host_identity_verified+host_is_superhost+room_type,data=data)
model1<-lm(log(price) ~ bedrooms+accommodates+bathrooms+host_identity_verified+room_type,data=data)


#Residual Standard error (Like Standard Deviation)
k=length(model$coefficients)-1 #Subtract one to ignore intercept
SSE=sum(model$residuals**2)
n=length(model$residuals)
sqrt(SSE/(n-(1+k))) #Residual Standard Error

k=length(model1$coefficients)-1 #Subtract one to ignore intercept
SSE=sum(model1$residuals**2)
n=length(model1$residuals)
sqrt(SSE/(n-(1+k))) #Residual Standard Error



hist(log(data$price))

plot(model)

#there are influencial points and leveraging points
plot(model,which=1,col=c("blue4"))
lev_scores=hatvalues(model)
p=7
n=305
plot(lev_scores,col=ifelse(lev_scores > (2*p/n), 'red2', 'navy'),type="h", ylab="Leverage score",xlab="Obs. number",main="Leverage Scores")
text(x=c(1:n)[lev_scores > (2*p/n)]+c(rep(2,4),-2,2),y=lev_scores[lev_scores > (2*p/n)], labels=c(1:n)[lev_scores > (2*p/n)])


#REMOVE INFLUENCE AND OUTLIERS. Points have a cook's distance above 1. Standard Residuals plot, above and below -4

data<-data[- c(31,138),]


model<-lm(log(price) ~ bedrooms+accommodates+bathrooms+host_identity_verified+host_is_superhost+room_type,data=data)
plot(model,which=1,col=c("blue4"))

