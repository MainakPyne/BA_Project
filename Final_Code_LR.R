setwd("/Users/sandhyasriraman/Downloads/")
mdata=read.csv("Final_data.csv")
#Removing extra variables from dataset

mdata$X=NULL
mdata$actor_1_name = NULL
mdata$actor_2_name = NULL
mdata$actor_3_name = NULL
mdata$movie_title = NULL
mdata$plot_keywords = NULL
mdata$pc = NULL
mdata$gross1 = NULL
mdata$title_year = NULL
mdata$director_name=NULL
mdata$profit=NULL
mdata=mdata[mdata$actor1_age>10,]
mdata=mdata[mdata$actor1_age<90,]
subdata=data.frame(mdata[,-c(34:85)])
#subdata$budget=NULL

cor(mdata) # to identify and remove correlated variables

subdata$actor_3_facebook_likes=NULL #since we don't use any other parameters for actor 3
subdata$cast_total_facebook_likes=NULL #highly correlated with actor facebook likes
subdata$pro_class=NULL
subdata$profit1=NULL
subdata$imdb_sc=NULL

library(glmnet)
set.seed(1)
mtrain=sample(1:nrow(subdata), 0.75*nrow(subdata))
mtest=(-mtrain)
m_test=subdata[mtest,]
m_train=subdata[mtrain,]


# Linear Regression of gross against all variables

linreg=lm(m_train$gross~.,data=m_train)
summary(linreg)
plot(linreg)
pred_1=predict(linreg,m_test)
actual=m_test$gross
mse_1=mean((actual-pred_1)^2)
mse_1


# Plot shows residuals are correlated--- hence Y variable is transformed
# Square root of gross
subdata$gross1 <- rep(NA, nrow(subdata))
subdata[,"gross1"]=sqrt(subdata$gross)
subdata$gross=NULL

#Again splitting training and test
set.seed(1)
mtrain=sample(1:nrow(subdata), 0.75*nrow(subdata))
mtest=(-mtrain)
m_test=subdata[mtest,]
m_train=subdata[mtrain,]




# Linear regression with sqrt of gross
linreg=lm(m_train$gross1~.,data=m_train)
summary(linreg)
plot(linreg)


pred_2=predict(linreg,m_test)
#actual=y[mtest]
mse_2=mean((m_test$gross-pred_2)^2)
mse_2

# Lasso for feature selection


x=model.matrix(subdata$gross1~.,data=subdata)[,-1]
y=subdata$gross1
lamgrid=10^seq(10,-2,length=100)
lass =cv.glmnet(x[mtrain,],y[mtrain] ,alpha=1, nfolds=10, lambda=lamgrid) 
plot(lass)
final_lam = lass$lambda.min
final_lam
laregr = glmnet(x[mtrain,],y[mtrain], alpha=1, lambda=final_lam)
coef(laregr)

#running on test data to find MSE
prediction = predict(laregr, x[mtest,])
actual = y[mtest]
mse_finalmodel_lasso= (mean((actual-prediction)^2))
mse_finalmodel_lasso


#After variable selection

dataforlinreg=data.frame(m_train[,-c(36,34,28,20)])
linreg=lm(dataforlinreg$gross1~.,data=dataforlinreg)
summary(linreg)
m_test_new=data.frame(m_test[,c(36,34,28,20)])
pred_3=predict(linreg,m_test)
#actual=y[mtest]
mse_3=mean((m_test$gross-pred_3)^2)
mse_3
plot(linreg)




