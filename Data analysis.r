#Assognment on Lalonde 3 ways

#Problem 1

#loading the matching library and lalonde dataset

install.packages("interplot")
library(Matching)

data(lalonde)

#boosting
#Plots a boost figure showing relative importance of variables based on a machine learning techinique
install.packages(("gbm"))
library(gbm)
boost<- gbm(re78~., data = lalonde, n.trees = 5000, interaction.depth = 4)
summary(boost)

#Creating the a linear model with re78 as the dependent variable and nodegr==0
lm.0 <- lm(re78~re74+re75+age+educ+treat, data = subset(lalonde,nodegr==0))
summary(lm.0) #Calling on the summary of the linear model
sjp.lm(lm.0, show.loess.ci = T, show.values = T, show.summary = T)
confint(lm.0)
lm.0.sim <- sim(lm.0)
coef(lm.0.sim) #simulated coefficients all equally likely under this model
t <- apply(coef(lm.0.sim), 2, mean)
sderoft <- apply(coef(lm.11.sim), 2, sd)
uppp <- t + 2*sderoft
loww <- t - 2*sderoft
uppp
loww


#Creating the a linear model with re78 as the dependent variable and nodegr==1
lm.1 <- lm(re78~re75+re74+age+educ+treat, data = subset(lalonde,nodegr==1))
summary(lm.1)
sjp.lm(lm.1, show.loess.ci = T, show.values = T, show.summary = T)
confint(lm.1)
lm.1.sim <- sim(lm.1)
coef(lm.1.sim) #simulated coefficients all equally likely under this model


#Problem 2

#Creating the fit with interaction terms
lm.11 <- lm(re78~treat+re74+re75+age+educ + nodegr + I(nodegr * treat), data = lalonde)
summary(lm.11)
confint(lm.11, level=0.95) #Getting 95% confidence interval using inbuilt function
lm.11.sim<- sim(lm.11)
coef(lm.11.sim)   #simulated coefficients all equally likely under this model
meanoft <- apply(coef(lm.11.sim), 2, mean)  #Standard deviation of simulated coefficients
sderoft <- apply(coef(lm.11.sim), 2, sd)
upp <- meanoft + 2*sderoft
low <- meanoft - 2*sderoft
upp
low

#Creating a relevant plot
library(sjPlot)
sjp.lm(lm.11, show.loess.ci = T, show.values = T, show.summary = T)
sjp.int(lm.11, show.values = T, show.ci = T)


#Problem 3

#creating u78
lalonde$u78[lalonde$re78==0]<-1 #Makes u78 = 1 if re78 = 0
lalonde$u78[lalonde$re78>0]<-0  #Makes u78 = 0 if re78 >0


#Creating a logistic regression with u78 for nodegr == 0
glm.0 <- glm(u78~re74+re75+age+educ+treat, data = subset(lalonde,nodegr==0), family = binomial)
summary(glm.0)
confint(glm.0)

#Creating a logistic regression with u78 for nodegr == 1
glm.1 <- glm(u78~re75+re74+age+educ+treat, data = subset(lalonde,nodegr==1), family = binomial)
summary(glm.1)
confint(glm.1)

#Problem 4

#creating randomforests
library(randomForest)
library(devtools)
library(tree)
set.seed(415)
bag.lalonde2 <- randomForest(re78~., data=lalonde,
                             mtry=8, ntree = 5000, importance =TRUE)
plot(bag.lalonde2)
cat("There are", length(which(lalonde$u78 == 1)),
    "unemployed observations in the total in 1978.")

## How does the OOB test error compare to the error in the test set?

## check the importance of the variables
varImpPlot(bag.lalonde2)
importance(bag.lalonde2)
getTree(bag.lalonde2,1,labelVar = T)
treesss <- tree(re78~u78+re74+re75+age+educ+treat, data=lalonde)
plot(treesss)
text(treesss, pretty = 0)



#cross validation
set.seed((4))
cv.proceed <- cv.tree(treesss )
cv.proceed
par(mfrow=c(1,2))
plot(cv.proceed$size, cv.proceed$dev, type = "b")
plot(cv.proceed$k, cv.proceed$dev, type = "b")
