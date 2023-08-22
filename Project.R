

setwd("D:/C DRIVE/All Docs/Great Learning/Project")

mydata=read.csv("Factor-Hair-Revised.csv",header = TRUE)

mydata

attach(mydata)

mydata

names(mydata)

cor(mydata[,2:13])

Model1=lm(Satisfaction~ProdQual)

summary(Model1)

Model2=lm(Satisfaction~Ecom)

summary(Model2)

Model3=lm(Satisfaction~TechSup)

summary(Model3)

Model4=lm(Satisfaction~CompRes)

summary(Model4)

Model5=lm(Satisfaction~Advertising)

summary(Model5)

Model6=lm(Satisfaction~ProdLine)

summary(Model6)

Model7=lm(Satisfaction~SalesFImage)

summary(Model7)

Model8=lm(Satisfaction~ComPricing)

summary(Model8)

Model9=lm(Satisfaction~WartyClaim)

summary(Model9)

Model10=lm(Satisfaction~OrdBilling)

summary(Model10)

Model11=lm(Satisfaction~DelSpeed)

summary(Model11)

mydata=mydata[,2:12]

cor(mydata)

library(nFactors)

# Eigen Value Computation

ev=eigen(cor(mydata)) # get eigenvalues

ev

EigenValue = ev$values

EigenValue

Factor=c(1,2,3,4,5,6,7,8,9,10,11)

Scree=data.frame(Factor,EigenValue)

plot(Scree,main="Scree Plot",col="Blue")

lines(Scree,col="Red")

library(psych)

Unrotate=principal(mydata, nfactors = 4, rotate="none")

print(Unrotate,digits = 4)

UnrotatedProfile = plot(Unrotate,row.names(Unrotate$loadings))

Rotate=principal(mydata,nfactors = 4,rotate="varimax")

print(Rotate,digits = 3)

RotatedProfile = plot(Rotate,row.names(Rotate$loadings),cex=1.0)

Rotate$scores

Scores=Rotate$scores

library(xlsx)




