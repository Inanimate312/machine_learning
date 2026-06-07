library(ISLR); library(ggplot2); library(caret)

data(Wage)
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)

# Feature plot
featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage,
            plot="pairs")

# Qplot, no color
qplot(age,wage,data=training)

# Qplot with color
qplot(age,wage,colour=jobclass,data=training)

# Add regression smoothers
qq <- qplot(age,wage,colour=education,data=training)
qq + geom_smooth(method='lm',formula=y~x)

# Cut2, making factors (Hmisc package)
library(Hmisc)
cutWage <- cut2(training$wage,g=3)
table(cutWage)

# Boxplot with cut2
p1 <- qplot(cutWage,age,data=training,fill=cutWage,
            geom=c("boxplot"))
p1

# Boxplot with points added
library(gridExtra)
p2 <- qplot(cutWage,age,data=training,fill=cutWage,
            geom=c("boxplot", "jitter"))
grid.arrange(p1,p2,ncol=2)

# Tables
t1 <- table(cutWage, training$jobclass)
t1

prop.table(t1,1)

# Density plot
qplot(wage, colour=education,data=training,geom="density")
