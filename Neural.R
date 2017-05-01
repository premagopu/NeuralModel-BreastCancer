#Clear the Environment
rm(list=ls())

#Read csv file
data<-read.csv("wisc_bc_data.csv")

#Copy data for backup
DataFrame<- data

#Dimensions of DataFrame
dim(DataFrame)
head(DataFrame,2,range)

#install the package neuralnet
install.packages("neuralnet", dependencies = TRUE)
require(neuralnet)

#Replacing B with 1 and M with 2
diagnosis <- ifelse(DataFrame$diagnosis=='B',1,
                  ifelse(DataFrame$diagnosis=='M',2,0))

#Calculating minValue and maxValue
maxValue <- apply(DataFrame[3:32],2,max)
minValue <- apply(DataFrame[3:32],2,min)

#Creating the data frame with scaled values for variables 3 to 32
DataFrame1 <- as.data.frame(scale(DataFrame[3:32], center = minValue, scale = maxValue-minValue))
View(DataFrame1)

#Binding the data frame
df <- cbind.data.frame(diagnosis,DataFrame1)

head(DataFrame1,2,range)

#Data Partition
ind <-  sample(nrow(df),as.integer(0.70 * nrow(df)))
traind <- df[ind,]
testd <- df[-ind,]

#form for neuralnet
allVars <- colnames(df)
predictorVars <- allVars[!allVars%in%"diagnosis"]
predictorVars <- paste(predictorVars,collapse = "+")
form <- as.formula(paste("diagnosis~",predictorVars,collapse = "+"))

#Neural Model
neuralmodel <- neuralnet(formula = form, hidden = c(5,3,2),linear.output = T, data = traind)

#Plotting Neural Model
plot(neuralmodel)

#Predictions
predictions <- compute(neuralmodel,testd[,2:31])
str(predictions)

predictions1 <- predictions$net.result*(max(testd$diagnosisgnosis)-min(testd$diagnosis)) +min(testd$diagnosis)
actualValues <- (testd$diagnosis)*(max(testd$diagnosis)-min(testd$diagnosis)) + min(testd$diagnosis)

#Mean Error Calculation
MSE <- sum((predictions1 - actualValues)^2)/nrow(testd)
MSE

#plot(testd$diagnosis,predictions1,col = 'blue', main = "Actual vs Predictions", pch = 1, cex=0.9,type="p",xlab = "Actual",ylab = "Prediction")

