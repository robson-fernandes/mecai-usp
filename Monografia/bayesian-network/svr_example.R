#Load support vector library
library (e1071)

movimentacao = read.table('movimentacao.csv', header=TRUE, sep=";")
attach(movimentacao)


#Create toy dataset
#X <- 1:10
#Y <- X*X
#data <- data.frame ( X , Y )

data <- movimentacao[1:100, ]

#Create SVR model
svrmodel <- svm( quantidade ~ diaSemana, data)
tuneResult <- tune(svm, quantidade ~ diaSemana,  data = data,ranges = list(epsilon = seq(0,0.2,0.01), cost= 2^(2:9)))
svrmodel <- tuneResult$best.model
predictedY <- predict( svrmodel , newdata = data.frame( quantidade <- data$quantidade+0.5))

#plot data
plot ( data )
#add line through data points
lines ( data , col = "blue" ) 

# Add points for fitted svrmodel
points(data$quantidade+0.5 , predictedY , col = "red" , pch=4)
lines( data$quantidade+0.5 , predictedY , col = "red" )