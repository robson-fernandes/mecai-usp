data = read.table('movimentacao.csv', header=TRUE, sep=";")
attach(data)

#data = data[1:1000, c("quantidade","diaSemana")]
data = data[1:100]

# Plot the data
plot(data$quantidade, data$diaSemana, pch=16)

# Create a linear regression model
#model <- lm(quantidade ~ diaSemana + codTipoVenda, data)
model <-  lm(quantidade ~ diaSemana + codTipoVenda, data = data)
summary(model)

# Add the fitted line
#abline(model)

# make a prediction for each X
predictedY <- predict(model, data, interval="confidence")

# display the predictions
points(data$diaSemana, predictedY, col = "blue", pch=4)