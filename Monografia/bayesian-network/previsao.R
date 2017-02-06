movimentacao = read.table('movimentacao.csv', header=TRUE, sep=";")
attach(movimentacao)


#model <- lm(quantidade ~ diaSemana + codProduto + codTipoVenda , movimentacao)
#summary(model)

tSet = movimentacao[1:50, ]
#plot(tSet, pch=16)

library(e1071)
model <- svm(quantidade ~ diaSemana + codProduto + codTipoVenda, data=tSet, scale = FALSE )

ftable(predict(model), quantidade)

#summary(model)
#predictedY <-predict(model, tSet)



#predictedY <- predict(model, tSet)

#points(tSet$diaSemana, predictedY, col = "red", pch=4)

#library(bnlearn)                       # Load the package in R
#library(forecast)


#training.set = movimentacao[1:200000, ] # This is training set to learn the parameters
#test.set = movimentacao[4001:4010, ]  # This is test set to give as evidence
#res = hc(training.set)                 # learn BN structure on training set data 
#fitted = bn.fit(res, training.set)     # learning of parameters
#pred = predict(fitted, "quantidade", test.set)  # predicts the value of node C given test set
#cbind(pred, test.set[, "quantidade"])           # compare the actual and predicted
#accuracy(f = pred, x = test.set[, "quantidade"])