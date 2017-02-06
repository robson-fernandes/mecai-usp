movimentacao = read.table('movimentacao.csv', header=TRUE, sep=";")
attach(movimentacao)


tSet = movimentacao[1:1000, c("diaSemana","codTipoVenda","codProduto", "quantidade", "vlrUnitario")]



library(bnlearn)                       # Load the package in R
library(forecast)

bn <- naive.bayes(tSet, "quantidade")
pred <- predict(bn, learning.test)
table(pred, learning.test[,"A"])

training.set = tSet # This is training set to learn the parameters
test.set = tSet[1:10, ]  # This is test set to give as evidence
res = hc(training.set)                 # learn BN structure on training set data 
fitted = bn.fit(res, training.set)     # learning of parameters
pred = predict(fitted, "quantidade", test.set)  # predicts the value of node C given test set
cbind(pred, test.set[, "quantidade"])           # compare the actual and predicted
accuracy(f = pred, x = test.set[, "quantidade"])