library(bnlearn)                       # Load the package in R
library(forecast)

data(gaussian.test)
training.set = gaussian.test[1:4000, ] # This is training set to learn the parameters
test.set = gaussian.test[4001:4010, ]  # This is test set to give as evidence
res = hc(training.set)                 # learn BN structure on training set data 
fitted = bn.fit(res, training.set)     # learning of parameters
pred = predict(fitted, "C", test.set)  # predicts the value of node C given test set
cbind(pred, test.set[, "C"])           # compare the actual and predicted
accuracy(f = pred, x = test.set[, "C"])
