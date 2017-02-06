m = read.table('movimentacao.csv', header=TRUE, sep=";")
attach(m)

movimentacao = m[1:50, ]


library(e1071)
svm.model <- svm(quantidade ~ diaSemana,
                 data=movimentacao,
                 type="nu-regression",
                 kernel="radial",
                 scale = FALSE)

movimentacao$collected.svm <- predict(svm.model,data=movimentacao)

#Plot
plot(quantidade ~ diaSemana,
     data=movimentacao,
     pch=16,
     col="blue",
     xlab="Future Due Value",
     ylab="Cash Collection",
     main="SVM Model")

points(movimentacao$quantidade, 
       movimentacao$collected.svm, 
       col = "green", 
       pch=17)
