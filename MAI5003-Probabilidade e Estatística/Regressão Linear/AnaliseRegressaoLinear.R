#----------------------------------------
# Robson Fernandes da Silva <robson.fernandes@usp.br>
# N. USP - 10107495
# Trabalho - Regress??o Linear
#----------------------------------------

dataOxigenio = read.table('/Users/robsonfernandes/Desktop/regressao/oxigenio.txt',header=T)
attach(dataOxigenio)

#----------------------------------------
# ESTATISTICA DESCRITIVA
#----------------------------------------
summary(dataOxigenio)
var(dataOxigenio$tempo)
var(dataOxigenio$oxigenio)
sd(dataOxigenio$tempo)
sd(dataOxigenio$oxigenio)


#Teste de correlao - Coeficiente de correlacao de Pearson
cor.test(dataOxigenio$tempo,dataOxigenio$oxigenio)

#----------------------------------------
# DIAGRAMA DE DISPERSAO
#----------------------------------------
plot(tempo,oxigenio,xlab="Tempo",ylab="Oxigenio")
points(mean(tempo), mean(oxigenio),col="red",lwd=5,lty=9)


#----------------------------------------
# AJUSTE NO MODELO
#----------------------------------------
#Modelo Ajustado
modelo = lm(tempo ~ oxigenio)
modelo
summary(modelo)


#----------------------------------------
# ANALISE DO MODELO DE REGRESSAO
#----------------------------------------
predict(modelo)  # valores preditos pela equacao de regressao
residuals(modelo) # valores residuos

#Verificando Normalidade dos residuos
shapiro.test(residuals(modelo))

#----------------------------------------
#ANALISE DE RESIDUOS
#----------------------------------------
plot(fitted(modelo),residuals(modelo),xlab="Valores Ajustados",ylab="Residuos")
abline(h=0)

plot(tempo,residuals(modelo),xlab="tempo",ylab="Residuos")
abline(h=0)

qqnorm(residuals(modelo), ylab="Residuos")
qqline(residuals(modelo))
#----------------------------------------

#----------------------------------------
# GRAFICO DE REGRESAO
#----------------------------------------
plot(tempo ~ oxigenio)
abline(modelo, col=2, lty=2, lwd=2)
legend("top", legend=c("valores observados", "valores ajustados"), lty=c(NA,2), col=c(1,2), lwd=1:2, bty="n", pch=c(1,NA))