# ------------------------------------------
# Análise de Importancia
# ------------------------------------------

#
# Limpa workspace e variáveis
#
ls()
rm(list=ls())
graphics.off()

#
# Pacotes
#
library(ggplot2)
library(caret)

#
# Funcao - Carregar conjunto de Dados
#
dados.grupos <- read.table("dados-grupos.csv", 
                           header=TRUE, 
                           sep=";")
attach(dados.grupos)

dados <-  cbind(dados.grupos$mes, 
                dados.grupos$quantidadeProduto,
                dados.grupos$grupoMilkShake, 
                dados.grupos$grupoSanduiche, 
                dados.grupos$grupoBebida,
                dados.grupos$grupoAcompanhamento,
                dados.grupos$grupoPrato,
                dados.grupos$grupoAdicional,
                dados.grupos$grupoBrinde,
                dados.grupos$venda)

colnames(dados) <- c("mes", 
                     "quantidadeProduto", 
                     "grupoMilkShake", 
                     "grupoSanduiche",
                     "grupoBebida",
                     "grupoAcompanhamento",
                     "grupoPrato",
                     "grupoAdicional",
                     "grupoBrinde",
                     "venda")

#
# Conjunto de Treinamento
#
data.set <<- dados[1:85, ]

data.set[,"mes"] <- as.double(data.set[,"mes"])
data.set <- scale(data.set)
data.set <<- as.data.frame(data.set)

ctrl<-trainControl(method = "cv",number = 10)


lmFit <- train(venda ~
               mes +   
               quantidadeProduto +
               grupoMilkShake + 
               grupoSanduiche + 
               grupoBebida +
               grupoAcompanhamento + 
               grupoPrato +
               grupoAdicional + 
               grupoBrinde, 
               data = data.set, 
               metric="Rsquared",
               trControl = ctrl,
               method = "lm")

v <- varImp(lmFit)

ggplot(v) 