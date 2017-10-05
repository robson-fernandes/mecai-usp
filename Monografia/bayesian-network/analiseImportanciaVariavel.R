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
dados.grupos <- read.table("dados-new.csv", 
                           header=TRUE, 
                           sep=";")
attach(dados.grupos)

dados <-  cbind(dados.grupos$mes, 
                dados.grupos$venda, 
                dados.grupos$grupoCarne,
                dados.grupos$grupoUniforme, 
                dados.grupos$grupoEmbalagem, 
                dados.grupos$grupoUsoConsumo,
                dados.grupos$grupoHortiFrutti,
                dados.grupos$grupoPaes,
                dados.grupos$grupoBebidas,
                dados.grupos$grupoSobremesas,
                dados.grupos$grupoMolhos,
                dados.grupos$grupoFrios,
                dados.grupos$grupoSecos
)

colnames(dados) <- c("mes", 
                     "venda", 
                     "grupoCarne",
                     "grupoUniforme", 
                     "grupoEmbalagem",
                     "grupoUsoConsumo",
                     "grupoHortiFrutti",
                     "grupoPaes",
                     "grupoBebidas",
                     "grupoSobremesas",
                     "grupoMolhos",
                     "grupoFrios",
                     "grupoSecos"
)

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
                 grupoBebidas +
                 grupoCarne  +
                 grupoUniforme +
                 grupoHortiFrutti +
                 grupoPaes + 
                 grupoSobremesas +
                 grupoMolhos +
                 grupoFrios,
               data = data.set, 
               metric="Rsquared",
               trControl = ctrl,
               method = "lm")

v <- varImp(lmFit)
v
ggplot(v) +
  xlab("Variável") +
  ylab("Importância") +
  ggtitle("Análise de Variáveis Importantes") + 
  geom_density(alpha=0.25) +
  geom_bar(stat="identity", fill="#9e8cbe", colour="#9e8cbe") +
  theme(plot.title = element_text(hjust = 0.5)) 