# ------------------------------------------
# Análise de Densidade
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
library(reshape2)

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
data.set <<- dados[1:90, ]

data.set[,"mes"] <- as.double(data.set[,"mes"])
data.set <- scale(data.set)
data.set <<- as.data.frame(data.set)


#Gráficos de Densidade
fill <- "purple"
line <- "purple"

p8 <- ggplot(data.set, aes(x = grupoMilkShake)) +
  geom_density(fill = fill, colour = line,alpha = 0.3) +
  scale_x_continuous(name = "Grupo MilkShake") +
  geom_vline(xintercept = 0, size = 1, colour = "blue",linetype = "dashed") +
  scale_y_continuous(name = "Densidade") +
  ggtitle("Densidade - Grupo MilkShake ")
p8


data.set.melt <- melt(data.set)

#Diagrama de Densidade - Todas as variáveis
ggplot(data.set.melt,aes(x=value, fill=variable)) +
xlab("Valor") +
ylab("Densidade") +
ggtitle("Diagrama de Densidade") +
theme(plot.title = element_text(hjust = 0.5)) +
geom_density(alpha=0.25) +
theme(legend.title=element_blank())

#Box
ggplot(data.set.melt,aes(x=variable, y=value, fill=variable)) + geom_boxplot()