library(sparsebn)

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
training.setOriginal <<- dados[1:85, ]
training.set <- training.setOriginal

training.set[,"mes"] <- as.double(training.set[,"mes"])
training.set <- scale(training.set)
training.set <<- as.data.frame(training.set)

#
# Conjunto de Teste
#
test.setOriginal <<- dados[85:90, ]
test.set <- test.setOriginal

test.set[,"mes"] <- as.double(test.set[,"mes"])
test.set <- scale(test.set)
test.set <<- as.data.frame(test.set)


data <- sparsebnData(training.set, type = "continuous")
dags <- estimate.dag(data)
plotDAG(dags)