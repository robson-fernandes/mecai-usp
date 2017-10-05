# ------------------------------------------
# Análise Exploratória
# ------------------------------------------

#
# Limpa workspace e variaveis
#
ls()
rm(list=ls())
graphics.off()


dados.grupos <- read.table("dados-grupos.csv", 
                           header=TRUE, 
                           sep=";")

grupoMilkShake <-dados.grupos$grupoMilkShake

#Se p-value > 0.05, a distribuição é normal.
shapiro.test(grupoMilkShake)

grupoMilkShake.new <- scale(grupoMilkShake)

shapiro.test(grupoMilkShake.new)


#### qq-plot + linha de referência
#   Se os pontos se concentrarem em torno de uma linha reta,
# então temos indícios de que a distribuição é normal.
qqnorm(dados.grupos$grupoMilkShake)
qqline(dados.grupos$grupoMilkShake)

#normalizar dados
grupoMilkShake <- scale(dados.grupos$grupoMilkShake)
qqnorm(grupoMilkShake)
qqline(grupoMilkShake)

