#---------------------------------------------------------------------------------------
# Hermes LUIZ BOLINELLI JUNIOR
#---------------------------------------------------------------------------------------
library(arules)

# obtem dados treinamento "train" (housePricing_trainSet.csv)  e "test" (housePricing_valSet.csv)
#
train <- read.csv("C:/Users/Hermes/Documents/housePricing_trainSet.csv")
head(train);tail(train)
#test <- read.csv("C:/Users/Hermes/Documents/housePricing_valSet.csv")
test <- read.csv("C:/Users/Hermes/Documents/housePricing_testSet.csv")
head(test);tail(test)


# Estrutura/Analise
#
str(train)
summary(train)
cor(train[,-10])
#
# dataframe com 12384 regsitros e 10 variaveis
# todos numericos com exceção de "ocean_proximity": Factor com 5 nivel:
#  <1H OCEAN     INLAND     ISLAND   NEAR BAY NEAR OCEAN 
#       5411       3927          3       1409       1634
# Problema 1: Há um problema de micronumerosidade aqui: ISLAND tem apenas 3 registros, as demais passam de mil.
# o modelo pode não ser adequado para imóves INLAND. Seá matida e depois avaliado nos dados.
#
# Problema 2: Temos 5411 registos <H OCEAN... não sei o que quer dizer.
# valores esperados são imóveis mais caros proximos (frente) ao Oceano. Mas isso é para Brasil. 
# COmo os dados são dos EUA vamos verificar se esse comportamento também ocorre.
#
# Variavel "total_bedrooms" tem NA´s: serão eliminados pois são de pouco ocorrencia (ver abaixo).
#
# Variavel "median_house_value": valores minimos (15k) e maximos(500k) são de mercados diferentes.... 
# .... em relação aos imóveis da mediana, 1ºQ e 2ºQ (120k a 265k).  Vou manter.
#-----------------------------------------------------------------------------------------------------------
# GRAFICOS
#library(car)
#scatterplotMatrix(train, spread = FALSE, smoother.args = list(lty = 2),
#                  main = "Scatter Plot Matrix")

#
#----------------------------------------------------------------------------------------------------------

# TRATAMENTO: -------------------------------------------------------------------------------------------
# 
nrow(train)              # Há 12384 regsitros originais
nrow(test)               # Há 4128 regsitros originais 
train <- na.omit(train)  # Elimino registros com NA´s
test <- na.omit(test)
nrow(train)              # Foram eliminados 116 registros (0,9367% do total) devido a NA´s, resultando em 12268 registros
nrow(test)               # Foram eliminados  44 regsitros (1,0658% do total) devido a NA´s, resultados em  4084 regsitros

# apenas para manter o original em "train"
#
t <- train               

# Correlações: cor(t[,-10])
#                           median_house_value
#longitude                 -0.04373933
#latitude                  -0.14552851 <=====2
#housing_median_age         0.11199024 <===4
#total_rooms                0.12821500 <====3
#total_bedrooms             0.04504947
#population                -0.03093698
#households                 0.06111810
#median_income              0.68981613  <===== 1
#median_house_value         1.00000000

# Normalização --------------------------------------------------------------------------------------------------
#
# Media/sd -------------------------------------------------------------------------------------
mlongitude <- mean(t$longitude)
mlatitude <- mean(t$latitude)
mhousing_median_age <- mean(t$housing_median_age)
mtotal_rooms <- mean(t$total_rooms)
mtotal_bedrooms <- mean(t$total_bedrooms)
mpopulation <- mean(t$population)
mhouseholds <- mean(t$households)
mmedian_income <- mean(t$median_income)

sdlongitude <- sd(t$longitude)
sdlatitude <- sd(t$latitude)
sdhousing_median_age <- sd(t$housing_median_age)
sdtotal_rooms <- sd(t$total_rooms)
sdtotal_bedrooms <- sd(t$total_bedrooms)
sdpopulation <- sd(t$population)
sdhouseholds <- sd(t$households)
sdmedian_income <- sd(t$median_income)
#-------------------------------------------------------------------------------------
#treino
t$longitude <- (t$longitude - mlongitude) / sdlongitude
t$latitude <- (t$latitude -  mlatitude) / sdlatitude
t$housing_median_age <- (t$housing_median_age - mhousing_median_age) / sdhousing_median_age
t$total_rooms <- (t$total_rooms - mtotal_rooms ) / sdtotal_rooms
t$total_bedrooms <- (t$total_bedrooms - mtotal_bedrooms ) / sdtotal_bedrooms
t$population <- (t$population - mpopulation ) / sdpopulation
t$households <- (t$households - mhouseholds) / sdhouseholds
t$median_income <- (t$median_income - mmedian_income) / sdmedian_income

# teste
test$longitude <- (test$longitude - mlongitude) / sdlongitude
test$latitude <- (test$latitude - mlatitude) / sdlatitude
test$housing_median_age <- (test$housing_median_age - mhousing_median_age) / sdhousing_median_age
test$total_rooms <- (test$total_rooms - mtotal_rooms) / sdtotal_rooms
test$total_bedrooms <- (test$total_bedrooms - mtotal_bedrooms) / sdtotal_bedrooms
test$population <- (test$population - mpopulation) / sdpopulation
test$households <- (test$households - mhouseholds) / sdhouseholds
test$median_income <- (test$median_income - mmedian_income) / sdmedian_income
#


# MODELO -------------------------------------------------------------------------------------------------------------
# 01 : todas as features
#
ylm <- lm(formula =median_house_value ~ longitude + latitude + housing_median_age + total_rooms + total_bedrooms
          + population + households + median_income + ocean_proximity, data = t)

Preditor <- predict (ylm, test )
MAE <- sum(abs(Preditor - test$median_house_value) / length(Preditor))
MAE
# MAE 01  = 49551.51

coefficients(ylm)
summary(ylm)
# Todas as variaveis são significativamente diferentes de zero ao nivel de 0.1% ('***' 0.001)
# Exceção: variavel ocean_proximity possui 2 situações diferentes:
# NEAR_BAY: significativamente diferente de zero a 32,84 % 
# NEAR_OCEAN: significativamente diferente de zero a 2% (1.36%) ('*' 0.05)
# R2 = 64.90% / 64.87% : poder de explicação das features para o valor
#
# vou eliminar no modelo 2 population por ser fortemente correlacinada com households, dormitorios, salas
# e ter a menor correlação com a variavel independente

#res <- resid(y)
#vlr <- fitted(y)
#res;vlr
#plot(res,vlr)
#
#
# predicao
#
#
# MODELO -------------------------------------------------------------------------------------------------------------
# 02 : Eliminei population 
#
ylm <- lm(formula =median_house_value ~ longitude + latitude + housing_median_age + total_rooms + total_bedrooms
          + households + median_income + ocean_proximity, data = t)   #population
ylm

Preditor <- predict (ylm, test )
MAE <- sum(abs(Preditor - test$median_house_value) / length(Preditor))
MAE
# MAE 02  = 51428.09

coefficients(ylm)
summary(ylm)

# signifcancia das features permanecem significativamente diferentes de zero para 0.1% ('***' 0.001)
# R2 = 62,97% / 62.94%: abaixou um pouco (era 64.90. è esperado pois tirei uma feature.)
# vou eliminar todas as features com correlação com a dependnete menorque 7% (Modelo 3)

# MODELO -------------------------------------------------------------------------------------------------------------
# 03 : Elimino: longitude / bedroons / households / population
#
ylm <- lm(formula =median_house_value ~ latitude + housing_median_age + total_rooms
          + median_income + ocean_proximity, data = t)   #  longitude + total_bedrooms + households + population
ylm

Preditor <- predict (ylm, test )
MAE <- sum(abs(Preditor - test$median_house_value) / length(Preditor))
MAE
# MAE 03  = 53477.14

coefficients(ylm)
summary(ylm)

# R2 = 60.51% / 60.48%
# Latitude está significativamenete diferente de zero a 9% (houve uma piora)
# Vamos trabalhar uma e duas mais significativas apenas: median_income (68.98%) e Latitude (-11.19%) .. ou total_rooms (12.82%)



# MODELO -------------------------------------------------------------------------------------------------------------
# 04 : Apenas com median_income (maior correlação com variavel dependente = 68.98%)
#
ylm <- lm(formula = median_house_value ~ median_income, data = t) 
ylm

Preditor <- predict (ylm, test )
MAE <- sum(abs(Preditor - test$median_house_value) / length(Preditor))
MAE
# MAE 04  = 62881.73

coefficients(ylm)
summary(ylm)

# R2 = 47.58% / 47.58%
# siginicancia menor 0.1%


# MODELO -------------------------------------------------------------------------------------------------------------
# 05 : Apenas com median_income e total_rooms
#
ylm <- lm(formula = median_house_value ~ median_income + total_rooms, data = t) 
ylm

Preditor <- predict (ylm, test )
MAE <- sum(abs(Preditor - test$median_house_value) / length(Preditor))
MAE
# MAE 05  = 62881.03

coefficients(ylm)
summary(ylm)

# R2 = 47.59% / 47.58%
# siginicancia menor 0.1% median_income e menr que 3% para total_rooms


# MODELO -------------------------------------------------------------------------------------------------------------
# 06 : Apenas com median_income e total_rooms
#
ylm <- lm(formula = median_house_value ~ median_income + latitude, data = t) 
ylm

Preditor <- predict (ylm, test )
MAE <- sum(abs(Preditor - test$median_house_value) / length(Preditor))
MAE
# MAE 06  = 62475.43

coefficients(ylm)
summary(ylm)

# R2 = 48.36% / 48.35%
# siginicancia menor 0.1% latitude e median_income


# MODELO -------------------------------------------------------------------------------------------------------------
# 07 : Apenas com median_income e total_rooms
#
ylm <- lm(formula = median_house_value ~ median_income + latitude + total_rooms, data = t) 
ylm

Preditor <- predict (ylm, test )
MAE <- sum(abs(Preditor - test$median_house_value) / length(Preditor))
MAE
# MAE 07  = 62487.57

coefficients(ylm)
summary(ylm)

# R2 = 48.37% / 48.36%
# siginicancia menor 0.1% latitude e median_income e menor que 12% para total_rooms


# vamos tentar combinar algumas features:.............................................................................
#
# MODELO -------------------------------------------------------------------------------------------------------------
# 08 : Não tenho "um norte", uma estratégia,... vou tentar...
#
ylm <- lm(formula =median_house_value ~ ((longitude *latitude)^2) + housing_median_age + total_rooms + total_bedrooms
          + population + households + median_income + ocean_proximity, data = t)
ylm

Preditor <- predict (ylm, test )
MAE <- sum(abs(Preditor - test$median_house_value) / length(Preditor))
MAE
# MAE 08  = 49302.74

coefficients(ylm)
summary(ylm)

# R2 = 65.20% / 65,17%
# siginicancia menor 0.1% lp/ Features
# ..........
#

# MODELO -------------------------------------------------------------------------------------------------------------
# 09 : Não tenho "um norte", uma estratégia,... vou tentar...
#
ylm <- lm(formula =median_house_value ~ ((longitude *latitude)^2) + housing_median_age + total_rooms + 
            total_bedrooms + median_income + ocean_proximity, data = t) 
ylm

Preditor <- predict (ylm, test )
MAE <- sum(abs(Preditor - test$median_house_value) / length(Preditor))
MAE
# MAE 09  = 51481.49

coefficients(ylm)
summary(ylm)

# R2 = 62.98% / 62.94%
# siginicancia menor 0.1% p/ features
# ..........


# MODELO -------------------------------------------------------------------------------------------------------------------------
# 10 : Não tenho "um norte", uma estratégia,... vou tentar...
#
ylm <- lm(formula =median_house_value ~ ((longitude *latitude)^2) + housing_median_age + total_rooms + total_bedrooms
          + households + median_income + ocean_proximity, data = t)
ylm

Preditor <- predict (ylm, test )
MAE <- sum(abs(Preditor - test$median_house_value) / length(Preditor))
MAE
# MAE 10  = 51245.84

coefficients(ylm)
summary(ylm)

# R2 = 63.21% / 63.17%
# siginicancia menor 0.1% latitude e median_income e menor que 12% para total_rooms
# .................................................................................................................................
#----------------------------------------------------------------------------------------------------------------------------------


# FIM
#----------------------------------------------------------------------------------------------------------------------------------



# RESPOSTA:------------------------------------------------------------------------------------------------------------------------
# MELHOR MODELO 08: MAE = 49302.74
# ylm <- lm(formula =median_house_value ~ ((longitude *latitude)^2) + housing_median_age + total_rooms + total_bedrooms
#            +           + population + households + median_income + ocean_proximity, data = t)
#----------------------------------------------------------------------------------------------------------------------------------
# MODELO -------------------------------------------------------------------------------------------------------------
# 08 :
#
ylm <- lm(formula =median_house_value ~ ((longitude *latitude)^2) + housing_median_age + total_rooms + total_bedrooms
          + population + households + median_income + ocean_proximity, data = t)
ylm

Preditor <- predict (ylm, test )
MAE <- sum(abs(Preditor - test$median_house_value) / length(Preditor))
MAE
# MAE 08  = 49302.74

coefficients(ylm)
summary(ylm)

# R2 = 65.20% / 65,17%
# siginicancia menor 0.1% lp/ Features
# ..........
#
#======================================================================================

