#--------------------------- Exemplo do Livro ----------------------------------#

# Data frame 
df2 <- data.frame(
  cultivares = c(1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8, 
                 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8),
  locais = rep(1:6, each = 8),
  medias = c(1992, 2132, 1794, 1986, 1944, 2047, 1910, 1921, 1599, 416, 634, 676, 629, 
             1263, 1109, 711, 4313, 3313, 3597, 3590, 3826, 3508, 3219, 3257, 6082, 4082,
             3860, 3869, 3989, 6124, 6889, 3813, 1461, 932, 558, 671, 522, 1551, 1339, 744, 
             2108, 285, 289, 238, 274, 2194, 2267, 246))

# tabela de dupla entrada 
df3=xtabs(medias ~ cultivares + locais, data=df2); df3

meanL=colMeans(df3); meanL    # média por local (Y_.j)
meanG=rowMeans(df3); meanG    # média por genótipo (Y_i.)
media.geral=mean(df3); media.geral  # média geral (Y_..)

# Transformando os dados
df3=transform(df2, cultivares=as.factor(cultivares), locais=as.factor(locais))
str(df3)

# Análise de variância
modelo_anova <- aov(medias ~ locais * cultivares, data=df3)
# aov(medias ~ locais + cultivares + cultivares:locais, data=df3)
summary(modelo_anova)

# Índices ambientais
I.amb=meanL - media.geral; I.amb   # Índices de cada ambiente

# a soma dos índices ajustados é praticamente zero
sum(I.amb) 

# nova tebela de dados para a regressão
df4=subset(df3, select=c(cultivares, locais, medias)) 
df4$I.amb=I.amb[df3$locais]; df4

# Ajustar o modelo de regressão linear 
M2 <- lm(medias ~ I.amb, data=df4)
summary(M2)  

coef(M2)      # Extrai os coeficientes 
predict(M2)   # Extrai as previsões do modelo
residuals(M2)  # Extrai os resíduos 

#--------------------------------------------------------------------------#
# obter os coeficientes de regressão para cada cultivar individualmente.

# uma lista para armazenar os modelos de regressão
list_M <- list()

# Loop sobre cada cultivar
for (i in unique(df4$cultivares)) {
  # subconjunto do dataframe para a cultivar atual
  dados_cultivar <- subset(df4, cultivares == i)
  # ajustar o modelo de regressão linear para a cultivar atual
  list_M[[paste("Cultivar", i)]] <- lm(medias ~ I.amb, data=dados_cultivar)
}

# Calcular R² para cada modelo de regressão linear ajustado para cada cultivar 
R2 <- numeric(length(list_M))
for (i in seq_along(list_M)) {
  R2[i] <- summary(list_M[[i]])$r.squared
}

# visualizar os coeficientes e R² para cada cultivar
for (i in seq_along(list_M)) {
  cat("Cultivar", i, ":\n")
  print(coef(list_M[[i]]))
  cat("R²:", R2[i], "\n\n")
}

#----------------- Outro jeito de fazer a mesma coisa! ------------------------#

# ajustar modelos de regressão linear para cada cultivar
modelos <- by(df4, df3$cultivares, function(x) lm(medias ~ I.amb, data=x))

# visualizar os coeficientes para cada cultivar
sapply(modelos, coef)

# Calcular R² para cada modelo de regressão linear ajustado para cada cultivar 
R2 <- sapply(modelos, function(model) summary(model)$r.squared); R2
