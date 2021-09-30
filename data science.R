library(tidyverse)
library(MASS)
library(glmnet)
library(boot)
library(glmnet)
library(locfit)
library(caret)
library(corrplot)
library(glmnet)
library(dplyr)
library(ggbiplot)

DirectMarketing <- read.csv("C:/Users/awens/OneDrive - Fundacao Getulio Vargas - FGV/GV 6o Semestre/Data Siens/DataSiensGrad/DirectMarketing.csv")
base <- DirectMarketing

#tirando NA
base[is.na(base)] = "Never"

table(base$History)

#transformando em dummies

dummy <- dummyVars(" ~ .", data = base)
base_num <- data.frame(predict(dummy, newdata = base))
head(base_num)

#plots

ggplot(base, aes(x=AmountSpent)) + geom_density(aes(group=History, fill=History), alpha=.2)
ggplot(base, aes(x=AmountSpent)) + geom_density(aes(group=Age, fill=Age), alpha=.2)
ggplot(data=base, aes(x=Age, y=mean(Salary))) + geom_col() + facet_grid(vars(OwnHome), vars(Gender)) +  ylab("Avg. Salary")
par(mfrow=c(3,1))
plot(density(base$AmountSpent), main="Density-AmountSpent", xlab="Amount Spent")
plot(density(base$Salary), main="Density-Salary", xlab="Salary")
plot(AmountSpent~Salary, data=base, main="Scatterplot of AmountSpent and Salary")

#dividindo a amostra em treino e teste

set.seed(4477)
corte <- sample(seq_len(nrow(base)), size=floor(0.7*nrow(base)))
train_base_num <- base_num[corte, ]
test_base_num <- base_num[-corte, ]

# Função que retorna o R^2 e o RMSE, usando os valores preditos por um modelo
# e os dados originais
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquared = R_square
  )
  
}

y_train <- train_base_num[,19]
y_test <- test_base_num[,19]

#modelo linear - treinando e testando na base de teste

lm1 <- lm(AmountSpent~., data=train_base_num)
summary(lm1)
## Métricas LM simples
lm_train <- predict(lm1, newdata=train_base_num)
lm_test <- predict(lm1, newdata=test_base_num)
lm_train_metrics <- eval_results(y_train, lm_train, train_base_num)
lm_test_metrics <- eval_results(y_test, lm_test, test_base_num)
results.lm <- rbind(lm_train_metrics, lm_test_metrics)
rownames(results.lm) <- c('train', 'test')
results.lm

# Lambdas para usar nas regressões Ridge e Lasso
lambdas <- 10^seq(5,-5,by=-0.01)

#ridge

ridge <- cv.glmnet(x=as.matrix(scale(train_base_num[,-19])), y=train_base_num$AmountSpent, alpha=0, family= 'gaussian', lambda = lambdas)
plot(ridge)
summary(ridge)
lambda.min_ridge <- ridge$lambda.min
lambda.min_ridge
# Ridge Metrics
ridge_train <- predict(ridge, s = lambda.min_ridge, newx = as.matrix(scale(train_base_num[,-19])))
ridge_test <- predict(ridge, s = lambda.min_ridge, newx = as.matrix(scale(test_base_num[,-19])))
ridge_train_metrics <- eval_results(y_train, ridge_train, train_base_num)
ridge_test_metrics <- eval_results(y_test, ridge_test, test_base_num)
results.ridge <- rbind(ridge_train_metrics, ridge_test_metrics)
rownames(results.ridge) <- c('train', 'test')
results.ridge

#lasso 
lasso <- cv.glmnet(x=as.matrix(scale(train_base_num[,-19])), y=train_base_num$AmountSpent, alpha=0, family= 'gaussian', lambda = lambdas)
plot(lasso)
summary(lasso)
lambda.min_lasso <- lasso$lambda.min
lambda.min_lasso
# lasso Metrics
lasso_train <- predict(lasso, s = lambda.min_lasso, newx = as.matrix(scale(train_base_num[,-19])))
lasso_test <- predict(lasso, s = lambda.min_lasso, newx = as.matrix(scale(test_base_num[,-19])))
lasso_train_metrics <- eval_results(y_train, lasso_train, train_base_num)
lasso_test_metrics <- eval_results(y_test, lasso_test, test_base_num)
results.lasso <- rbind(lasso_train_metrics, lasso_test_metrics)
rownames(results.lasso) <- c('train', 'test')
results.lasso


##

#cross-validation 10 fold
controle <- trainControl(method="cv", number = 10, p = 0.9)
cv.10 <- train(AmountSpent~., data = base_num, method="lm", trControl=controle, metric = 'RMSE', maximize = T, tuneLength = 0)
## Métricas cv10
cv.10_train <- predict(cv.10, newdata=train_base_num)
cv.10_test <- predict(cv.10, newdata=test_base_num)
cv.10_train_metrics <- eval_results(y_train, cv.10_train, train_base_num)
cv.10_test_metrics <- eval_results(y_test, cv.10_test, test_base_num)
results.cv.10 <- rbind(cv.10_train_metrics, cv.10_test_metrics)
rownames(results.cv.10) <- c('train', 'test')
results.cv.10

## Resultados usando o lm simples (1 amostra de treino e 1 de teste)
results.lm 
## Resultados usando os métodos LASSO e Ridge, usando CV dentro da amostra de treino
## p/ escolher o "melhor" lambda, e então usando a amostra de teste p ver RMSE e R^2 nelas
results.lasso
results.ridge
## Usando CV p escolher um modelo dentro da amostra de treino, e depois testando ele na amostra de teste
results.cv.10

geral <- rbind(results.lm, results.ridge, results.lasso, results.cv.10)
geral$modelo <- c('lm', 'lm', 'ridge', 'ridge', 'lasso', 'lasso', 'k-fold', 'k-fold')
geral$amostra <- rep(c('train', 'test'), 4)


ggplot(data = geral, aes(x = modelo, y = RMSE, fill = amostra)) +
  geom_bar(stat = 'identity', position = 'dodge')
 
ggplot(data = geral, aes(x = modelo, y = Rsquared, fill = amostra)) +
  geom_bar(stat = 'identity', position = 'dodge')


stargazer::stargazer(geral, type = 'text', summary = F, rownames = F) #p passar pro tex mudar o 'text' p 'latex'

##Última coisa é o PCA


pca <- prcomp(train_base_num[,-c(12,19)], center = T, scale. = T)
summary(pca)
## Vou usar os 6 primeiros PC(~75% da variação) + salário num modelo linear simples na base de teste
## e dai testar esse modelo na base geral

train.pc1.6 <- as.matrix(train_base_num[,-c(12,19)])%*%as.matrix(pca$rotation[,1:6])
test.pc1.6 <- as.matrix(test_base_num[,-c(12,19)])%*%as.matrix(pca$rotation[,1:6])
## Obs: uso a mesma rotação PCA da base de treino p base de teste tb
train_pca <- cbind(train.pc1.6, train_base_num[,c(12,19)])
test_pca <- cbind(test.pc1.6, test_base_num[,c(12,19)])

pca.reg <- lm(data = train_pca, AmountSpent~.)

pca_train <- predict(pca.reg, newdata=train_pca)
pca_test <- predict(pca.reg, newdata=test_pca)
pca_train_metrics <- eval_results(y_train, pca_train, train_base_num)
pca_test_metrics <- eval_results(y_test, pca_test, test_base_num)
results.pca <- rbind(pca_train_metrics, pca_test_metrics)
rownames(results.pca) <- c('train', 'test')
results.pca


## Se quiserem usar o PCA. aqui ficam os gráficos com tudo
geral <- rbind(results.lm, results.ridge, results.lasso, results.cv.10, results.pca)
geral$modelo <- c('lm', 'lm', 'ridge', 'ridge', 'lasso', 'lasso', 'k-fold', 'k-fold', 'PCA (6)', 'PCA (6)' )
geral$amostra <- rep(c('train', 'test'), 5)


ggplot(data = geral, aes(x = modelo, y = RMSE, fill = amostra)) +
  geom_bar(stat = 'identity', position = 'dodge')
  
  ggplot(data = geral, aes(x = modelo, y = Rsquared, fill = amostra)) +
  geom_bar(stat = 'identity', position = 'dodge')
  
  
stargazer::stargazer(geral, type = 'text', summary = F, rownames = F) #p passar pro tex mudar o 'text' p 'latex'











