library(tidyverse)
library(MASS)
library(glmnet)
library(boot)
library(glmnet)
library(locfit)
library(caret)
library(corrplot)
library(glmnet)

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
par(mfrow=c(2,1))
plot(density(base$AmountSpent), main="Density-AmountSpent", xlab="Amount Spent")
plot(density(base$Salary), main="Density-Salary", xlab="Salary")
plot(AmountSpent~Salary, data=base, main="Scatterplot of AmountSpent and Salary")

#dividindo a amostra em treino e teste

set.seed(4477)
corte <- sample(seq_len(nrow(base)), size=floor(0.7*nrow(base)))
train_base <- base[corte, ]
test_base <- base[-corte, ]

train_base_num <- base_num[corte, ]
test_base_num <- base_num[-corte, ]

#modelo linear

lm1 <- lm(AmountSpent~., data=train_base)
summary(lm1)

#RMSE base de teste

test_base_prev <- test_base %>% mutate(amount_spent_test_preds = predict(lm1, test_base[, c(1:9)]))
test_errors <- test_base_prev$amount_spent_test_preds - test_base$AmountSpent
sqrt(mean(test_errors^2))

#RMSE base treino
train_base_prev <- train_base %>% mutate(amount_spent_train_preds = predict(lm1, train_base[, c(1:9)]))
train_errors <- train_base_prev$amount_spent_train_preds - train_base$AmountSpent
sqrt(mean(train_errors^2))


#plot dos residuos

residuo <- test_base_prev$amount_spent_test_preds - test_base$AmountSpent
plot(test_base$AmountSpent, residuo)


#cross-validation

controle <- trainControl(method="cv", number = 10)
lm2 <- train(x=base[,-10], y=base[,10], method="lm", trControl=controle)
lm2

#ridge

ridge <- cv.glmnet(x=as.matrix(scale(train_base_num[,-10])), y=train_base_num$AmountSpent, alpha=0)
plot(ridge)

lambda.min_ridge <- ridge$lambda.min
lambda.min_ridge

ridge_prev <- predict(ridge, s = lambda.min_ridge, newx = as.matrix(scale(test_base_num[,-10])))
RMSE_ridge <- sqrt(mean((ridge_prev - test_base_num[,10])^2))
RMSE_ridge

#lasso 
##OBS: nessa parte ele usa o s = lambda.min_ridge nos predicts e fiquei em duvida se usamos o lambda.min_ridge ou o lambda.min_lasso (o lasso é muito menor) 

lambda <- 10^seq(10,-2,length=1000)
lasso <- cv.glmnet(x=as.matrix(scale(train_base_num[,-10])), y=train_base_num$AmountSpent, alpha=1, lambda=lambda)
plot(lasso)

lambda.min_lasso <- lasso$lambda.min
lambda.min_lasso

lasso_prev <- predict(lasso, s = lambda.min_ridge, newx = as.matrix(scale(test_base_num[,-10])))
RMSE_lasso <- sqrt(mean((lasso_prev - test_base_num[,10])^2))
RMSE_lassolasso_prev <- predict(lasso, s = lambda.min_ridge, newx = as.matrix(scale(test_base_num[,-10])))
RMSE_lasso <- sqrt(mean((lasso_prev - test_base_num[,10])^2))
RMSE_lasso
