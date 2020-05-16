library(glmnet)
library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)
library(randomForest)
library(coefplot)
library(gridExtra)

#install.packages("randomForestExplainer")
#library(randomForestExplainer)
# Read the file
blog = read.csv("/Users/jasonjklim/Desktop/Baruch/STA 9890/Project/BlogFeedback/blogData_train.csv", header = TRUE)

n  =  dim(blog)[1]
p  =  dim(blog)[2]-1
y  = blog[,p]
X = data.matrix(blog[,-p]) 
# does not scale the variable 
# because some of variables are statistical variables.



n.train        =     floor(0.8*n)
n.test         =     n-n.train

# Lambda
lam.las = c(seq(1e-3,0.1,length=100),seq(0.12,2.5,length=100)) 
lam.rid = lam.las*1000




M              =     100

Rsq.test.en    =     rep(0,M)  #en = elastic net
Rsq.train.en   =     rep(0,M)
Rsq.test.ridge    =     rep(0,M)  # Ridge
Rsq.train.ridge   =     rep(0,M)
Rsq.test.lasso    =     rep(0,M)  # Lasso
Rsq.train.lasso   =     rep(0,M)
Rsq.test.rf    =     rep(0,M)  # rf= randomForest
Rsq.train.rf   =     rep(0,M)


for (m in c(1:M)) {
  shuffled_indexes =     sample(n)
  train            =     shuffled_indexes[1:n.train]
  test             =     shuffled_indexes[(1+n.train):n]
  X.train          =     X[train, ]
  y.train          =     y[train]
  X.test           =     X[test, ]
  y.test           =     y[test]
  
  # fit elastic-net and calculate and record the train and test R squares 
  a=0.5 # elastic-net
  cv_en.fit           =     cv.glmnet(X.train, y.train, alpha = a, nfolds = 10)
  en_fit              =     glmnet(X.train, y.train, alpha = a, lambda = cv_en.fit$lambda.min)
  y.train.hat_en      =     predict(en_fit, newx = X.train, type = "response") # y.train.hat=X.train %*% fit$beta + fit$a0
  y.test.hat_en       =     predict(en_fit, newx = X.test, type = "response") # y.test.hat=X.test %*% fit$beta  + fit$a0
  Rsq.test.en[m]   =     1-mean((y.test - y.test.hat_en)^2)/mean((y - mean(y))^2)
  Rsq.train.en[m]  =     1-mean((y.train - y.train.hat_en)^2)/mean((y - mean(y))^2)  
  
  
  b=0 # Ridge
  cv_ridge.fit           =     cv.glmnet(X.train, y.train, alpha = b, lambda = lam.rid, nfolds = 10)
  optimal_lambda <- cv_ridge.fit$lambda.min
  ridge_fit              =     glmnet(X.train, y.train, alpha = b, lambda = optimal_lambda)
  y.train.hat_ridge      =     predict(ridge_fit, newx = X.train, type = "response") # y.train.hat=X.train %*% fit$beta + fit$a0
  y.test.hat_ridge      =     predict(ridge_fit, newx = X.test, type = "response") # y.test.hat=X.test %*% fit$beta  + fit$a0
  Rsq.test.ridge[m]   =     1-mean((y.test - y.test.hat_ridge)^2)/mean((y - mean(y))^2)
  Rsq.train.ridge[m]  =     1-mean((y.train - y.train.hat_ridge)^2)/mean((y - mean(y))^2)  
  
  c=1 # Lasso
  cv_lasso.fit           =     cv.glmnet(X.train, y.train, alpha = c, lambda = lam.las, nfolds = 10)
  optimal_lambda2 <- cv_lasso.fit$lambda.min
  lasso_fit              =     glmnet(X.train, y.train, alpha = c, lambda = optimal_lambda2)
  y.train.hat_lasso      =     predict(lasso_fit, newx = X.train, type = "response") # y.train.hat=X.train %*% fit$beta + fit$a0
  y.test.hat_lasso      =     predict(lasso_fit, newx = X.test, type = "response") # y.test.hat=X.test %*% fit$beta  + fit$a0
  Rsq.test.lasso[m]   =     1-mean((y.test - y.test.hat_lasso)^2)/mean((y - mean(y))^2)
  Rsq.train.lasso[m]  =     1-mean((y.train - y.train.hat_lasso)^2)/mean((y - mean(y))^2)  
  
  # fit RF and calculate and record the train and test R squares 
  rf               =     randomForest(X.train, y.train, mtry = sqrt(p), importance = TRUE, ntree = 100)
  y.test.hat_rf      =     predict(rf, X.test)
  y.train.hat_rf     =     predict(rf, X.train)
  Rsq.test.rf[m]   =     1-mean((y.test - y.test.hat_rf)^2)/mean((y - mean(y))^2)
  Rsq.train.rf[m]  =     1-mean((y.train - y.train.hat_rf)^2)/mean((y - mean(y))^2)  
  
  
  
  cat(sprintf("m=%3.f| ,  Rsq.test.en=%.2f|,  Rsq.train.en=%.2f|, 
              Rsq.test.ridge=%.2f|,  Rsq.train.ridge=%.2f|,
              Rsq.test.lasso=%.2f|,  Rsq.train.lasso=%.2f|,
              Rsq.test.rf=%.2f|,  Rsq.train.rf=%.2f|\n", 
              m, Rsq.test.en[m], Rsq.train.en[m],
              Rsq.test.ridge[m], Rsq.train.ridge[m],
              Rsq.test.lasso[m], Rsq.train.lasso[m],
              Rsq.test.rf[m], Rsq.train.rf[m]
              ))
  
}
# (b) R^2 boxplot


# For side boxplot for R^2
# Train
boxplot(Rsq.train.rf, Rsq.train.en, Rsq.train.lasso, Rsq.train.ridge,
        main = "R^2 in Train Set",
        names = c("RF", "EN", "LASSO", "RIDGE"),
        col = c("orange","red", "blue", "yellow"))


text(1, 0.80, paste("Avg:",round(mean(Rsq.train.rf),4)))
text(2, 0.90, paste("Avg:",round(mean(Rsq.train.en),4)))
text(3, 0.90, paste("Avg:",round(mean(Rsq.train.lasso),4)))
text(4, 0.90, paste("Avg:",round(mean(Rsq.train.ridge),4)))



# Test
boxplot(Rsq.test.rf, Rsq.test.en, Rsq.test.lasso, Rsq.test.ridge,
        main = "R^2 in Test Set",
        names = c("RF", "EN", "LASSO", "RIDGE"),
        col = c("orange","red", "blue", "yellow"))


text(1, 0, paste("Avg:",round(mean(Rsq.test.rf),4)))
text(2, 0, paste("Avg:",round(mean(Rsq.test.en),4)))
text(3, 0, paste("Avg:",round(mean(Rsq.test.lasso),4)))
text(4, 0, paste("Avg:",round(mean(Rsq.test.ridge),4)))


# (C) cv curves

# boxplot for  10-fold CV cruves EN
plot(cv_en.fit , 
     main="10-fold CV curves for Elastic Net\n\n")

# boxplot for  10-fold CV cruves Ridge
plot(cv_ridge.fit , 
     main="10-fold CV curves for Ridge\n\n")

# boxplot for  10-fold CV cruves Lasso
plot(cv_lasso.fit, 
     main="10-fold CV curves for Lasso\n\n", ylim=c(50, 500))



  
  
  


# (d) side by side residual boxplots
# Creating Residual variables
train_res_en = y.train-y.train.hat_en #EN
test_res_en = y.test-y.test.hat_en
train_res_ridge = y.train-y.train.hat_ridge #Ridge
test_res_ridge = y.test-y.test.hat_ridge
train_res_lasso = y.train-y.train.hat_lasso #Lasso
test_res_lasso = y.test-y.test.hat_lasso
train_res_rf = y.train-y.train.hat_rf # RF
test_res_rf = y.test-y.test.hat_rf

# TRAIN
boxplot(train_res_rf, train_res_en, train_res_lasso, train_res_ridge,
        main = "Residuals in Train Set",
        names = c("RF", "EN", "LASSO", "RIDGE"))

text(1, 650, paste("Avg:",round(mean(train_res_rf),4)))
text(2, 650, paste("Avg:",round(mean(train_res_en),4)))
text(3, 650, paste("Avg:",round(mean(train_res_lasso),4)))
text(4, 650, paste("Avg:",round(mean(train_res_ridge),4)))

# TEST
boxplot(test_res_rf, test_res_en, test_res_lasso, test_res_ridge,
        main = "Residuals in Test Set",
        names = c("RF", "EN", "LASSO", "RIDGE"))

text(1, -200, paste("Avg:",round(mean(test_res_rf),4)))
text(2, -200, paste("Avg:",round(mean(test_res_en),4)))
text(3, -200, paste("Avg:",round(mean(test_res_lasso),4)))
text(4, -200, paste("Avg:",round(mean(test_res_ridge),4)))







# Bootstrapped
bootstrapSamples =     100
beta.rf.bs       =     matrix(0, nrow = p, ncol = bootstrapSamples)    
beta.en.bs       =     matrix(0, nrow = p, ncol = bootstrapSamples)         
beta.ridge.bs    =     matrix(0, nrow = p, ncol = bootstrapSamples) 
beta.lasso.bs    =     matrix(0, nrow = p, ncol = bootstrapSamples) 

for (m in 1:bootstrapSamples){
  bs_indexes       =     sample(n, replace=T)
  X.bs             =     X[bs_indexes, ]
  y.bs             =     y[bs_indexes]
  
  # fit bs rf
  rf               =     randomForest(X.bs, y.bs, mtry = sqrt(p), importance = TRUE, ntree=100)
  beta.rf.bs[,m]   =     as.vector(rf$importance[,1])
  # fit bs en
  a                =     0.5 # elastic-net
  cv.fit           =     cv.glmnet(X.bs, y.bs, alpha = a, nfolds = 10)
  fit              =     glmnet(X.bs, y.bs, alpha = a, lambda = cv.fit$lambda.min)  
  beta.en.bs[,m]   =     as.vector(fit$beta)
  
  # fit bs lasso
  a                =     1 # lasso
  cv.fit           =     cv.glmnet(X.bs, y.bs, alpha = a, nfolds = 10)
  fit              =     glmnet(X.bs, y.bs, alpha = a, lambda = cv.fit$lambda.min)  
  beta.lasso.bs[,m]   =     as.vector(fit$beta)
  
  # fit bs ridge
  a                =     0 # Ridge
  cv.fit           =     cv.glmnet(X.bs, y.bs, alpha = a, nfolds = 10)
  fit              =     glmnet(X.bs, y.bs, alpha = a, lambda = cv.fit$lambda.min)  
  beta.ridge.bs[,m]   =     as.vector(fit$beta)
  
  
  
  
  cat(sprintf("Bootstrap Sample %3.f \n", m))
}


# calculate bootstrapped standard errors / alternatively you could use qunatiles to find upper and lower bounds
rf.bs.sd    = apply(beta.rf.bs, 1, "sd")
en.bs.sd    = apply(beta.en.bs, 1, "sd")
lasso.bs.sd = apply(beta.lasso.bs, 1, "sd")
ridge.bs.sd = apply(beta.ridge.bs, 1, "sd")


# fit rf to the whole data
rf               =     randomForest(X, y, mtry = sqrt(p), importance = TRUE, ntree=100)

# fit en to the whole data
a=0.5 # elastic-net
cv_en.fit           =     cv.glmnet(X, y, alpha = a, nfolds = 10)
enfit              =     glmnet(X, y, alpha = a, lambda = cv_en.fit$lambda.min)

# fit lasso to the whole data
a                =     1 # lasso
cv_lasso.fit           =     cv.glmnet(X, y, alpha = a, nfolds = 10)
lassofit              =     glmnet(X, y, alpha = a, lambda = cv_lasso.fit$lambda.min)  

# fit Ridge to the whole data
a                =     0 # Ridge
cv_ridge.fit           =     cv.glmnet(X, y, alpha = a, nfolds = 10)
ridgefit              =     glmnet(X, y, alpha = a, lambda = cv_ridge.fit$lambda.min)




betaS.rf               =     data.frame(names(X[1,]), as.vector(rf$importance[,1]), 2*rf.bs.sd)
colnames(betaS.rf)     =     c( "feature", "value", "err")

betaS.en               =     data.frame(names(X[1,]), as.vector(enfit$beta), 2*en.bs.sd)
colnames(betaS.en)     =     c( "feature", "value", "err")


betaS.lasso               =     data.frame(names(X[1,]), as.vector(lassofit$beta), 2*lasso.bs.sd)
colnames(betaS.lasso)     =     c( "feature", "value", "err")

betaS.ridge               =     data.frame(names(X[1,]), as.vector(ridgefit$beta), 2*ridge.bs.sd)
colnames(betaS.ridge)     =     c( "feature", "value", "err")


# Order them by value and pick top 5 
# Maybe dplyr

betarf_sample = head(betaS.rf %>% arrange(desc(value)), n=5)
betaen_sample =head(betaS.en %>% arrange(desc(value)), n=5)
betalasso_sample =head(betaS.lasso %>% arrange(desc(value)), n=5)
betaridge_sample =head(betaS.ridge %>% arrange(desc(value)), n=5)


betarf_sample$feature     =  factor(betarf_sample$feature, levels = betarf_sample$feature[order(betarf_sample$value, decreasing = TRUE)])
betaen_sample$feature     =  factor(betaen_sample$feature, levels = betaen_sample$feature[order(betaen_sample$value, decreasing = TRUE)])
betalasso_sample$feature     =  factor(betalasso_sample$feature, levels = betalasso_sample$feature[order(betalasso_sample$value, decreasing = TRUE)])
betaridge_sample$feature     =  factor(betaridge_sample$feature, levels = betaridge_sample$feature[order(betaridge_sample$value, decreasing = TRUE)])


rfPlot1 =  ggplot(betarf_sample, aes(x=feature, y=value)) +
  geom_bar(stat = "identity", fill="white", colour="black")    +
  geom_errorbar(aes(ymin=value-err, ymax=value+err), width=.2) +
  ggtitle('Random Forest')+theme(plot.title = element_text(hjust = 0.5))

enPlot1 =  ggplot(betaen_sample, aes(x=feature, y=value)) +
  geom_bar(stat = "identity", fill="white", colour="black")    +
  geom_errorbar(aes(ymin=value-err, ymax=value+err), width=.2) +
  ggtitle('Elastic Net')+theme(plot.title = element_text(hjust = 0.5))


grid.arrange(rfPlot1, enPlot1, nrow = 2,
             top = "RF vs EN Important Parameter")                    

lassoPlot1 =  ggplot(betalasso_sample, aes(x=feature, y=value)) +
  geom_bar(stat = "identity", fill="white", colour="black")    +
  geom_errorbar(aes(ymin=value-err, ymax=value+err), width=.2) +
  ggtitle('Lasso')+theme(plot.title = element_text(hjust = 0.5))

ridgePlot1 =  ggplot(betaridge_sample, aes(x=feature, y=value)) +
  geom_bar(stat = "identity", fill="white", colour="black")    +
  geom_errorbar(aes(ymin=value-err, ymax=value+err), width=.2) +
  ggtitle('Ridge')+theme(plot.title = element_text(hjust = 0.5))

grid.arrange(lassoPlot1, ridgePlot1, nrow = 2,
             top = "Lasso vs Ridge Important Parameter")
