rm(list = ls())
cat("\14")

getwd()
setwd("C:/Users/matth/OneDrive/Desktop/Data Mining/project")
for (pkg in c("gridExtra","hexbin", "tidyverse", "modelr", "lubridate", "glmnet", "leaps", "car","randomForest", "dplyr", "reshape2", "caret")) library(pkg, character.only=TRUE)

## access the data

set.seed(4444)
agg_data <- read.csv("agg_data.csv")
names(agg_data)[1] <- "zipcode"

#look at the correlation matrix

X = model.matrix(inc_rate ~ .+0, agg_data)
cormat <- round(cor(X),2) 
melted_cormat <- melt(cormat)
dim(X)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() + theme(axis.text.x = element_blank()) 

# normalizing the data set

data <- agg_data %>%
  select(-inc_rate, -zipcode, -total_pop, -incarerated_pop) %>%
  mutate_all(.funs = function(x) {x / sqrt(mean((x-mean(x))^2))}) %>%
  mutate(inc_rate = sqrt(agg_data$inc_rate)) %>%  ## square root transformation because of left skew
  glimpse()


'For each ntrain = 0:8n, repeat the following 100 times, plot the box-plots of the errors
of the different models mentioned below.'

'(a) Randomly split the dataset into two mutually exclusive datasets'
'(b) Use Dlearn to fit lasso, elastic-net alpha = 0:5, ridge, and random forrests.'
'(c) Tune the lambdas using 10-fold CV'
'(d) for each model calculate the Rsq test'

t   =   100
y   =   data$inc_rate
X   =   model.matrix(inc_rate ~ .+0, data)
n   =   dim(data)[1]
p   =   dim(data)[2]-1

rsqts_las <- 1:t
rsqtr_las <- 1:t
rsqts_el  <- 1:t
rsqtr_el  <- 1:t
rsqts_rid <- 1:t
rsqtr_rid <- 1:t
rsqtr_rf  <- 1:t
rsqts_rf  <- 1:t

for(i in 1:t) {
  
  part   <- createDataPartition(data$inc_rate, list = FALSE, p = .8)
  datatr <- data[part, ]
  datats <- data[-part, ]
  
  Xtr <- model.matrix(inc_rate ~ .+0,datatr)
  Xts <- model.matrix(inc_rate ~ .+0,datats)
  ytr <- datatr$inc_rate
  yts <- datats$inc_rate
  
  # LASSO
  cv.fit   =     cv.glmnet(Xtr, ytr, alpha = 1, nfolds = 10)
  fit      =     glmnet(Xtr, ytr, alpha = 1, lambda = cv.fit$lambda.min) 
  
  y.train.hat  =     predict(fit, newx = Xtr, type = "response") 
  y.test.hat   =     predict(fit, newx = Xts, type = "response") 
  
  rsqts_las[i] =     1-mean((yts - y.test.hat)^2)/mean((y - mean(y))^2)
  rsqtr_las[i] =     1-mean((ytr - y.train.hat)^2)/mean((y - mean(y))^2)  
  
  # RIDGE
  cv.fit       =     cv.glmnet(Xtr, ytr, alpha = 0, nfolds = 10)
  fit          =     glmnet(Xtr, ytr, alpha = 0, lambda = cv.fit$lambda.min)

  y.train.hat  =     predict(fit, newx = Xtr, type = "response")
  y.test.hat   =     predict(fit, newx = Xts, type = "response")
  
  rsqts_rid[i] =     1-mean((yts - y.test.hat)^2)/mean((y - mean(y))^2)
  rsqtr_rid[i] =     1-mean((ytr - y.train.hat)^2)/mean((y - mean(y))^2)

  # ELASTIC NET
  cv.fit       =     cv.glmnet(Xtr, ytr, alpha = 0.5, family = "gaussian")
  fit          =     glmnet(Xtr, ytr, alpha = 0.5, lambda = cv.fit$lambda.min)

  y.train.hat  =     predict(fit, newx = Xtr, type = "response")
  y.test.hat   =     predict(fit, newx = Xts, type = "response")

  rsqts_el[i]  =     1-mean((yts - y.test.hat)^2)/mean((y - mean(y))^2)
  rsqtr_el[i]  =     1-mean((ytr - y.train.hat)^2)/mean((y - mean(y))^2)

  # RANDOM FORREST
  rf           =     rf <-  randomForest(Xtr, ytr, mtry = sqrt(p), importance = TRUE)

  y.train.hat  =     predict(rf, Xtr)
  y.test.hat   =     predict(rf, Xts)

  rsqts_rf[i]  =     1-mean((yts - y.test.hat)^2)/mean((y - mean(y))^2)
  rsqtr_rf[i]  =     1-mean((ytr - y.train.hat)^2)/mean((y - mean(y))^2)

  cat(sprintf("t=%3.f| rsqts_rf=%.2f,  rsqts_el=%.2f, rsqts_rid=%.2f, rsqts_las=%.2f| rsqtr_rf=%.2f,  rsqtr_el=%.2f, rsqtr_rid=%.2f, rsqtr_las=%.2f| \n", i,  rsqts_rf[i], rsqts_el[i],  rsqts_rid[i], rsqts_las[i], rsqtr_rf[i], rsqtr_el[i], rsqtr_rid[i], rsqtr_las[i]))
}


## format the R squared for analysis

tr_las <- as.data.frame(rsqtr_las)
ts_las <- as.data.frame(rsqts_las)
tr_el  <- as.data.frame(rsqtr_el)
ts_el  <- as.data.frame(rsqts_el)
tr_rid <- as.data.frame(rsqtr_rid)
ts_rid <- as.data.frame(rsqts_rid)
tr_rf  <- as.data.frame(rsqtr_rf)
ts_rf  <- as.data.frame(rsqts_rf)

lasso_r <- cbind(tr_las,ts_las)
mm_las = melt(lasso_r, measure.vars = c('rsqtr_las','rsqts_las'))

elnet_r <- cbind(tr_el,ts_el)
mm_en = melt(elnet_r, measure.vars = c('rsqtr_el','rsqts_el'))

ridge_r <- cbind(tr_rid,ts_rid)
mm_rid = melt(ridge_r, measure.vars = c('rsqtr_rid','rsqts_rid'))

rf_r <- cbind(tr_rf,ts_rf)
mm_rf = melt(rf_r, measure.vars = c('rsqtr_rf','rsqts_rf'))

## create boxplots for each model

las_box = ggplot(mm_las, aes(x=paste(variable), y=value)) + 
  ggtitle("Lasso R-Squared") + 
  theme_minimal() +
  geom_boxplot(outlier.colour="red", lwd = 1) +
  scale_x_discrete(labels = c('Training','Test')) +
  theme(plot.title = element_text(hjust = 0.5, size=35, face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16));
las_box

en_box = ggplot(mm_en, aes(x=paste(variable), y=value)) + 
  ggtitle("Elastic Net R-Squared") + 
  theme_minimal() +
  geom_boxplot(outlier.colour="red", lwd = 1) +
  scale_x_discrete(labels = c('Training','Test')) +
  theme(plot.title = element_text(hjust = 0.5, size=35, face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16));
en_box

rid_box = ggplot(mm_rid, aes(x=paste(variable), y=value)) + 
  ggtitle("Ridge R-Squared") + 
  theme_minimal() +
  geom_boxplot(outlier.colour="red", lwd = 1) +
  scale_x_discrete(labels = c('Training','Test')) +
  theme(plot.title = element_text(hjust = 0.5, size=35, face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16));
rid_box

rf_box = ggplot(mm_rf, aes(x=paste(variable), y=value)) + 
  ggtitle("Random Forrest R-Squared") + 
  theme_minimal() +
  geom_boxplot(outlier.colour="red", lwd = 1) +
  scale_x_discrete(labels = c('Training','Test')) +
  theme(plot.title = element_text(hjust = 0.5, size=35, face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16));

rf_box

## create bar-plots (with bootstrapped error bars) of the estimated coefficients, and the importance of the parameters

bootstrapSamples  =     100
beta.rf.bs        =     matrix(0, nrow = p, ncol = bootstrapSamples)    
beta.en.bs        =     matrix(0, nrow = p, ncol = bootstrapSamples)         
beta.las.bs       =     matrix(0, nrow = p, ncol = bootstrapSamples)         
beta.rid.bs       =     matrix(0, nrow = p, ncol = bootstrapSamples)         

for (m in 1:bootstrapSamples){
  bs_indexes       =     sample(n, replace=T)
  X.bs             =     X[bs_indexes, ]
  y.bs             =     y[bs_indexes]
  
  
  # fit bs las
  cv.fit.las       =     cv.glmnet(X.bs, y.bs, alpha = 1, nfolds = 10)
  fit.las          =     glmnet(X.bs, y.bs, alpha = 1, lambda = cv.fit$lambda.min)  
  beta.las.bs[,m]  =     as.vector(fit.las$beta)

  # fit bs rid
  cv.fit.rid       =     cv.glmnet(X.bs, y.bs, alpha = 0, nfolds = 10)
  fit.rid          =     glmnet(X.bs, y.bs, alpha = 0, lambda = cv.fit$lambda.min)  
  beta.rid.bs[,m]  =     as.vector(fit.rid$beta)

  # fit bs en
  cv.fit.en        =     cv.glmnet(X.bs, y.bs, alpha = 0.5, nfolds = 10)
  fit.en           =     glmnet(X.bs, y.bs, alpha = 0.5, lambda = cv.fit$lambda.min)  
  beta.en.bs[,m]   =     as.vector(fit.en$beta)

    # fit bs rf
  rf               =     randomForest(X.bs, y.bs, mtry = sqrt(p), importance = TRUE)
  beta.rf.bs[,m]   =     as.vector(rf$importance[,1])
  
   cat(sprintf("Bootstrap Sample %3.f \n", m))
}

# calculate bootstrapped standard errors

rf.bs.sd    = apply(beta.rf.bs, 1, "sd")
en.bs.sd    = apply(beta.en.bs, 1, "sd")
las.bs.sd   = apply(beta.las.bs, 1, "sd")
rid.bs.sd   = apply(beta.rid.bs, 1, "sd")


# fit las to the whole data
cv.fit.las       =     cv.glmnet(X, y, alpha = 1, nfolds = 10)
fit.las          =     glmnet(X, y, alpha = 1, lambda = cv.fit.las$lambda.min)

# fit rid to the whole data
cv.fit.rid       =     cv.glmnet(X, y, alpha = 0, nfolds = 10)
fit.rid          =     glmnet(X, y, alpha = 0, lambda = cv.fit.rid$lambda.min)

# fit en to the whole data
cv.fit.en       =     cv.glmnet(X, y, alpha = 0.5, nfolds = 10)
fit.en          =     glmnet(X, y, alpha = 0.5, lambda = cv.fit.en$lambda.min)

# fit rf to the whole data
rf               =     randomForest(X, y, mtry = sqrt(p), importance = TRUE)



betaS.las              =     data.frame(names(X[1,]), as.vector(fit.las$beta), 2*las.bs.sd)
colnames(betaS.las)    =     c( "feature", "las_value", "las_err")

betaS.rid              =     data.frame(names(X[1,]), as.vector(fit.rid$beta), 2*rid.bs.sd)
colnames(betaS.rid)    =     c( "feature", "rid_value", "rid_err")

betaS.en               =     data.frame(names(X[1,]), as.vector(fit.en$beta), 2*en.bs.sd)
colnames(betaS.en)     =     c( "feature", "en_value", "en_err")

betaS.rf               =     data.frame(names(X[1,]), as.vector(rf$importance[,1]), 2*rf.bs.sd)
colnames(betaS.rf)     =     c( "feature", "rf_value", "rf_err")

# order the factors

betaS.rf$feature     =  factor(betaS.rf$feature, levels = betaS.rf$feature[order(betaS.rf$rf_value, decreasing = TRUE)])
betaS.en$feature     =  factor(betaS.en$feature, levels = betaS.rf$feature[order(betaS.en$en_value, decreasing = TRUE)])
betaS.las$feature    =  factor(betaS.las$feature, levels = betaS.rf$feature[order(betaS.las$las_value, decreasing = TRUE)])
betaS.rid$feature    =  factor(betaS.rid$feature, levels = betaS.rf$feature[order(betaS.rid$rid_value, decreasing = TRUE)])

# construct beta data set for analysis

rid_tibble <- as_tibble(betaS.rid)
las_tibble <- as_tibble(betaS.las)
en_tibble  <- as_tibble(betaS.en)
rf_tibble  <- as_tibble(betaS.rf)

join <- las_tibble %>% 
  inner_join(rid_tibble) %>%  inner_join(en_tibble) %>%  inner_join(rf_tibble) 


# plot the betas

lasPlot =  ggplot(betaS.las, aes(x=feature, y=las_value)) +
  ggtitle("Lasso Betas") +
  geom_bar(stat = "identity", fill="gray90", colour="black")    +
  geom_errorbar(aes(ymin=las_value-las_err, ymax=las_value+las_err), width=.2, size = .75) +
  coord_cartesian(ylim = c(-15, 20)) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'gray'),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "grey10"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank());

ridPlot =  ggplot(betaS.rid, aes(x=feature, y=rid_value)) +
  ggtitle("Ridge Betas") +
  geom_bar(stat = "identity", fill="gray90", colour="black")    +
  geom_errorbar(aes(ymin=rid_value-rid_err, ymax=rid_value+rid_err), width=.2, size = .75) +
  coord_cartesian(ylim = c(-10, 20)) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'gray'),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "grey10"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank());

enPlot =  ggplot(betaS.en, aes(x=feature, y=en_value)) +
  ggtitle("Elastic Net Betas") +
  geom_bar(stat = "identity", fill="gray90", colour="black")    +
  geom_errorbar(aes(ymin=en_value-en_err, ymax=en_value+en_err), width=.2, size = .75)+
  coord_cartesian(ylim = c(-10, 20)) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'gray'),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "grey10"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank());

rfPlot =  ggplot(betaS.rf, aes(x=feature, y=rf_value)) +
  ggtitle("Random Forrest Betas") +
  geom_bar(stat = "identity", fill="gray90", colour="black")    +
  geom_errorbar(aes(ymin=rf_value-rf_err, ymax=rf_value+rf_err), width=.2, size = .75)+
  theme(panel.background = element_rect(fill = 'white', colour = 'gray'),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "grey10"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank());

grid.arrange(rfPlot, enPlot, lasPlot, ridPlot, nrow = 4)

## Residual Box Plots / CV fit Plot for one example of each model

# LASSO
cv.fit   =     cv.glmnet(Xtr, ytr, alpha = 1, nfolds = 10)
fit      =     glmnet(Xtr, ytr, alpha = 1, lambda = cv.fit$lambda.min) 
plot(cv.fit, main = "Lasso Plot")

y.train.hat  =     predict(fit, newx = Xtr, type = "response") 
y.test.hat   =     predict(fit, newx = Xts, type = "response") 

las.train.residuals = (ytr - y.train.hat)^2
las.test.residuals = (yts - y.test.hat)^2

tr_las_rss  <- as.data.frame(las.train.residuals)
ts_las_rss  <- as.data.frame(las.test.residuals)

ts_las_rss$set = c('test')
tr_las_rss$set = c('train')
mm_las_rss <- rbind(tr_las_rss,ts_las_rss)
colnames(mm_las_rss)

rss_las_box = ggplot(mm_las_rss, aes(x=paste(set), y=s0)) + 
  ggtitle("Lasso Residuals") + 
  theme_minimal() +
  geom_boxplot(outlier.colour="red", lwd = 1) +
  scale_x_discrete(labels = c('Training','Test')) +
  coord_cartesian(ylim = c(0, 1000)) +
  theme(plot.title = element_text(hjust = 0.5, size=35, face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16))

rss_las_box

# RIDGE
cv.fit       =     cv.glmnet(Xtr, ytr, alpha = 0, nfolds = 10)
fit          =     glmnet(Xtr, ytr, alpha = 0, lambda = cv.fit$lambda.min)
plot(cv.fit, main = "Ridge Plot")

y.train.hat  =     predict(fit, newx = Xtr, type = "response")
y.test.hat   =     predict(fit, newx = Xts, type = "response")

rid.train.residuals = (ytr - y.train.hat)^2
rid.test.residuals = (yts - y.test.hat)^2

tr_rid_rss  <- as.data.frame(rid.train.residuals)
ts_rid_rss  <- as.data.frame(rid.test.residuals)

ts_rid_rss$set = c('test')
tr_rid_rss$set = c('train')
mm_rid_rss <- rbind(tr_rid_rss,ts_rid_rss)

rss_rid_box = ggplot(mm_rid_rss, aes(x=paste(set), y=s0)) + 
  ggtitle("Ridge Residuals") + 
  theme_minimal() +
  geom_boxplot(outlier.colour="red", lwd = 1) +
  scale_x_discrete(labels = c('Training','Test')) +
  coord_cartesian(ylim = c(0, 1000)) +
  theme(plot.title = element_text(hjust = 0.5, size=35, face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16))

rss_rid_box

# ELASTIC NET
cv.fit       =     cv.glmnet(Xtr, ytr, alpha = 0.5, family = "gaussian")
fit          =     glmnet(Xtr, ytr, alpha = 0.5, lambda = cv.fit$lambda.min)
plot(cv.fit, main = "Elastic Net Plot")

y.train.hat  =     predict(fit, newx = Xtr, type = "response")
y.test.hat   =     predict(fit, newx = Xts, type = "response")

en.train.residuals = (ytr - y.train.hat)^2
en.test.residuals = (yts - y.test.hat)^2

tr_en_rss  <- as.data.frame(en.train.residuals)
ts_en_rss  <- as.data.frame(en.test.residuals)

ts_en_rss$set = c('test')
tr_en_rss$set = c('train')
mm_en_rss <- rbind(tr_en_rss,ts_rid_rss)

rss_en_box = ggplot(mm_rid_rss, aes(x=paste(set), y=s0)) + 
  ggtitle("Elastic Net Residuals") + 
  theme_minimal() +
  geom_boxplot(outlier.colour="red", lwd = 1) +
  scale_x_discrete(labels = c('Training','Test')) +
  coord_cartesian(ylim = c(0, 1000)) +
  theme(plot.title = element_text(hjust = 0.5, size=35, face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16))

rss_en_box

# RANDOM FORREST
rf           =     rf <-  randomForest(Xtr, ytr, mtry = sqrt(p), importance = TRUE)

y.train.hat  =     predict(rf, Xtr)
y.test.hat   =     predict(rf, Xts)

rs.train.residuals = (ytr - y.train.hat)^2
rs.test.residuals = (yts - y.test.hat)^2

tr_rs_rss  <- as.data.frame(rs.train.residuals)
ts_rs_rss  <- as.data.frame(rs.test.residuals)

ts_rs_rss$set = c('test')
tr_rs_rss$set = c('train')
mm_rs_rss <- rbind(tr_en_rss,ts_rid_rss)

rss_rs_box = ggplot(mm_rid_rss, aes(x=paste(set), y=s0)) + 
  ggtitle("Random Forrest Residuals") + 
  theme_minimal() +
  geom_boxplot(outlier.colour="red", lwd = 1) +
  scale_x_discrete(labels = c('Training','Test')) +
  coord_cartesian(ylim = c(0, 1000)) +
  theme(plot.title = element_text(hjust = 0.5, size=35, face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16))

rss_rs_box