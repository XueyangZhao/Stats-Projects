# Data importing
wcommod1<-read.csv("wcommod.txt",sep=" ", skip = 1)
wcommod <- wcommod1[,1:8]
ccorp<-read.csv("ccorp.txt",sep=" ", skip = 1)
cmacro<-read.csv("cmacro.txt",sep=" ", skip = 1)
crs<-read.csv("crs.txt",sep=" ", skip = 1)
umacro<-read.csv("umacro.txt",sep=" ", skip = 1)
vix<-read.csv("vix.txt",sep=" ", skip = 1)
ir127yr <- read.table("ir127yr.txt", header = TRUE)
ir164yr <- read.table("ir164yr.txt", header = TRUE)
ir247yr <- read.table("ir247yr.txt", header = TRUE)
ir302yr <- read.table("ir302yr.txt", header = TRUE)



library(zoo)
wcommodts<-ts(wcommod,start=c(1993,1),frequency=4)
plot(zoo(wcommodts))

ccorpts<-ts(ccorp$ccorp,start=c(1996,1),frequency=4)
plot(ccorpts)

cmacrots<-ts(cmacro,start=c(1993,1),frequency=4)
plot(zoo(cmacrots))

crsts<-ts(crs$crs,start=c(1997,1),frequency=4)
plot(crsts)

umacrots<-ts(cmacro,start=c(1993,1),frequency=4)
plot(zoo(cmacrots))
umacrots<-ts(umacro,start=c(1993,1),frequency=4)
plot(zoo(umacrots))

vixts<-ts(vix$vix,start=c(1993,1),frequency=4)
plot(vixts)

igc037 <- read.csv("igc037.txt",sep = " ")
igc102 <- read.csv("igc102.txt",sep = " ")
igc239 <- read.csv("igc239.txt",sep = " ")
igc469 <- read.csv("igc469.txt",sep = " ")

# Plots of igc037 stressed
igc037 <- read.csv("igc037.txt",sep = " ")
igc037ts <- ts(igc037)
igc0371 <- igc037[c(4:10)]
plot(igc0371[1,])

par(mfrow = c(1,1))
for (i in c(105:119)){
  x <- igc0371[i,]
  plot(x,main = igc037$date[i], ylim = c(0,0.06))
}

for (i in c(100:109)){
  y<-igc0371[i, ]
  x<-c(1, 2, 3, 4, 5, 7, 10)
  plot(x,y,main=igc037$date[i],xlab = "range", ylab = "CDS")
}

for (i in c(100:114)){
  y<-ir127yr[i,c(seq(3,21,3))]
  x<-c(1, 2, 3, 4, 5, 7, 10, 20 ,30)
  plot(x,y,main=igc037$date[i],xlab = "range", ylab = "CDS")
}

matplot(igc0371[66:120,],type = c("l"),pch=1,col = 1:7,xlab = "change in interest rate of different maturaites",ylab = "interest rate")
legend("topleft", legend = 1:7, col=1:7,pch = 1,cex = 0.75,c("1yr","2yr"))



#data cleaning
wcommod1 <- wcommod[1:95,]
vix1 <- vix[1:95,]
umacro1 <- umacro[1:95,]

sub1 <- cbind(wcommod1,vix1)
sub2 <- cbind(sub1,umacro1)

# Relative World GDP
count = 1
wrgdp.percent <- rep(0,50)
for(i in (1:50)){
  wrgdp.percent[i] = (sub2$wrgdp[i+45] - sub2$wrgdp[i+45 - 16])/sub2$wrgdp[i+45-16] 
  count = count + 1
}
wrgdp.percent1 <- data.frame("wrgdpp" = wrgdp.percent)

# Relative ucprof
count = 1
ucprof.percent <- rep(0,50)
for(i in (1:50)){
  ucprof.percent[i] = (sub2$ucprof[i+45] - sub2$ucprof[i+45-16])/sub2$ucprof[i+45-16] 
  count = count + 1
}
ucprof.percent1 <- data.frame("ucprof" = ucprof.percent)
# Relative upop
count = 1
upop.percent <- rep(0,nrow(sub5)-32)
for(i in (1:50)){
  upop.percent[i] = (sub5$upop[i+29] - sub5$upop[i+29-16])/sub5$upop[i+29-16] 
  count = count + 1
}
upop.percent1 <- data.frame("upop" = upop.percent)

sub3 <- sub2[46:95,c(4:8,10,12:19,22:27)]  #for interest rate

sub4 <- cbind(sub3,ucprof.percent1)
sub5 <- cbind(sub4,upop.percent1)
full.dataset<- cbind(sub5,wrgdp.percent1) # for interest rate

start = 2005
end = 2017
total = (end - start+1)*4
one_year_mat1=rep(0, total)
ten_year_mat1=rep(0,total)
one_year_mat2=rep(0, total)
ten_year_mat2=rep(0,total)
one_year_mat3=rep(0, total)
ten_year_mat3=rep(0,total)
one_year_mat4=rep(0, total)
ten_year_mat4=rep(0,total)
count = 1

for (x in start:end){
  for(y in seq(1,10,3)){
    start_time = 10000 * x + 100 * y
    end_time = 10000 * x + 100 * (y+3)
    one_year_mat1[count] = mean(ir127yr$i365[ir127yr$date >= start_time & ir127yr$date <= end_time])
    ten_year_mat1[count] = mean(ir127yr$i3650[ir127yr$date >= start_time & ir127yr$date <= end_time])
    count = count + 1
  }
}


count=1
for (x in start:end){
  for(y in seq(1,10,3)){
    start_time = 10000 * x + 100 * y
    end_time = 10000 * x + 100 * (y+3)
    one_year_mat2[count] = mean(ir164yr$i365[ir164yr$date >= start_time & ir164yr$date <= end_time])
    ten_year_mat2[count] = mean(ir164yr$i3650[ir164yr$date >= start_time & ir164yr$date <= end_time])
    count = count + 1
  }
}

count=1
for (x in start:end){
  for(y in seq(1,10,3)){
    start_time = 10000 * x + 100 * y
    end_time = 10000 * x + 100 * (y+3)
    one_year_mat3[count] = mean(ir247yr$i365[ir247yr$date >= start_time & ir247yr$date <= end_time])
    ten_year_mat3[count] = mean(ir247yr$i3650[ir247yr$date >= start_time & ir247yr$date <= end_time])
    count = count + 1
  }
}

count=1
for (x in start:end){
  for(y in seq(1,10,3)){
    start_time = 10000 * x + 100 * y
    end_time = 10000 * x + 100 * (y+3)
    one_year_mat4[count] = mean(ir302yr$i365[ir302yr$date >= start_time & ir302yr$date <= end_time])
    ten_year_mat4[count] = mean(ir302yr$i3650[ir302yr$date >= start_time & ir302yr$date <= end_time])
    count = count + 1
  }
}
mean_ir127<-data.frame("interest rate"=ten_year_mat1-one_year_mat1)[1:50,]
mean_ir164<-data.frame("interest rate"=ten_year_mat2-one_year_mat2)[1:50,]
mean_ir247<-data.frame("interest rate"=ten_year_mat3-one_year_mat3)[1:50,]
mean_ir302<-data.frame("interest rate"=ten_year_mat4-one_year_mat4)[1:50,]

ir127.full <- cbind(mean_ir127,full.dataset)
names(ir127.full)[names(ir127.full)=="mean_ir127"] <- "interestrate"
plot(ir127.full$interestrate)

ir127.full[,c("ugdebt","walum","ugbond05","uggip","wgas","utbill","ugrev","upop")] <- list(NULL)
ir127.clean <- ir127.full
fitir127 <- lm(interestrate ~. ,data = ir127.clean)
summary(fitir127)

# Selecting features
library(corrplot)
corr<-cor(ir127.clean)
corrplot(corr,method="circle")  #删 ugdebt walum ugbond05 uggip wgas utbill ugrev upop

abs(corr)>0.8


#Random Forest
library("randomForest")
set.seed(123)
dat.rf = randomForest(interestrate ~ ., data = ir127.clean, ntree = 1000, ype='response')
dat.rf
plot(dat.rf)

k <- 5
n <- nrow(ir127.clean)
ii <- (1:n) %% k + 1
set.seed(123)
b<-predict(dat.rf, type='response')
mean((ir127.clean$interestrate - b)^2)

# Regression Tree
library(rpart)
myc <- rpart.control(minsplit=1, cp = 1e-8)
set.seed(123)
tree.to <- rpart(interestrate~ ., data = ir127.clean, method = 'class', control = myc)
plot(tree.to,uniform=T,margin=0.1)
c <- tree.to$cptable[which.min(tree.to$cptable[,"xerror"]),"CP"]
tree.t3 <- prune(tree.to, cp=c)
plot(tree.t3, uniform=FALSE)
text(tree.t3, pretty=TRUE)

# LASSO
library(glmnet)
x <- as.matrix(ir127.clean[,-11])
y <- as.vector(ir127.clean$interestrate)

set.seed(123)
fittedobject <- glmnet(x=x, y=y, alpha=0.5)
plot(fittedobject, xvar ='lambda', label = TRUE)
model = cv.glmnet(x=x,y=y, nfolds = 5, alpha=0.5)
op.la = model$lambda.min
coef(model, s=op.la)

#Step AIC
library(MASS)
null <- lm(interestrate ~ 1, dat = ir127.clean)
full <- lm(interestrate ~ ., dat = ir127.clean)
st <- stepAIC(null, scope=list(lower=null, upper=full), trace=FALSE)
# using 5-fold CV to check its MSPE
k <- 5
n <- nrow(ir127.clean)
ii <- (1:n) %% k + 1
set.seed(123)
ii <- sample(ii)
pr.st <- rep(0, n)
for(j in 1:k) {
  x0 <- ir127.clean[ii != j, ]
  null0 <- lm(interestrate ~ 1, data=ir127.clean)
  full0 <- lm(interestrate ~ ., data=ir127.clean) # needed for stepwise
  step.lm0 <- stepAIC(null0, scope=list(lower=null0, upper=full0), trace=FALSE)
  pr.st[ ii == j ] <- predict(step.lm0, newdata=ir127.clean[ii==j,])
}
mspe.st <- mean( (ir127.clean$interestrate - pr.st)^2 )
mspe.st

summary(st)



scenario <- read.csv("WorldUSData.csv")
scenario <- scenario[1:15]
predict <- predict(st,dat = scenario)
View(predict)
predict.1 <- cbind(predict[1:31],seq(41,71,1))
predict.1 <- as.data.frame(predict.1)
plot(predict.1$V2,predict.1$V1,type = "l",xlab = "time index",ylab = "interest rate",xlim = c(1,101))
lines(mean_ir127,col = "red")

predict(st,dat = real.data)
plot(predict(st,dat = real.data))
qqnorm(predict(st,dat = scenario))

## Boxplot

library(MASS)
xm <- as.matrix(ir127.clean[, -11])
n <- nrow(xm)
k <- 5
ii <- (1:n) %% k + 1
set.seed(123)
N <- 100
lambdas <- exp( seq(-3, 10, length=100))
mspe.la <- mspe.st <- mspe.f <-mspe.t<- rep(0, N)

for (i in 1:N){
  ii <- sample(ii)
  pr.la <- pr.f <- pr.st <- pr.t <- rep(0, n)
  for(j in 1:k) {
    tmp.la <- cv.glmnet(x = xm[ii != j, ], y = y[ii != j], lambda = lambdas,nfolds = 5, alpha = 1, family = "gaussian")
    null <- lm(interestrate ~ 1, data=ir127.clean[ii != j, ])
    full <- lm(interestrate ~ ., data=ir127.clean[ii != j, ])
    tmp.st <- stepAIC(null, scope=list(lower=null, upper=full), trace=0)
    tree.to <- rpart(interestrate ~ ., data=ir127.clean[ii != j, ], method = 'anova')
    
    pr.la[ ii == j ] <- predict(tmp.la, s = "lambda.min", newx = xm[ii ==j, ])
    pr.st[ ii == j ] <- predict(tmp.st, newdata=ir127.clean[ii == j, ])
    pr.f[ ii == j ] <- predict(full, newdata=ir127.clean[ii == j, ])
    pr.t[ ii == j ] <- predict(tree.to, newdata=ir127.clean[ii == j, ], type='vector')
  }
  mspe.la[i] <- mean( (ir127.clean$interestrate - pr.la)^2)
  mspe.st[i] <- mean( (ir127.clean$interestrate- pr.st)^2 )
  mspe.f[i] <- mean( (ir127.clean$interestrate - pr.f)^2 )
  mspe.t[i] <- mean( (ir127.clean$interestrate - pr.t)^2 )
}
boxplot(mspe.la, mspe.st, mspe.f,mspe.t, names = c("LASSO", "StepAIC", "Full","Tree"), col = c("steelblue", "tomato", "springgreen","black"), cex.axis = 1, cex.lab = 1, cex.main = 1)
mtext(expression(hat(MSPE)), side = 2, line = 2.5)



#CDS
igc0372 <- igc037[c(1,4:10)]

start = 2008
end = 2017
total = (end - start+1)*4
one_year_cds=rep(0, total)
ten_year_cds=rep(0,total)

count = 1
for (x in start:end){
  for(y in seq(1,10,3)){
    start_time = 10000 * x + 100 * y
    end_time = 10000 * x + 100 * (y+3)
    one_year_cds[count] = mean(igc0372$yr01[igc0372$date >= start_time & igc0372$date <= end_time])
    ten_year_cds[count] = mean(igc0372$yr10[igc0372$date >= start_time & igc0372$date <= end_time])
    count = count + 1
  }
}
one_year_cds

mean_igc037<-data.frame("cds"=ten_year_cds-one_year_cds)[2:39,]
length(mean_igc037)
igc037.new <- cbind(mean_igc037,full.dataset[13:50,c(1:20,39:41)])
names(igc037.new)[names(igc037.new)=="mean_igc037"] <- "cds"


#full model
igc037.fit <- lm(cds ~.,data = igc037.new)
summary(igc037.fit)

# check correlation
corr<-cor(igc037.new)
corrplot(corr,method="circle")

abs(corr)>0.75

#StepAIC
library(MASS)
null <- lm(cds ~ 1, dat = igc037.new)
full <- lm(cds ~ ., dat = igc037.new)
st <- stepAIC(null, scope=list(lower=null, upper=full), trace=FALSE)
# using 5-fold CV to check its MSPE
k <- 5
n <- nrow(igc037.new)
ii <- (1:n) %% k + 1
set.seed(123)
ii <- sample(ii)
pr.st <- rep(0, n)
for(j in 1:k) {
  x0 <- igc037.new[ii != j, ]
  null0 <- lm(cds ~ 1, data = igc037.new)
  full0 <- lm(cds ~ ., data = igc037.new) # needed for stepwise
  step.lm0 <- stepAIC(null0, scope=list(lower=null0, upper=full0), trace=FALSE)
  pr.st[ ii == j ] <- predict(step.lm0, newdata=igc037.new[ii==j,])
}
mspe.st <- mean( (igc037.new$cds - pr.st)^2 )
mspe.st

summary(st)

# LASSO
library(glmnet)
x <- as.matrix(igc037.new)
y <- as.vector(igc037.new$cds)

set.seed(123)
fittedobject <- glmnet(x=x, y=y, alpha=0.5)
plot(fittedobject, xvar ='lambda', label = TRUE)
model = cv.glmnet(x=x,y=y, nfolds = 5, alpha=0.5)
op.la = model$lambda.min
coef(model, s=op.la)
plot(model, xvar='lambda', label=TRUE, lwd=6, cex.axis=1.5, cex.lab=1.2)


xm <- as.matrix(scenario[, -11])

predict.glmnet(model,as.matrix(xm), s = 0.01, type = "response")

predict(st,dat = scenario)
qqnorm(predict(st,dat = scenario))

plot(predict(st,dat = scenario))


library(MASS)
xm <- as.matrix(igc037.new[, -11])
n <- nrow(xm)
k <- 5
ii <- (1:n) %% k + 1
set.seed(123)
N <- 100
lambdas <- exp( seq(-3, 10, length=100))
mspe.la <- mspe.st <- mspe.f <-mspe.t<- rep(0, N)

for (i in 1:N){
  ii <- sample(ii)
  pr.la <- pr.f <- pr.st <- pr.t <- rep(0, n)
  for(j in 1:k) {
    tmp.la <- cv.glmnet(x = xm[ii != j, ], y = y[ii != j], lambda = lambdas,nfolds = 5, alpha = 1, family = "gaussian")
    null <- lm(cds ~ 1, data=igc037.new[ii != j, ])
    full <- lm(cds ~ ., data=igc037.new[ii != j, ])
    tmp.st <- stepAIC(null, scope=list(lower=null, upper=full), trace=0)
    tree.to <- rpart(cds ~ ., data=igc037.new[ii != j, ], method = 'anova')
    
    pr.la[ ii == j ] <- predict(tmp.la, s = "lambda.min", newx = xm[ii ==j, ])
    pr.st[ ii == j ] <- predict(tmp.st, newdata=igc037.new[ii == j, ])
    pr.f[ ii == j ] <- predict(full, newdata=igc037.new[ii == j, ])
    pr.t[ ii == j ] <- predict(tree.to, newdata=igc037.new[ii == j, ], type='vector')
  }
  mspe.la[i] <- mean( (igc037.new$cds - pr.la)^2)
  mspe.st[i] <- mean( (igc037.new$cds- pr.st)^2 )
  mspe.f[i] <- mean( (igc037.new$cds - pr.f)^2 )
  mspe.t[i] <- mean( (igc037.new$cds - pr.t)^2 )
}
boxplot(mspe.la, mspe.st, mspe.f,mspe.t, names = c("LASSO", "StepAIC", "Full","Tree"), col = c("steelblue", "tomato", "springgreen","black"), cex.axis = 1, cex.lab = 1, cex.main = 1)
mtext(expression(hat(MSPE)), side = 2, line = 2.5)

####### Restart CDS
sub11 <- sub2[59:95,c(4:8,10,12:19,22:27)]

start = 2008
end = 2017
total = (end - start+1)*4
one_year_cds=rep(0, total)
ten_year_cds=rep(0,total)

count = 1
for (x in start:end){
  for(y in seq(1,10,3)){
    start_time = 10000 * x + 100 * y
    end_time = 10000 * x + 100 * (y+3)
    one_year_cds[count] = mean(igc037$yr01[igc037$date >= start_time & igc037$date <= end_time])
    ten_year_cds[count] = mean(igc037$yr10[igc037$date >= start_time & igc037$date <= end_time])
    count = count + 1
  }
}
one_year_cds

mean_igc037<-data.frame("cds"=ten_year_cds-one_year_cds)[2:38,]
length(mean_igc037)
full.dataset.cds <- full.dataset[14:50,]
full.dataset.cds[,c("walum","vix","ugbond05","ungdp","ugbal","uggi","uhpi","uhst","ucrepi","ugdebt","ugrev")] <- list(NULL)
igc037.new <- cbind(mean_igc037,full.dataset.cds)
names(igc037.new)[names(igc037.new)=="mean_igc037"] <- "cds"
  
corr<-cor(full.dataset.cds)
corrplot(corr,method="circle") #删walum vix ugbond05 ungdp ugbal uggi


##AIC
library(MASS)
null <- lm(cds ~ 1, dat = igc037.new)
full <- lm(cds ~ ., dat = igc037.new)
st <- stepAIC(null, scope=list(lower=null, upper=full), trace=FALSE)
# using 5-fold CV to check its MSPE
k <- 5
n <- nrow(igc037.new)
ii <- (1:n) %% k + 1
set.seed(123)
ii <- sample(ii)
pr.st <- rep(0, n)
for(j in 1:k) {
  x0 <- igc037.new[ii != j, ]
  null0 <- lm(cds ~ 1, data = igc037.new)
  full0 <- lm(cds ~ ., data = igc037.new) # needed for stepwise
  step.lm0 <- stepAIC(null0, scope=list(lower=null0, upper=full0), trace=FALSE)
  pr.st[ ii == j ] <- predict(step.lm0, newdata=igc037.new[ii==j,])
}
mspe.st <- mean( (igc037.new$cds - pr.st)^2 )
mspe.st

summary(st)

## LASSO
library(glmnet)
x <- as.matrix(igc037.new)
y <- as.vector(igc037.new$cds)

set.seed(123)
fittedobject <- glmnet(x=x, y=y, alpha=0.5)
plot(fittedobject, xvar ='lambda', label = TRUE)
model = cv.glmnet(x=x,y=y, nfolds = 5, alpha=0.5)
op.la = model$lambda.min
coef(model, s=op.la)
plot(model, xvar='lambda', label=TRUE, lwd=6, cex.axis=1.5, cex.lab=1.2)


xm <- as.matrix(scenario)

predict.glmnet(model,as.matrix(xm))

predict(st,dat = scenario)
qqnorm(predict(st,dat = scenario))

plot(predict(st,dat = scenario))


library(MASS)
xm <- as.matrix(igc037.new[, -11])
n <- nrow(xm)
k <- 5
ii <- (1:n) %% k + 1
set.seed(123)
N <- 100
lambdas <- exp( seq(-3, 10, length=100))
mspe.la <- mspe.st <- mspe.f <-mspe.t<- rep(0, N)

for (i in 1:N){
  ii <- sample(ii)
  pr.la <- pr.f <- pr.st <- pr.t <- rep(0, n)
  for(j in 1:k) {
    tmp.la <- cv.glmnet(x = xm[ii != j, ], y = y[ii != j], lambda = lambdas,nfolds = 5, alpha = 1, family = "gaussian")
    null <- lm(cds ~ 1, data=igc037.new[ii != j, ])
    full <- lm(cds ~ ., data=igc037.new[ii != j, ])
    tmp.st <- stepAIC(null, scope=list(lower=null, upper=full), trace=0)
    tree.to <- rpart(cds ~ ., data=igc037.new[ii != j, ], method = 'anova')
    
    pr.la[ ii == j ] <- predict(tmp.la, s = "lambda.min", newx = xm[ii ==j, ])
    pr.st[ ii == j ] <- predict(tmp.st, newdata=igc037.new[ii == j, ])
    pr.f[ ii == j ] <- predict(full, newdata=igc037.new[ii == j, ])
    pr.t[ ii == j ] <- predict(tree.to, newdata=igc037.new[ii == j, ], type='vector')
  }
  mspe.la[i] <- mean( (igc037.new$cds - pr.la)^2)
  mspe.st[i] <- mean( (igc037.new$cds- pr.st)^2 )
  mspe.f[i] <- mean( (igc037.new$cds - pr.f)^2 )
  mspe.t[i] <- mean( (igc037.new$cds - pr.t)^2 )
}
boxplot(mspe.la, mspe.st, mspe.f,mspe.t, names = c("LASSO", "StepAIC", "Full","Tree"), col = c("steelblue", "tomato", "springgreen","black"), cex.axis = 1, cex.lab = 1, cex.main = 1)
mtext(expression(hat(MSPE)), side = 2, line = 2.5)


predict <- predict(st,dat = scenario)
predict.1 <- cbind(predict[1:31],seq(41,71,1))
predict.1 <- as.data.frame(predict.1)
plot(predict.1$V2,predict.1$V1,type = "l",xlab = "time index",ylab = "cds",xlim = c(1,101),ylim = c(0,0.04))
lines(mean_ir127,col = "red")
legend("topleft",legend = c("real data","scenario"),col=1:2,lty=1:2,cex=0.5)
