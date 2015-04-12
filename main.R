library("plyr")
# library("bestglm")
library("glmnet")
library("MASS")
# setwd("~/repos/cs7280-project/data/")

min.year<- 1951
max.year <- 2012
data.hurseason <- data.frame(year=1951:2012)
data.summer <- data.hurseason
data.fall <- data.hurseason
data.spring <- data.hurseason

for(f in list.files(path="data", pattern = "*.data")){
    data <- read.table(file.path("data", f), skip = 1, nrows = 66)

    tmp <- data.frame(year=data$V1)
    tmp[f] <- rowMeans(data[,c("V7", "V8", "V9", "V10", "V11", "V12")])
    data.hurseason <- join(data.hurseason, tmp, by="year")

    tmp <- data.frame(year=data$V1)
    tmp[f] <- rowMeans(data[,c("V4", "V5", "V6")])
    data.spring <- join(data.spring, tmp, by="year")

    tmp <- data.frame(year=data$V1)
    tmp[f] <- rowMeans(data[,c("V7", "V8", "V9")])
    data.summer <- join(data.summer, tmp, by="year")

    tmp <- data.frame(year=data$V1)
    tmp[f] <- rowMeans(data[,c("V10", "V11", "V12")])
    data.fall <- join(data.fall, tmp, by="year")

}

colnames(data.spring) <-
  lapply(colnames(data.spring), FUN=function(x) {gsub(pattern="data", replacement="spring",x=x)})
colnames(data.summer) <-
  lapply(colnames(data.summer), FUN=function(x) {gsub(pattern="data", replacement="summer",x=x)})
colnames(data.fall) <-
  lapply(colnames(data.fall), FUN=function(x) {gsub(pattern="data", replacement="fall",x=x)})

data.hurseason[,-1] <- scale(data.hurseason[,-1])
data.spring[,-1] <- scale(data.spring[,-1])
data.summer[,-1] <- scale(data.summer[,-1])
data.fall[,-1] <- scale(data.fall[,-1])

hurdata <- read.csv("data/hurdat.csv")
colnames(hurdata) <- replace(colnames(hurdata), list=c(1), values=c("year"))

## Histogram of Named Storms
hist(hurdata$RevisedNamedStorms, breaks=0:30)

## Pair wise plot of features
# pairs(data.summer)

## Create X and y to model on
namedstorms <- hurdata[(hurdata$year >= min.year) & (hurdata$year <= max.year),"RevisedNamedStorms"]
X <- as.matrix(data.summer[,-1])
y <- namedstorms

## Poisson regression
df = data.summer[, -1]
df$y = y
glm.full = glm(y ~ ., data=df, family=poisson)
glm.subset = step(glm.full, direction="both")

## Poisson using Lasso
fit.summer.poisson.lasso <- cv.glmnet(X, y, family="poisson")
bestlambda <- fit.summer.poisson.lasso$lambda.min
betas <- fit.summer.poisson.lasso$glmnet.fit$beta[,fit.summer.poisson.lasso$glmnet.fit$lambda == bestlambda]
fit.summer.poisson <- glm(y~X[,betas != 0] , family="poisson")
yhat <- predict(fit.summer.poisson, as.data.frame(X[,betas != 0]), type="link")

## Plot residuals against prediction
plot(residuals(fit.summer.poisson, X, type="pearson") ~ yhat)

## Plot mean and variance to check for overdispersion
### Looks to be some over dispersion
lambda <- predict(fit.summer.poisson, type="response")
plot(log(lambda), log((y -lambda)^2),
     xlab=expression(hat(lambda)),
     ylab=expression((y - hat(lambda))^2))
abline(0,1)

## Negative Binomial Using the Same Variables from Poisson Lasso
fit.negbinomial <- glm.nb(y~X[,betas != 0])
summary(fit.negbinomial)
