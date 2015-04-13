library("glmnet")
library("MASS")
# rm(list=ls())

min.year = 1951
max.year = 2013

clim.jun2nov = data.frame(row.names=as.character(min.year:max.year))
# clim.mar2may = data.frame(row.names=as.character(min.year:max.year))
# clim.jun2aug = data.frame(row.names=as.character(min.year:max.year))

## Load climate index data as predictors
for (f in list.files(path="data", pattern="*.data")) {
  #   print(f)
  clim = read.table(file.path("data", f), skip=1, row.names=1, nrows=66)
  colnames(clim) = month.abb
  clim.jun2nov[, f] = scale(rowMeans(clim[as.character(min.year:max.year), 6:11]))
#   clim.mar2may[, f] = scale(rowMeans(clim[as.character(min.year:max.year), 3:5]))
#   clim.jun2aug[, f] = scale(rowMeans(clim[as.character(min.year:max.year), 6:8]))
}

## Load hurricane count data as the dependent variable
hurdat = read.csv("data/hurdat.csv", row.names=1)
hur.count = hurdat[as.character(min.year:max.year), "RevisedHurricanes"]
# hist(hur.count)

## Poisson regression with best subset stepwise selection
df = cbind(hur.count, clim.jun2nov)
glm.full = glm(hur.count ~ ., data=df, family=poisson)
summary(glm.full)
glm.subset = step(glm.full, direction="both")
summary(glm.subset)
# anova(glm.full, glm.subset)

## Check over/under-dispersion
summary(glm(formula(glm.subset), data=df, family=quasipoisson))

## Goodness of fit test (between the fitted model and saturated model)
p.value = pchisq(glm.subset$deviance, glm.subset$df.residual, lower.tail=FALSE)
print(paste("Goodness of fit test, p-value =", p.value)) # Large p-value indicates good fit.

## Likelihood ratio test (between the fitted model and null model)
p.value = pchisq(glm.subset$null.deviance-glm.subset$deviance,
                 glm.subset$df.null-glm.subset$df.residual, lower.tail=FALSE)
print(paste("Likelihood ratio test, p-value =", p.value)) # Small p-value indicates not all betas are zero.

## Different residuals
# resid(glm.subset, type="response") # Response residuals (of limited use)
# resid(glm.subset, type="pearson") # Pearson residuals
# resid(glm.subset) # Deviance residuals

## Residual plots
par(mfrow=c(2, 2))
plot(resid(glm.subset, type="response") ~ predict(glm.subset, type="response"),
     xlab=expression(hat(lambda)), ylab="Response residuals")
abline(h=0)
plot(resid(glm.subset, type="response") ~ predict(glm.subset, type="link"),
     xlab=expression(paste(hat(eta), " = X", hat(beta))), ylab="Response residuals")
abline(h=0)
plot(resid(glm.subset) ~ predict(glm.subset, type="link"),
     xlab=expression(paste(hat(eta), " = X", hat(beta))), ylab="Deviance residuals")
abline(h=0)
plot(resid(glm.subset, type="pearson") ~ predict(glm.subset, type="link"),
     xlab=expression(paste(hat(eta), " = X", hat(beta))), ylab="Pearson residuals")
abline(h=0)

## Poisson regression with lasso variable selection
# x = model.matrix(hur.count ~ ., data=df)[, -1]
# y = df$hur.count
# cv.out = cv.glmnet(x, y, family="poisson", nfolds=5)
# bestlam = cv.out$lambda.min
# fit.lasso = glmnet(x, y, family="poisson", alpha=1, lambda=bestlam)
# glm.lasso = glm(y ~ x[, as.vector(fit.lasso$beta!=0)], family=poisson)
