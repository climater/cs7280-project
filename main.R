library("glmnet")
library("MASS")
# rm(list=ls())

min.year = 1951
max.year = 2013

clim.jun2nov = data.frame(row.names=as.character(min.year:max.year))
clim.mar2may = data.frame(row.names=as.character(min.year:max.year))
clim.jun2aug = data.frame(row.names=as.character(min.year:max.year))

for (f in list.files(path="data", pattern="*.data")) {
  #   print(f)
  clim = read.table(file.path("data", f), skip=1, row.names=1, nrows=66)
  colnames(clim) = month.abb
  clim.jun2nov[, f] = scale(rowMeans(clim[as.character(min.year:max.year), 6:11]))
  clim.mar2may[, f] = scale(rowMeans(clim[as.character(min.year:max.year), 3:5]))
  clim.jun2aug[, f] = scale(rowMeans(clim[as.character(min.year:max.year), 6:8]))
}

hurdat = read.csv("data/hurdat.csv", row.names=1)
hur.count = hurdat[as.character(min.year:max.year), "RevisedHurricanes"]
# hist(hur.count)

## Poisson regression with best subset stepwise selection
df = cbind(hur.count, clim.jun2nov)
glm.full = glm(hur.count ~ ., data=df, family=poisson)
summary(glm.full)
glm.subset = step(glm.full, direction="both")
summary(glm.subset)
anova(glm.full, glm.subset)

summary(glm(formula(glm.subset), data=df, family=quasipoisson))

## Poisson regression with lasso variable selection
x = model.matrix(hur.count ~ ., data=df)[, -1]
y = df$hur.count
cv.out = cv.glmnet(x, y, family="poisson", nfolds=5)
bestlam = cv.out$lambda.min
fit.lasso = glmnet(x, y, family="poisson", alpha=1, lambda=bestlam)
