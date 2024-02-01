#install.packages("AICcmodavg")
library(AICcmodavg)
library(lme4)
library(MuMIn)
library(bbmle)

#Weight
cand.set <- list(Nollmodell_weight, model_weight1, model_weight2, model_weight3, model_weight4, model_weight5, model_weight6, model_weight7, model_weight8, model_weight9, model_weight10, model_weight11, model_weight12, model_weight13, model_weight14, model_weight15, model_weight16)
names <- c("NOLL", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16")
aictab(cand.set, modnames=names, second.ord=TRUE, nobs=NULL, sort=TRUE)

aictab(Nollmodell_weight, model_weight1, model_weight2, model_weight3, model_weight4, model_weight5, model_weight6, model_weight7)


#Residuals
cand.set <- list(Nollmodell_res, model_res1, model_res2, model_res3, model_res4,  model_res5,  model_res6, model_res7, model_res8, model_res9, model_res10, model_res11, model_res12, model_res13, model_res14, model_res15, model_res16)
names <- c("NOLL", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16")
aictab(cand.set, modnames=names, second.ord=TRUE, nobs=NULL, sort=TRUE)


#PCA
cand.set <- list(Nollmodel_pca, model_pca1, model_pca2, model_pca3, model_pca4,  model_pca5,  model_pca6, model_pca7, model_pca8, model_pca9, model_pca10, model_pca11, model_pca12, model_pca13, model_pca14, model_pca15, model_pca16)
names <- c("NOLL", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16")
aictab(cand.set, modnames=names, second.ord=TRUE, nobs=NULL, sort=TRUE)


