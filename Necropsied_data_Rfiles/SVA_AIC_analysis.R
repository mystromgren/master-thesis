#install.packages("AICcmodavg")
library(AICcmodavg)
library(lme4)
library(MuMIn)
library(bbmle)

#Weight 
cand.set <- list(Nollmodell_weightSVA, model_weight1SVA, model_weight2SVA, model_weight3SVA, model_weight4SVA, model_weight5SVA, model_weight6SVA, model_weight7SVA, model_weight8SVA, model_weight9SVA, model_weight10SVA, model_weight11SVA, model_weight12SVA, model_weight13SVA, model_weight14SVA, model_weight15SVA, model_weight16SVA)
names <- c("NOLL", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16")
aictab(cand.set, modnames=names, second.ord=TRUE, nobs=NULL, sort=TRUE)


#Residuals
cand.set <- list(Nollmodell_resSVA, model_res1SVA, model_res2SVA, model_res3SVA, model_res4SVA,  model_res5SVA,  model_res6SVA, model_res7SVA, model_res8SVA, model_res9SVA, model_res10SVA, model_res11SVA, model_res12SVA, model_res13SVA, model_res14SVA, model_res15SVA, model_res16SVA)
names <- c("NOLL", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16")
aictab(cand.set, modnames=names, second.ord=TRUE, nobs=NULL, sort=TRUE)


#PCA
cand.set <- list(Nollmodel_pcaSVA, model_pca1SVA, model_pca2SVA, model_pca3SVA, model_pca4SVA,  model_pca5SVA,  model_pca6SVA, model_pca7SVA, model_pca8SVA, model_pca9SVA, model_pca10SVA, model_pca11SVA, model_pca12SVA, model_pca13SVA, model_pca14SVA, model_pca15SVA, model_pca16SVA)
names <- c("NOLL", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16")
aictab(cand.set, modnames=names, second.ord=TRUE, nobs=NULL, sort=TRUE)
