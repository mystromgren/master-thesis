wolves <- read.csv("/Users/mystromgren/Desktop/Master thesis/SVA_omarbetad2.csv", header=TRUE, sep=";")
sum(is.na(wolves$Totallängd..Riks..mm))
table(wolves$Skabb)

#merging length variables and correcting them
wolves$Totallängd..Riks..mm <- ifelse(is.na(wolves$Totallängd..Riks..mm), wolves$Totallängd..mm., wolves$Totallängd..Riks..mm)
sum(is.na(wolves$Totallängd..Riks..mm ))

wolves$Svans.anus.mm <- ifelse(is.na(wolves$Svans.anus.mm), wolves$Svans..anus..mm., wolves$Svans.anus.mm)
sum(is.na(wolves$Svans.anus.mm ))

wolves$bodylength <- wolves$Totallängd..Riks..mm - wolves$Svans.anus.mm

wolves$bodylength <- wolves$bodylength / 10

wolves$bodylength <- ifelse(is.na(wolves$bodylength), wolves$Kroppslängd..kontur...cm., wolves$bodylength)
sum(is.na(wolves$bodylength))

wolves$bodylength <- ifelse(is.na(wolves$bodylength), wolves$Kroppsl.kontur.cm, wolves$bodylength)
sum(is.na(wolves$bodylength))



#Getting rid of NA individuals in body_length
SVA_residuals <- wolves[!is.na(wolves$bodylength),]
sum(is.na(SVA_residuals$Levandevikt))
SVA_residuals11 <-SVA_residuals[!is.na(SVA_residuals$Levandevikt),]


nrow(SVA_residuals1) #370 data points when NAs are removed


#creating model for residuals
modelres1 <- lm(SVA_residuals1$Levandevikt ~ SVA_residuals1$bodylength)

summary(modelres)

plot(SVA_residuals1$Levandevikt ~ SVA_residuals1$bodylength, xlab = "Body length (cm)", ylab = "Body weight (kg)")

abline(modelres1)


#Extracting residuals
resid(modelres)
residuals <- resid(modelres)

print(residuals)

#Adding residuals to data frame
SVA_residuals1$Residuals <- modelres$resid

table(SVA_residuals1$Residuals)
hist(SVA_residuals1$Residuals)
table(SVA_residuals1$bodylength)


#creating and saving the new data frame with residuals:
write.csv(SVA_residuals1, "/Users/mystromgren/Desktop/Master thesis/Data\\SVA_residuals3.csv", row.names=FALSE)
write.xlsx(SVA_residuals1, "/Users/mystromgren/Desktop/Master thesis/Data\\SVA_residuals3.xlsx")
