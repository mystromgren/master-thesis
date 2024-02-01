wolves <- read.csv("/Users/mystromgren/Desktop/Master thesis/Data/Wolf_captures_merged.csv", header=TRUE, sep=";")

#Getting rid of NA individuals in body_length(21) (there are no NAs in weight)
wolves_residuals <- wolves[!is.na(wolves$Body_length_cm),]

nrow(wolves3) #162 data points when NAs are removed
table(wolves3$Wolf_individual_capture) #115 individuals

#creating vectors, making sure r interpret them as numeric
weight3 <- as.numeric(wolves3$Weight_kg)
body_length <- as.numeric(wolves3$Body_length_cm)
body_length
weight3

#creating model for residuals
modelres <- lm(wolves_residuals$Weight_kg ~ wolves_residuals$Body_length_cm)

summary(modelres)

plot(wolves_residuals$Weight_kg ~ wolves_residuals$Body_length_cm, xlab = "Body length (cm)", ylab = "Body weight (kg)",  ylim=c(20,60))

abline(modelres)

#Extracting residuals
resid(modelres)
residuals <- resid(modelres)

print(residuals)

#Adding residuals to data frame
wolves_residuals$Residuals <- modelres$resid

wolves_residuals



#creating and saving the new data frame with residuals:
write.csv(wolves_residuals, "/Users/mystromgren/Desktop/Master thesis/Data\\wolves_residuals.csv", row.names=FALSE)
write.xlsx(wolves_residuals, "/Users/mystromgren/Desktop/Master thesis/Data\\wolves_residuals.xlsx")
