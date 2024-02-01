#install.packages("corrr")
library('corrr')
#install.packages("ggcorrplot")
library(ggcorrplot)
#install.packages("FactoMineR")
library("FactoMineR")
library(tidyverse)

wolves <- read.csv("/Users/mystromgren/Desktop/Master thesis/Data/Wolf_captures_merged.csv", header=TRUE, sep=";")

#prepairing for PCA
sum(is.na(wolves1$Forearm_right_cm))

wolves1 <- wolves[!is.na(wolves$Inavelskoefficient),]
wolves2 <- wolves1[!is.na(wolves1$Body_length_cm),]
wolves3 <- wolves2[!is.na(wolves2$Ear_length_cm),]
wolves4 <- wolves3[!is.na(wolves3$Tail_length_cm),]
#wolves5 <- wolves4[!is.na(wolves4$Head_length_cm),]
#wolves6 <- wolves5[!is.na(wolves5$Head_circumference_cm),]
#wolves7 <- wolves6[!is.na(wolves6$Chest_circumference_cm),]
#wolves8 <- wolves7[!is.na(wolves7$Neck_circumference_cm),]
#wolves9 <- wolves8[!is.na(wolves8$Forearm_right_cm),]


#PCA of body length, ear length and tail length
wolves_PCA <- wolves4[,c('Body_length_cm', 'Ear_length_cm', 'Tail_length_cm')]
results_PCA <- prcomp(wolves_PCA, scale = TRUE)

results_PCA$rotation <- 1*results_PCA$rotation
results_PCA$rotation

results_PCA$sdev^2 / sum(results_PCA$sdev^2)

#adding to data set
wolves4$pc1 <- results_PCA[["x"]][,1]

#prepairing data set for analysis
wolves_pca1 <- wolves4[!is.na(wolves4$number_neighbour_terr),]
wolves_pca <- wolves_pca1[!is.na(wolves_pca1$hunt_county),]
wolves_pca$Age_group[wolves_pca$Age_group == "pup"] <- "Pups"
wolves_pca$Age_group[wolves_pca$Age_group == "young"] <- "Yearlings"
wolves_pca$Age_group[wolves_pca$Age_group == "adult"] <- "Adults"

colnames(wolves_pca)[colnames(wolves_pca) == "Quater"] <- "Season"
wolves_pca$Season[wolves_pca$Season == "1"] <- "Quarter 1"
wolves_pca$Season[wolves_pca$Season == "4"] <- "Quarter 4"

#min, max and mean values
nrow(wolves_pca) #130 observationer
table(wolves_pca$Wolf_individual_capture) #88 individer
lowest_value <- min(wolves_pca$pc1, na.rm = TRUE)
print(lowest_value) #-3.369092
highest_value <- max(wolves_pca$pc1, na.rm = TRUE)
print(highest_value) #4.260912
mean(wolves_pca$pc1)
std.error(wolves_pca$pc1)


Females <- subset(wolves_pca, Sex == "F")
highest_value <- min(Females$pc1, na.rm = TRUE)
print(highest_value) 
mean(Females$pc1)

Males <- subset(wolves_pca, Sex == "M")
highest_value <- max(Males$pc1, na.rm = TRUE)
print(highest_value) 
mean(Males$pc1)

#testing correlation between weight and PC1
cor.test(wolves_pca$Weight_kg, wolves_pca$pc1, method=c("pearson"))


#Prepairing for figure
wolves_pca$Age_group <- factor(wolves_pca$Age_group, levels = c("Pups", "Yearlings", "Adults"))

library(sjlabelled)
set_label(wolves_pca$Body_weight_kg) <- "Body weight (kg)"
set_label(wolves_pca$Inavelskoefficient) <- "Inbreeding coefficient"
set_label(wolves_pca$Age_group) <- "Age group"
set_label(wolves_pca$number_neighbour_terr) <- "Wolf density"
set_label(wolves_pca$hunt_county) <- "Food availability"

#creating figure
set_theme(base = theme_classic())

means <- wolves_pca %>% 
  group_by(Age_group) %>% 
  summarize(mean_residuals = mean(pc1))

ggplot(wolves_pca, aes(x = Sex, y = pc1, fill = Age_group)) + 
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot() +
  scale_fill_manual(values = c( "purple3", "coral1", "dodgerblue")) +
  stat_summary(fun = base::mean, geom = "point", size = 2, shape = 4, position = position_dodge2(width = 0.75)) +
  theme(legend.position = "bottom", axis.text.x = element_text(color = "black"), text = element_text(size = 13)) +
  labs(x = "", 
       y = "Structural size",
       fill = "") +
  scale_x_discrete(labels = c(M = "Male", F = "Female")) + ylim(-4, 5)


#creating models
Nollmodell_pca <- lmer(pc1 ~ (1 | Wolf_individual_capture), REML = FALSE, data = wolves_pca)

summary(Nollmodell_res)


model_pca1 <- lmer(pc1 ~ Age_group + Sex + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_pca)

summary(model_pca1)
confint(model_pca1)

model_pca2 <- lmer(pc1 ~ Age_group + Sex + Season + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_pca)

summary(model_pca2)
confint(model_pca2)

model_pca3 <- lmer(pc1 ~ Inavelskoefficient + Age_group + Sex + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_pca)

summary(model_pca3)
confint(model_pca3)

model_pca4 <- lmer(pc1 ~ Inavelskoefficient + Age_group + Sex + Season + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_pca)

summary(model_pca4)
confint(model_pca4)

model_pca5 <- lmer(pc1 ~ number_neighbour_terr + Age_group + Sex + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_pca)

summary(model_pca5)
confint(model_pca5)

model_pca6 <- lmer(pc1 ~ number_neighbour_terr + Age_group + Sex + Season + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_pca)

summary(model_pca6)


model_pca7 <- lmer(pc1 ~ hunt_county + Age_group + Sex + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_pca)

summary(model_pca7)
confint(model_pca7)

model_pca8 <- lmer(pc1 ~ hunt_county + Age_group + Sex + Season + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_pca)

summary(model_pca8)


model_pca9 <- lmer(pc1 ~ Inavelskoefficient + number_neighbour_terr + Age_group + Sex + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_pca)

summary(model_pca9)


model_pca10 <- lmer(pc1 ~ Inavelskoefficient + number_neighbour_terr + Age_group + Sex + Season + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_pca)

summary(model_pca10)


model_pca11 <- lmer(pc1 ~ Inavelskoefficient + hunt_county + Age_group + Sex + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_pca)

summary(model_pca11)


model_pca12 <- lmer(pc1 ~ Inavelskoefficient + hunt_county + Age_group + Sex + Season + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_pca)

summary(model_pca12)


model_pca13 <- lmer(pc1 ~ Inavelskoefficient + number_neighbour_terr + hunt_county + Age_group + Sex + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_pca)

summary(model_pca13)


model_pca14 <- lmer(pc1 ~ Inavelskoefficient + number_neighbour_terr + hunt_county + Age_group + Sex + Season + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_pca)

summary(model_pca14)


model_pca15 <- lmer(pc1 ~ number_neighbour_terr + hunt_county + Age_group + Sex + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_pca)

summary(model_pca15)


model_pca16 <- lmer(pc1 ~ number_neighbour_terr + hunt_county + Age_group + Sex + Season + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_pca)

summary(model_pca16)


#Testing normality of the residuals of the model (assumption):
hist(resid(model_pca7))
qqnorm(resid(model_pca4), pch = 1, frame = FALSE)
qqline(resid(model_pca4), col = "steelblue", lwd = 2)

#Testing homogeneity of variances (assumption):
leveneTest(residuals(model_pca4) ~ wolves_pca$pc1)


#Testing age groups
table(wolves_pca$Age)
#Deleting all individuals that are not 1 or 2:
wolves_pca1 <- subset(wolves_pca, Age > 0)
wolves_pca2 <- subset(wolves_pca1, Age < 2) 

wolves_pcaold <- subset(wolves_pca, Age > 1)

wolves_pcapup <- subset(wolves_pca, Age < 1)

highest_value <- min(wolves_pcaold$pc1, na.rm = TRUE)
print(highest_value) 
mean(wolves_pca2$pc1)

model_age12 <- lmer(pc1 ~ Age + (1 | Wolf_individual_capture), data = wolves_pca2)
model_ageold <- lmer(pc1 ~ Age + (1 | Wolf_individual_capture), data = wolves_pcaold)

summary(model_age12)#not significant
summary(model_ageold)#not significant


#Plotting models
p1 <- plot_model(model_pca7, type="pred",
                 terms=c("Sex"), 
                 colors = c( "purple3", "coral1", "dodgerblue"), axis.title = "Structural size")
p2 <- plot_model(model_pca7, type="pred",
                 terms=c("Age_group"), 
                 colors = c( "purple3", "coral1", "dodgerblue"), axis.title = "Structural size")
p3 <- plot_model(model_pca7, type="pred",
                 terms=c("hunt_county"), 
                 colors = c( "purple3", "coral1", "dodgerblue"), axis.title = "Structural size")
p4 <- plot_model(model_pca4, type="pred",
                 terms=c("Season"), 
                 colors = c( "purple3", "coral1", "dodgerblue"), axis.title = "Structural size")


plot_grid(list(p1, p2, p3), tags = NULL)
grid.arrange(p1, p2, ncol = 2)      



#Testing correlation between residuals and PC1
modelrescap <- lm(wolves_pca$Weight_kg ~ wolves_pca$Body_length_cm)

summary(modelrescap)

plot(wolves_pca$Weight_kg ~ wolves_pca$Body_length_cm)
abline(modelrescap)

#Extracting residuals
resid(modelrescap)
residualscap <- resid(modelrescap)

print(residuals)

#Adding residuals to data frame
wolves_pca$Residuals <- modelrescap$resid

wolves_pca

cor.test(wolves_pca$Residuals, wolves_pca$pc1, method=c("pearson"))

