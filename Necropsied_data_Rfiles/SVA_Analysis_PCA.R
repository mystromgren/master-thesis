#install.packages("corrr")
library('corrr')
#install.packages("ggcorrplot")
library(ggcorrplot)
#install.packages("FactoMineR")
library("FactoMineR")
library(tidyverse)

wolves <- read.csv("/Users/mystromgren/Desktop/Master thesis/SVA_pcaNeww.csv", header=TRUE, sep=";", skipNul = TRUE)

#Preparing data
sum(is.na(wolves4$Quater ))

wolves_pca1 <- wolves[!is.na(wolves$number_neighbour_terr),]
wolves_pcaSVA <- wolves_pca1[!is.na(wolves_pca1$hunt_county),]
wolves_pcaSVA$Age_group[wolves_pcaSVA$Age_group == "pup"] <- "Pups"
wolves_pcaSVA$Age_group[wolves_pcaSVA$Age_group == "young"] <- "Yearlings"
wolves_pcaSVA$Age_group[wolves_pcaSVA$Age_group == "adult"] <- "Adults"

colnames(wolves_pcaSVA)[colnames(wolves_pcaSVA) == "Quater"] <- "Season"
wolves_pcaSVA$Season[wolves_pcaSVA$Season == "1"] <- "Quarter 1"
wolves_pcaSVA$Season[wolves_pcaSVA$Season == "4"] <- "Quarter 4"
wolves_pcaSVA$Season[wolves_pcaSVA$Season == "2"] <- "Quarter 2"
wolves_pcaSVA$Season[wolves_pcaSVA$Season == "3"] <- "Quarter 3"

colnames(wolves_pcaSVA)[colnames(wolves_pcaSVA) == "Kön"] <- "Sex"
wolves_pcaSVA$Sex[wolves_pcaSVA$Sex == "F"] <- "Female"
wolves_pcaSVA$Sex[wolves_pcaSVA$Sex == "M"] <- "Male"


#max, min and mean values
nrow(wolves_pcaSVA) #216 observationer
lowest_value <- min(wolves_pcaSVA$pc1, na.rm = TRUE)
print(lowest_value) #23.5
highest_value <- max(wolves_pcaSVA$pc1, na.rm = TRUE)
print(highest_value) #57
mean(wolves_pcaSVA$pc1)
std.error(wolves_pcaSVA$pc1)

Females <- subset(wolves_pcaSVA, Sex == "Female")
highest_value <- min(Females$pc1, na.rm = TRUE)
print(highest_value) 
mean(Females$pc1)

Males <- subset(wolves_pcaSVA, Sex == "Male")
highest_value <- max(Males$pc1, na.rm = TRUE)
print(highest_value) 
mean(Males$pc1)

#preparing for figure
wolves_pcaSVA$Age_group <- factor(wolves_pcaSVA$Age_group, levels = c("Pups", "Yearlings", "Adults"))

library(sjlabelled)
set_label(wolves_pcaSVA$Weight) <- "Body weight (kg)"
set_label(wolves_pcaSVA$Inavelskoefficient) <- "Inbreeding coefficient"
set_label(wolves_pcaSVA$Age_group) <- "Age group"
set_label(wolves_pcaSVA$hunt_county) <- "Food availability"
set_label(wolves_pcaSVA$number_neighbour_terr) <- "Wolf density"

#creating figure
set_theme(base = theme_classic())

means <- wolves_pcaSVA %>% 
  group_by(Age_group) %>% 
  summarize(mean_residuals = mean(pc1))

ggplot(wolves_pcaSVA, aes(x = Sex, y = pc1, fill = Age_group)) + 
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
Nollmodell_pcaSVA <- lm(pc1 ~ 1, data = wolves_pcaSVA)

summary(Nollmodell_pcaSVA)


model_pca1SVA <- lm(pc1 ~ Age_group + Sex, data = wolves_pcaSVA)

summary(model_pca1SVA)


model_pca2SVA <- lm(pc1 ~ Age_group + Sex + Season, data = wolves_pcaSVA)

summary(model_pca2SVA)


model_pca3SVA <- lm(pc1 ~ Inavelskoefficient + Age_group + Sex, data = wolves_pcaSVA)

summary(model_pca3SVA)


model_pca4SVA <- lm(pc1 ~ Inavelskoefficient + Age_group + Sex + Season, data = wolves_pcaSVA)

summary(model_pca4SVA)


model_pca5SVA <- lm(pc1 ~ number_neighbour_terr + Age_group + Sex, data = wolves_pcaSVA)

summary(model_pca5SVA)


model_pca6SVA <- lm(pc1 ~ number_neighbour_terr + Age_group + Sex + Season, data = wolves_pcaSVA)

summary(model_pca6SVA)


model_pca7SVA <- lm(pc1 ~ hunt_county + Age_group + Sex, data = wolves_pcaSVA)

summary(model_pca7SVA)


model_pca8SVA <- lm(pc1 ~ hunt_county + Age_group + Sex + Season, data = wolves_pcaSVA)

summary(model_pca8SVA)


model_pca9SVA <- lm(pc1 ~ Inavelskoefficient + number_neighbour_terr + Age_group + Sex, data = wolves_pcaSVA)

summary(model_pca9SVA)
confint(model_pca9SVA)

model_pca10SVA <- lm(pc1 ~ Inavelskoefficient + number_neighbour_terr + Age_group + Sex + Season, data = wolves_pcaSVA)

summary(model_pca10SVA)


model_pca11SVA <- lm(pc1 ~ Inavelskoefficient + hunt_county + Age_group + Sex, data = wolves_pcaSVA)

summary(model_pca11SVA)


model_pca12SVA <- lm(pc1 ~ Inavelskoefficient + hunt_county + Age_group + Sex + Season, data = wolves_pcaSVA)

summary(model_pca12SVA)


model_pca13SVA <- lm(pc1 ~ Inavelskoefficient + number_neighbour_terr + hunt_county + Age_group + Sex, data = wolves_pcaSVA)

summary(model_pca13SVA)
confint(model_pca13SVA)

model_pca14SVA <- lm(pc1 ~ Inavelskoefficient + number_neighbour_terr + hunt_county + Age_group + Sex + Season, data = wolves_pcaSVA)

summary(model_pca14SVA)


model_pca15SVA <- lm(pc1 ~ number_neighbour_terr + hunt_county + Age_group + Sex, data = wolves_pcaSVA)

summary(model_pca15SVA)


model_pca16SVA <- lm(pc1 ~ number_neighbour_terr + hunt_county + Age_group + Sex + Season, data = wolves_pcaSVA)

summary(model_pca16SVA)



#Testing normality of the residuals of the model (assumption):
hist(resid(model_pca7))
qqnorm(resid(model_pca4), pch = 1, frame = FALSE)
qqline(resid(model_pca4), col = "steelblue", lwd = 2)

#Testing homogeneity of variances (assumption):
leveneTest(residuals(model_pca4) ~ wolves_pca$pc1)


#TEsting age groups
table(wolves_pcaSVA$Age_group)
#Deleting all individuals that are not 1 or 2:
wolves_pca1 <- subset(wolves_pcaSVA, Ålder > 0)
wolves_pca2 <- subset(wolves_pca1, Ålder < 2) 

wolves_pcaold <- subset(wolves_pcaSVA, Ålder > 1)

wolves_pcapup <- subset(wolves_pcaSVA, Ålder < 1)

highest_value <- min(wolves_pcaold$pc1, na.rm = TRUE)
print(highest_value) 
mean(wolves_pca2$pc1)

model_age12 <- lm(pc1 ~ Ålder, data = wolves_pca2)
model_ageold <- lm(pc1 ~ Ålder, data = wolves_pcaold)

summary(model_age12)#not significant
summary(model_ageold)#not significant

#plotting models
plot_model(model_res3)
plot(model_weight2)
#install.packages("see", dependencies = TRUE)
plot_model(model_pca9SVA, type="pred", terms =c("number_neighbour_terr","Age_group", "Sex"))


p1 <- plot_model(model_pca9SVA, type="pred",
                 terms=c("Sex"), 
                 colors = c( "purple3", "coral1", "dodgerblue"), axis.title = "Structural size")
p2 <- plot_model(model_pca9SVA, type="pred",
                 terms=c("Age_group"), 
                 colors = c( "purple3", "coral1", "dodgerblue"), axis.title = "Structural size")
p3 <- plot_model(model_pca9SVA, type="pred",
                 terms=c("Inavelskoefficient"), 
                 colors = c( "purple3", "coral1", "dodgerblue"), axis.title = "Structural size")
p4 <- plot_model(model_pca9SVA, type="pred",
                 terms=c("number_neighbour_terr"), 
                 colors = c( "purple3", "coral1", "dodgerblue"), axis.title = "Structural size")

grid.arrange(p1, p2, p3,p4, ncol = 2)      

plot_grid(list(p1, p2, p3, p4), tags = NULL)


#testing correlations between pc1 and residuals

mergednec <- merge(wolves_pcaSVA, SVA_r1, by = "ROVBASEID")

mergednec <- mergednec[!is.na(mergednec$pc1),]

cor.test(mergednec$Residuals, mergednec$pc1, method=c("pearson"))

#corr between pc1 and weight

cor.test(wolves_pcaSVA$Levandevikt, wolves_pcaSVA$pc1, method=c("pearson"))

