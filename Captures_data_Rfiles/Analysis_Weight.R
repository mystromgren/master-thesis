wolves <- read.csv("/Users/mystromgren/Desktop/Master thesis/Data/Wolf_captures_merged.csv", header=TRUE, sep=";")

#install.packages("plotrix")
library(lme4)
library(lmerTest)
library(ggplot2)
library(plotrix)

names(wolves)


sum(is.na(wolves$Weight_kg))
sum(is.na(wolves$Sex))
sum(is.na(wolves$Age_group))
sum(is.na(wolves$Quater))

sum(is.na(wolves$Inavelskoefficient))

#prepairing data
wolves_weight <- wolves[!is.na(wolves$Inavelskoefficient),]
wolves_w <- wolves_weight[!is.na(wolves_weight$number_neighbour_terr),]
wolves_w1 <- wolves_w[!is.na(wolves_w$hunt_county),]
wolves_w1$Age_group[wolves_w1$Age_group == "pup"] <- "Pups"
wolves_w1$Age_group[wolves_w1$Age_group == "young"] <- "Yearlings"
wolves_w1$Age_group[wolves_w1$Age_group == "adult"] <- "Adults"

colnames(wolves_w1)[colnames(wolves_w1) == "Quater"] <- "Season"
wolves_w1$Season[wolves_w1$Season == "1"] <- "quarter 1"
wolves_w1$Season[wolves_w1$Season == "4"] <- "quarter 4"

colnames(wolves_w1)[colnames(wolves_w1) == "Inavelskoefficient"] <- "Inbreeding_coefficient"
colnames(wolves_w1)[colnames(wolves_w1) == "Weight_kg"] <- "Body_weight_kg"
colnames(wolves_w1)[colnames(wolves_w1) == "number_neighbour_terr"] <- "neigh.terr."
colnames(wolves_w1)[colnames(wolves_w1) == "hunt_county"] <- "Food_availability"


#max, min and mean values
nrow(wolves_w1) #159 observationer
table(wolves_w1$Wolf_individual_capture) #103 individer
lowest_value <- min(wolves_w1$Body_weight_kg, na.rm = TRUE)
print(lowest_value) #23.5
highest_value <- max(wolves_w1$Body_weight_kg, na.rm = TRUE)
print(highest_value) #57
mean(wolves_w1$Body_weight_kg)
std.error(wolves_w1$Body_weight_kg)


Females <- subset(wolves_w1, Sex == "Female")
highest_value <- max(Females$Body_weight_kg, na.rm = TRUE)
print(highest_value) 
mean(Females$Body_weight_kg)

Males <- subset(wolves_w1, Sex == "Male")
highest_value <- max(Males$Body_weight_kg, na.rm = TRUE)
print(highest_value) 
mean(Males$Body_weight_kg)

#prepairing for figure
wolves_w1$Age_group <- factor(wolves_w1$Age_group, levels = c("Pups", "Yearlings", "Adults"))

library(sjlabelled)
set_label(wolves_w1$Body_weight_kg) <- "Body weight (kg)"
set_label(wolves_w1$Inbreeding_coefficient) <- "Inbreeding coefficient"
set_label(wolves_w1$Age_group) <- "Age group"

#figure
set_theme(base = theme_classic())


means <- wolves_w1 %>% 
  group_by(Age_group) %>% 
  summarize(mean_weight = mean(Body_weight_kg))

ggplot(wolves_w1, aes(x = Sex, y = Body_weight_kg, fill = Age_group)) + 
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot() +
  scale_fill_manual(values = c( "purple3", "coral1", "dodgerblue")) +
  stat_summary(fun = mean, geom = "point", size = 2, shape = 4, position = position_dodge2(width = 0.75)) +
theme(legend.position = "bottom", axis.text.x = element_text(color = "black"), text = element_text(size = 13)) +
  labs(x = "", 
       y = "Body weight (kg)",
       fill = "") +
  scale_x_discrete(labels = c(M = "Male", F = "Female")) + ylim(20, 60)


#normality
hist(wolves_w1$Body_weight_kg) #normally distributed
hist(wolves_w1$Inbreeding_coefficient) #Close to normal


#creating models
Nollmodell_weight <- lmer(Body_weight_kg ~ (1 | Wolf_individual_capture), REML = FALSE, data = wolves_w1)

summary(Nollmodell_weight)


model_weight1 <- lmer(Body_weight_kg ~ Age_group + Sex + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_w1)

summary(model_weight1)
confint(model_weight1)

model_weight2 <- lmer(Body_weight_kg ~ Age_group + Sex + Season + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_w1)

summary(model_weight2)


model_weight3 <- lmer(Body_weight_kg ~ Inbreeding_coefficient + Age_group + Sex + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_w1)

summary(model_weight3)
confint(model_weight3)

model_weight4 <- lmer(Body_weight_kg ~ Inbreeding_coefficient + Age_group + Sex + Season + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_w1)

summary(model_weight4)
confint(model_weight4)


model_weight5 <- lmer(Body_weight_kg ~ neigh.terr. + Age_group + Sex + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_w1)

summary(model_weight5)


model_weight6 <- lmer(Body_weight_kg ~ neigh.terr. + Age_group + Sex + Season + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_w1)

summary(model_weight6)


model_weight7 <- lmer(Body_weight_kg ~ Food_availability + Age_group + Sex + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_w1)

summary(model_weight7)


model_weight8 <- lmer(Body_weight_kg ~ Food_availability + Age_group + Sex + Season + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_w1)

summary(model_weight8)


model_weight9 <- lmer(Body_weight_kg ~ Inbreeding_coefficient + neigh.terr. + Age_group + Sex + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_w1)

summary(model_weight9)


model_weight10 <- lmer(Body_weight_kg ~ Inbreeding_coefficient + neigh.terr. + Age_group + Sex + Season + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_w1)

summary(model_weight10)
confint(model_weight10)

model_weight11 <- lmer(Body_weight_kg ~ Inbreeding_coefficient + Food_availability + Age_group + Sex + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_w1)

summary(model_weight11)


model_weight12 <- lmer(Body_weight_kg ~ Inbreeding_coefficient + Food_availability + Age_group + Sex + Season + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_w1)

summary(model_weight12)
confint(model_weight12)

model_weight13 <- lmer(Body_weight_kg ~ Inbreeding_coefficient + neigh.terr. + Food_availability + Age_group + Sex + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_w1)

summary(model_weight13)


model_weight14 <- lmer(Body_weight_kg ~ Inbreeding_coefficient + neigh.terr. + Food_availability + Age_group + Sex + Season + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_w1)

summary(model_weight14)


model_weight15 <- lmer(Body_weight_kg ~ neigh.terr. + Food_availability + Age_group + Sex + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_w1)

summary(model_weight15)


model_weight16 <- lmer(Body_weight_kg ~ neigh.terr. + Food_availability + Age_group + Sex + Season + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_w1)

summary(model_weight16)


#Testing normality of the residuals of the model (assumption):
hist(resid(model_weight4))
qqnorm(resid(model_weight4), pch = 1, frame = FALSE)
qqline(resid(model_weight4), col = "steelblue", lwd = 2)



#creating age groups and testing if age has an influence on weight in age groups
table(wolves_w1$Age)
#Deleting all individuals that are not 1 or 2:
wolves_weight1 <- subset(wolves_w1, Age > 0)
wolves_weight2 <- subset(wolves_weight1, Age < 2) 

wolves_weightold <- subset(wolves_w1, Age > 1)

wolves_weightzero <- subset(wolves_w1, Age < 1)

highest_value12 <- min(wolves_weightzero$Body_weight_kg, na.rm = TRUE)
print(highest_value12)
mean(wolves_weightzero$Body_weight_kg)

model_age12 <- lmer(Body_weight_kg ~ Age + (1 | Wolf_individual_capture), data = wolves_weight2)
model_ageold <- lmer(Body_weight_kg ~ Age + (1 | Wolf_individual_capture), data = wolves_weightold)

summary(model_age12)#not significant
summary(model_ageold)#not significant


#plotting models
install.packages("sjPlot")
library("sjPlot")
plot_model(model_weight6)
plot(model_weight2)
install.packages("see", dependencies = TRUE)


p1 <- plot_model(model_weight10, type="pred",
            terms=c("Sex"), 
           colors = c( "purple3", "coral1", "dodgerblue"), axis.title = "Body weight (kg)", show.title = FALSE)
p2 <- plot_model(model_weight10, type="pred",
           terms=c("Age_group"), 
           colors = c( "purple3", "coral1", "dodgerblue"), axis.title = "Body weight (kg)", legend.title = "")
p3 <- plot_model(model_weight10, type="pred",
                 terms=c("Inbreeding_coefficient"), 
                 colors = c( "purple3", "coral1", "dodgerblue"), axis.title = "Body weight (kg)", legend.title = "")
p4 <- plot_model(model_weight10, type="pred",
                 terms=c("neigh.terr."), 
                 colors = c( "purple3", "coral1", "dodgerblue"), axis.title = "Body weight (kg)", legend.title = "")
p5 <- plot_model(model_weight10, type="pred",
                 terms=c("Season"), 
                 colors = c( "purple3", "coral1", "dodgerblue"), axis.title = "Body weight (kg)", legend.title = "")


plot_grid(list(p1, p2, p3, p4, p5), tags = NULL)

grid.arrange(p1, p2, p3, p4, p5, ncol = 2) 

