residuals <- read.csv("/Users/mystromgren/Desktop/Master thesis/Data/wolves_residuals.csv", header=TRUE, sep=";")

library(car)

names(residuals)

sum(is.na(residuals$Inavelskoefficient))

#prepairing data
wolves_residuals <- residuals[!is.na(residuals$Inavelskoefficient),]
wolves_r <- wolves_residuals[!is.na(wolves_residuals$number_neighbour_terr),]
wolves_r1 <- wolves_r[!is.na(wolves_r$hunt_county),]
wolves_r1$Age_group[wolves_r1$Age_group == "pup"] <- "Pups"
wolves_r1$Age_group[wolves_r1$Age_group == "young"] <- "Yearlings"
wolves_r1$Age_group[wolves_r1$Age_group == "adult"] <- "Adults"

colnames(wolves_r1)[colnames(wolves_r1) == "Quater"] <- "Season"
wolves_r1$Season[wolves_r1$Season == "1"] <- "Quater 1"
wolves_r1$Season[wolves_r1$Season == "4"] <- "Quater 4"

#testing for correlation between residuals and weight
cor.test(wolves_r1$Residuals, wolves_r1$Weight_kg, method=c("pearson"))

#max, min and mean values
nrow(wolves_r1) #140 observationer
table(wolves_r1$Wolf_individual_capture) #96 individer
lowest_value <- min(wolves_r1$Residuals, na.rm = TRUE)
print(lowest_value) #-13.14099
highest_value <- max(wolves_r1$Residuals, na.rm = TRUE)
print(highest_value) #14.24975
mean(wolves_r1$Residuals)
std.error(wolves_r1$Residuals)

Females <- subset(wolves_r1, Sex == "F")
highest_value <- max(Females$Residuals, na.rm = TRUE)
print(highest_value) 
mean(Females$Residuals)


Males <- subset(wolves_r1, Sex == "M")
highest_value <- min(Males$Residuals, na.rm = TRUE)
print(highest_value) 
mean(Males$Residuals)


highest_value <- min(wolves_r1$hunt_county, na.rm = TRUE)
print(highest_value)

#normality
hist(wolves_r1$Residuals) #normally distributed
hist(wolves_r1$Inavelskoefficient) #Close to normal

#prepairing for figure
wolves_r1$Age_group <- factor(wolves_r1$Age_group, levels = c("Pups", "Yearlings", "Adults"))

library(sjlabelled)
set_label(wolves_r1$Body_weight_kg) <- "Body weight (kg)"
set_label(wolves_r1$Inbreeding_coefficient) <- "Inbreeding coefficient"
set_label(wolves_r1$Age_group) <- "Age group"
set_label(wolves_r1$number_neighbour_terr) <- "Wolf density" 

#figure
set_theme(base = theme_classic())

means <- wolves_r1 %>% 
  group_by(Age_group) %>% 
  summarize(mean_residuals = mean(Residuals))

ggplot(wolves_r1, aes(x = Sex, y = Residuals, fill = Age_group)) + 
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot() +
  scale_fill_manual(values = c( "purple3", "coral1", "dodgerblue")) +
  stat_summary(fun = base::mean, geom = "point", size = 2, shape = 4, position = position_dodge2(width = 0.75)) +
  theme(legend.position = "bottom", axis.text.x = element_text(color = "black"), text = element_text(size = 13)) +
  labs(x = "", 
       y = "Body condition",
       fill = "") +
  scale_x_discrete(labels = c(M = "Male", F = "Female")) + ylim(-15, 20)


#creating models
Nollmodell_res <- lmer(Residuals ~ (1 | Wolf_individual_capture), REML = FALSE, data = wolves_r1)

summary(Nollmodell_res)


model_res1 <- lmer(Residuals ~ Age_group + Sex + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_r1)

summary(model_res1)
confint(model_res1)


model_res2 <- lmer(Residuals ~ Age_group + Sex + Season + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_r1)

summary(model_res2)
confint(model_res2)


model_res3 <- lmer(Residuals ~ Inavelskoefficient + Age_group + Sex + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_r1)

summary(model_res3)


model_res4 <- lmer(Residuals ~ Inavelskoefficient + Age_group + Sex + Season + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_r1)

summary(model_res4)


model_res5 <- lmer(Residuals ~ number_neighbour_terr + Age_group + Sex + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_r1)

summary(model_res5)
confint(model_res5)

model_res6 <- lmer(Residuals ~ number_neighbour_terr + Age_group + Sex + Season + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_r1)

summary(model_res6)
confint(model_res6)

model_res7 <- lmer(Residuals ~ hunt_county + Age_group + Sex + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_r1)

summary(model_res7)


model_res8 <- lmer(Residuals ~ hunt_county + Age_group + Sex + Season + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_r1)

summary(model_res8)


model_res9 <- lmer(Residuals ~ Inavelskoefficient + number_neighbour_terr + Age_group + Sex + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_r1)

summary(model_res9)


model_res10 <- lmer(Residuals ~ Inavelskoefficient + number_neighbour_terr + Age_group + Sex + Season + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_r1)

summary(model_res10)


model_res11 <- lmer(Residuals ~ Inavelskoefficient + hunt_county + Age_group + Sex + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_r1)

summary(model_res11)


model_res12 <- lmer(Residuals ~ Inavelskoefficient + hunt_county + Age_group + Sex + Season + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_r1)

summary(model_res12)


model_res13 <- lmer(Residuals ~ Inavelskoefficient + number_neighbour_terr + hunt_county + Age_group + Sex + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_r1)

summary(model_res13)


model_res14 <- lmer(Residuals ~ Inavelskoefficient + number_neighbour_terr + hunt_county + Age_group + Sex + Season + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_r1)

summary(model_res14)


model_res15 <- lmer(Residuals ~ number_neighbour_terr + hunt_county + Age_group + Sex + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_r1)

summary(model_res15)


model_res16 <- lmer(Residuals ~ number_neighbour_terr + hunt_county + Age_group + Sex + Season + (1 | Wolf_individual_capture), REML = FALSE, data = wolves_r1)

summary(model_res16)



#Testing normality of the residuals of the model (assumption):
hist(resid(model_res4))
qqnorm(resid(model_res4), pch = 1, frame = FALSE)
qqline(resid(model_res4), col = "steelblue", lwd = 2)

#Testing homogeneity of variances (assumption):
leveneTest(residuals(model_res4) ~ wolves_residuals$Inavelskoefficient)


#Testing age groups
table(wolves_r1$Age)
#Deleting all individuals that are not 1 or 2:
wolves_res1 <- subset(wolves_r1, Age > 0)
wolves_res2 <- subset(wolves_res1, Age < 2) 

wolves_resold <- subset(wolves_r1, Age > 1)

wolves_respup <- subset(wolves_r1, Age < 1)

highest_value <- min(wolves_resold$Residuals, na.rm = TRUE)
print(highest_value) 
mean(wolves_res2$Residuals)

model_age12 <- lmer(Residuals ~ Age + (1 | Wolf_individual_capture), data = wolves_res2)

model_ageold <- lmer(Residuals ~ Age + (1 | Wolf_individual_capture), data = wolves_resold)

summary(model_age12)#not significant

summary(model_ageold)#not significant


#plotting models
plot_model(model_res3)
plot(model_weight2)

install.packages("gridExtra")               # Install gridExtra package
library("gridExtra") 
#install.packages("see", dependencies = TRUE)
plot_model(model_res1, type="pred", terms =c("Sex", "Age_group"))

p1 <- plot_model(model_res2, type="pred",
                 terms=c("Sex"), 
                 colors = c( "purple3", "coral1", "dodgerblue"), axis.title = "Body condition", ylim(-8,10))
p2 <- plot_model(model_res2, type="pred",
                 terms=c("Age_group"), 
                 colors = c( "purple3", "coral1", "dodgerblue"), axis.title = "Body condition", ylim(-8,2))
p3 <- plot_model(model_res2, type="pred",
                 terms=c("Season"), 
                 colors = c( "purple3", "coral1", "dodgerblue"), axis.title = "Body condition", ylim(-8,2))
p4 <- plot_model(model_res6, type="pred",
                 terms=c("Season"), 
                 colors = c( "purple3", "coral1", "dodgerblue"), axis.title = "Body condition", ylim(-8,2))


plot_grid(list(p1, p2, p3), tags = NULL)
grid.arrange(p1, p2, ncol = 2)          

