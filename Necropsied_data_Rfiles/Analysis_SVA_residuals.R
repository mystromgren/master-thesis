residuals <- read.csv("/Users/mystromgren/Desktop/Master thesis//SVA_residuals3.csv", header=TRUE, sep=",")

table(residuals$bodylength)
library(car)

#preparing data
sum(is.na(residuals$Ålder))
residuals_åldrar<- residuals[!is.na(residuals$Ålder),]

sum(is.na(residuals_åldrar$Kön))
sum(residuals_åldrar$Kön=="")
res_åld_kön <- residuals_åldrar[!is.na(residuals_åldrar$Kön),]

sum(is.na(res_åld_kön$Inavelskoefficient))
sum(res_åld_kön$Inavelskoefficient=="")
res_withoutNA <- res_åld_kön[!is.na(res_åld_kön$Inavelskoefficient),]

table(res_withoutNA$Dödsorsak_test)
table(res_withoutNA$Skabb)

sum(is.na(res_withoutNA$Kön))

SVA_r <- res_withoutNA[!is.na(res_withoutNA$number_neighbour_terr),]
SVA_r1 <- SVA_r[!is.na(SVA_r$hunt_county),]
SVA_r1$Age_group[SVA_r1$Age_group == "pup"] <- "Pups"
SVA_r1$Age_group[SVA_r1$Age_group == "young"] <- "Yearlings"
SVA_r1$Age_group[SVA_r1$Age_group == "adult"] <- "Adults"

colnames(SVA_r1)[colnames(SVA_r1) == "Quater"] <- "Season"
SVA_r1$Season[SVA_r1$Season == "1"] <- "Quarter 1"
SVA_r1$Season[SVA_r1$Season == "4"] <- "Quarter 4"
SVA_r1$Season[SVA_r1$Season == "2"] <- "Quarter 2"
SVA_r1$Season[SVA_r1$Season == "3"] <- "Quarter 3"

colnames(SVA_r1)[colnames(SVA_r1) == "Kön"] <- "Sex"

SVA_r1$Sex[SVA_r1$Sex == "F"] <- "Female"
SVA_r1$Sex[SVA_r1$Sex == "M"] <- "Male"


#max, min and mean values
nrow(SVA_r1) #312 observationer / individer
lowest_value <- min(SVA_r1$Residuals, na.rm = TRUE)
print(lowest_value) #23.5
highest_value <- max(SVA_r1$Residuals, na.rm = TRUE)
print(highest_value) #57
mean(SVA_r1$Residuals)
std.error(SVA_r1$Residuals)

Females <- subset(SVA_r1, Sex == "Female")
highest_value <- min(Females$Residuals, na.rm = TRUE)
print(highest_value) 
mean(Females$Residuals)

Males <- subset(SVA_r1, Sex == "Male")
highest_value <- max(Males$Residuals, na.rm = TRUE)
print(highest_value) 
mean(Males$Residuals)

#preparing for figure
SVA_r1$Age_group <- factor(SVA_r1$Age_group, levels = c("Pups", "Yearlings", "Adults"))

library(sjlabelled)
set_label(SVA_r1$Weight) <- "Body weight (kg)"
set_label(SVA_r1$Inavelskoefficient) <- "Inbreeding coefficient"
set_label(SVA_r1$Age_group) <- "Age group"
set_label(SVA_r1$hunt_county) <- "Food availability"
set_label(SVA_r1$number_neighbour_terr) <- "Wolf density"

#creating figure
set_theme(base = theme_classic())

means <- SVA_r1 %>% 
  group_by(Age_group) %>% 
  summarize(mean_residuals = mean(Residuals))

ggplot(SVA_r1, aes(x = Sex, y = Residuals, fill = Age_group)) + 
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot() +
  scale_fill_manual(values = c( "purple3", "coral1", "dodgerblue")) +
  stat_summary(fun = base::mean, geom = "point", size = 2, shape = 4, position = position_dodge2(width = 0.75)) +
  theme(legend.position = "bottom", axis.text.x = element_text(color = "black"), text = element_text(size = 13)) +
  labs(x = "", 
       y = "Body condition",
       fill = "") +
  scale_x_discrete(labels = c(M = "Male", F = "Female")) + ylim(-20, 20)


#Normality
hist(SVA_r1$Residuals) #normally distributed
hist(SVA_r1$Inavelskoefficient) #normally distributed


#Creating models
Nollmodell_resSVA <- lm(Residuals ~ 1, data = SVA_r1)

summary(Nollmodell_resSVA)


model_res1SVA <- lm(Residuals ~ Age_group + Sex, data = SVA_r1)

summary(model_res1SVA)
confint(model_res1SVA)

model_res2SVA <- lm(Residuals ~ Age_group + Sex + Season, data = SVA_r1)

summary(model_res2SVA)


model_res3SVA <- lm(Residuals ~ Inavelskoefficient + Age_group + Sex, data = SVA_r1)

summary(model_res3SVA)
confint(model_res3SVA)


model_res4SVA <- lm(Residuals ~ Inavelskoefficient + Age_group + Sex + Season, data = SVA_r1)

summary(model_res4SVA)


model_res5SVA <- lm(Residuals ~ number_neighbour_terr + Age_group + Sex, data = SVA_r1)

summary(model_res5SVA)
confint(model_res5SVA)


model_res6SVA <- lm(Residuals ~ number_neighbour_terr + Age_group + Sex + Season, data = SVA_r1)

summary(model_res6SVA)


model_res7SVA <- lm(Residuals ~ hunt_county + Age_group + Sex, data = SVA_r1)

summary(model_res7SVA)
confint(model_res7SVA)

model_res8SVA <- lm(Residuals ~ hunt_county + Age_group + Sex + Season, data = SVA_r1)

summary(model_res8SVA)


model_res9SVA <- lm(Residuals ~ Inavelskoefficient + number_neighbour_terr + Age_group + Sex, data = SVA_r1)

summary(model_res9SVA)


model_res10SVA <- lm(Residuals ~ Inavelskoefficient + number_neighbour_terr + Age_group + Sex + Season, data = SVA_r1)

summary(model_res10SVA)


model_res11SVA <- lm(Residuals ~ Inavelskoefficient + hunt_county + Age_group + Sex, data = SVA_r1)

summary(model_res11SVA)
confint(model_res11SVA)


model_res12SVA <- lm(Residuals ~ Inavelskoefficient + hunt_county + Age_group + Sex + Season, data = SVA_r1)

summary(model_res12SVA)


model_res13SVA <- lm(Residuals ~ Inavelskoefficient + number_neighbour_terr + hunt_county + Age_group + Sex, data = SVA_r1)

summary(model_res13SVA)


model_res14SVA <- lm(Residuals ~ Inavelskoefficient + number_neighbour_terr + hunt_county + Age_group + Sex + Season, data = SVA_r1)

summary(model_res14SVA)


model_res15SVA <- lm(Residuals ~ number_neighbour_terr + hunt_county + Age_group + Sex, data = SVA_r1)

summary(model_res15SVA)
confint(model_res15SVA)

model_res16SVA <- lm(Residuals ~ number_neighbour_terr + hunt_county + Age_group + Sex + Season, data = SVA_r1)

summary(model_res16SVA)



#Testing normality of the residuals of the model (assumption):
hist(resid(model_res4))
qqnorm(resid(model_res4), pch = 1, frame = FALSE)
qqline(resid(model_res4), col = "steelblue", lwd = 2)

#Testing homogeneity of variances (assumption):
leveneTest(residuals(model_res4) ~ wolves_residuals$Inavelskoefficient)


#Testing age groups
table(SVA_r1$Ålder)
#Deleting all individuals that are not 1 or 2:
wolves_res1 <- subset(SVA_r1, Ålder > 0)
wolves_res2 <- subset(wolves_res1, Ålder < 2) 

wolves_resold <- subset(SVA_r1, Ålder > 1)

wolves_reszero <- subset(SVA_r1, Ålder < 1)

model_age12 <- lm(Residuals ~ Ålder, data = wolves_res2)

model_ageold <- lm(Residuals ~ Ålder, data = wolves_resold)

summary(model_age12)#not significant

summary(model_ageold)#not significant
mean(wolves_reszero$Residuals)

#plotting models
plot_model(model_res4)
plot(model_weight2)
#install.packages("see", dependencies = TRUE)
plot_model(model_res7SVA, type="pred", terms =c("hunt_county", "Age_group","Sex"))


p1 <- plot_model(model_res3SVA, type="pred",
                 terms=c("Sex"), 
                 colors = c( "purple3", "coral1", "dodgerblue"), axis.title = "Body condition")
p2 <- plot_model(model_res3SVA, type="pred",
                 terms=c("Age_group"), 
                 colors = c( "purple3", "coral1", "dodgerblue"), axis.title = "Body condition")
p3 <- plot_model(model_res3SVA, type="pred",
                 terms=c("Inavelskoefficient"), 
                 colors = c( "purple3", "coral1", "dodgerblue"), axis.title = "Body condition")

grid.arrange(p1, p2, ncol = 2)      

plot_grid(list(p1, p2, p3), tags = NULL)

#correlation between weight and residuals

cor.test(SVA_r1$Levandevikt, SVA_r1$Residuals, method=c("pearson"))



