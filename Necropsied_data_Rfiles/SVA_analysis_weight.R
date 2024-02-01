SVA <- read.csv("/Users/mystromgren/Desktop/Master thesis/SVA_omarbetad2.csv", header=TRUE, sep=";")

library(lme4)
library(lmerTest)
library(ggplot2)


#Preparing data 
sum(SVA$Levandevikt=="")
sum(is.na(SVA$Levanedvikt))
SVA <- subset(SVA, !is.na(Levandevikt) & Levandevikt != "")

sum(is.na(SVA$Kön))
SVA <- subset(SVA, !is.na(Kön) & Kön != "")

sum(is.na(SVA$Ålder))
SVA <- subset(SVA, !is.na(Ålder) & Ålder != "")

sum(is.na(SVA$Quater))
SVA <- subset(SVA, !is.na(Quater) & Quater != "")

sum(is.na(SVA$Inavelskoefficient))
SVA <- subset(SVA, !is.na(Inavelskoefficient) & Inavelskoefficient != "")


SVA <- subset(SVA, !is.na(number_neighbour_terr) & number_neighbour_terr != "")
SVA <- subset(SVA, !is.na(hunt_county) & hunt_county != "")
SVA$Age_group[SVA$Age_group == "pup"] <- "Pups"
SVA$Age_group[SVA$Age_group == "young"] <- "Yearlings"
SVA$Age_group[SVA$Age_group == "adult"] <- "Adults"

SVA$Kön[SVA$Kön == "F"] <- "Female"
SVA$Kön[SVA$Kön == "M"] <- "Male"

colnames(SVA)[colnames(SVA) == "Quater"] <- "Season"
SVA$Season[SVA$Season == "1"] <- "Quarter 1"
SVA$Season[SVA$Season == "4"] <- "Quarter 4"
SVA$Season[SVA$Season == "2"] <- "Quarter 2"
SVA$Season[SVA$Season == "3"] <- "Quarter 3"

colnames(SVA)[colnames(SVA) == "Levandevikt"] <- "Weight"
colnames(SVA)[colnames(SVA) == "Kön"] <- "Sex"


#max, min and mean values
nrow(SVA) #338 observationer / individer
lowest_value <- min(SVA$Weight, na.rm = TRUE)
print(lowest_value) #9.7
highest_value <- max(SVA$Weight, na.rm = TRUE)
print(highest_value) #54
mean(SVA$Weight)
std.error(SVA$Weight)

lowest_value <- max(SVA$År, na.rm = TRUE)
print(lowest_value)

Females <- subset(SVA, Sex == "Female")
highest_value <- min(Females$Weight, na.rm = TRUE)
print(highest_value) 
mean(Females$Weight)

Males <- subset(SVA, Sex == "Male")
highest_value <- max(Males$Weight, na.rm = TRUE)
print(highest_value) 
mean(Males$Weight)


#Preparing for figure
SVA$Age_group <- factor(SVA$Age_group, levels = c("Pups", "Yearlings", "Adults"))

SVA$Season <- factor(SVA$Season, levels = c("Quarter 1", "Quarter 2", "Quarter 3", "Quarter 4"))

library(sjlabelled)
set_label(SVA$Weight) <- "Body weight (kg)"
set_label(SVA$Inavelskoefficient) <- "Inbreeding coefficient"
set_label(SVA$Age_group) <- "Age group"
set_label(SVA$hunt_county) <- "Food availability"

#creating figure
set_theme(base = theme_classic())

means <- SVA %>% 
  group_by(Age_group) %>% 
  summarize(mean_residuals = mean(Weight))

ggplot(SVA, aes(x = Sex, y = Weight, fill = Age_group)) + 
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot() +
  scale_fill_manual(values = c( "purple3", "coral1", "dodgerblue")) +
  stat_summary(fun = base::mean, geom = "point", size = 2, shape = 4, position = position_dodge2(width = 0.75)) +
  theme(legend.position = "bottom", axis.text.x = element_text(color = "black"), text = element_text(size = 13)) +
  labs(x = "", 
       y = "Body weight (kg)",
       fill = "") +
  scale_x_discrete(labels = c(M = "Male", F = "Female")) + ylim(9, 60)

#Normality
hist(SVA$Weight) #Close to normal
hist(SVA$Inavelskoefficient) #Normally distributed

#correlation tests independent variables
cor.test( ~ Inavelskoefficient + number_neighbour_terr , #no correlation
          data=SVA,
          method = "spearman",
          exact=FALSE, 
          continuity = FALSE,
          conf.level = 0.95)

cor.test( ~ Inavelskoefficient + hunt_county , #no correlation
          data=SVA,
          method = "spearman",
          exact=FALSE, 
          continuity = FALSE,
          conf.level = 0.95)


#creating models
Nollmodell_weightSVA <- lm(Weight ~ 1, data = SVA)

summary(Nollmodell_weightSVA)


model_weight1SVA <- lm(Weight ~ Age_group + Sex, data = SVA)

summary(model_weight1SVA)
confint(model_weight1SVA)


model_weight2SVA <- lm(Weight ~ Age_group + Sex + Season, data = SVA)

summary(model_weight2SVA)
confint(model_weight2SVA)

model_weight3SVA <- lm(Weight ~ Inavelskoefficient + Age_group + Sex, data = SVA)

summary(model_weight3SVA)
confint(model_weight3SVA)


model_weight4SVA <- lm(Weight ~ Inavelskoefficient + Age_group + Sex + Season, data = SVA)

summary(model_weight4SVA)
confint(model_weight4SVA)

model_weight5SVA <- lm(Weight ~ number_neighbour_terr + Age_group + Sex, data = SVA)

summary(model_weight5SVA)


model_weight6SVA <- lm(Weight ~ number_neighbour_terr + Age_group + Sex + Season, data = SVA)

summary(model_weight6SVA)


model_weight7SVA <- lm(Weight ~ hunt_county + Age_group + Sex, data = SVA)

summary(model_weight7SVA)
confint(model_weight7SVA)

model_weight8SVA <- lm(Weight ~ hunt_county + Age_group + Sex + Season, data = SVA)

summary(model_weight8SVA)
confint(model_weight8SVA)


model_weight9SVA <- lm(Weight ~ Inavelskoefficient + number_neighbour_terr + Age_group + Sex, data = SVA)

summary(model_weight9SVA)


model_weight10SVA <- lm(Weight ~ Inavelskoefficient + number_neighbour_terr + Age_group + Sex + Season, data = SVA)

summary(model_weight10SVA)


model_weight11SVA <- lm(Weight ~ Inavelskoefficient + hunt_county + Age_group + Sex, data = SVA)

summary(model_weight11SVA)
confint(model_weight11SVA)

model_weight12SVA <- lm(Weight ~ Inavelskoefficient + hunt_county + Age_group + Sex + Season, data = SVA)

summary(model_weight12SVA)
confint(model_weight12SVA)

model_weight13SVA <- lm(Weight ~ Inavelskoefficient + number_neighbour_terr + hunt_county + Age_group + Sex, data = SVA)

summary(model_weight13SVA)


model_weight14SVA <- lm(Weight ~ Inavelskoefficient + number_neighbour_terr + hunt_county + Age_group + Sex + Season, data = SVA)

summary(model_weight14SVA)


model_weight15SVA <- lm(Weight ~ number_neighbour_terr + hunt_county + Age_group + Sex, data = SVA)

summary(model_weight15SVA)
confint(model_weight15SVA)

model_weight16SVA <- lm(Weight ~ number_neighbour_terr + hunt_county + Age_group + Sex + Season, data = SVA)

summary(model_weight16SVA)
confint(model_weight16SVA)



#Testing normality of the residuals of the model (assumption):
hist(resid(model_weight4))
qqnorm(resid(model_weight4), pch = 1, frame = FALSE)
qqline(resid(model_weight4), col = "steelblue", lwd = 2)

#Testing homogeneity of variances (assumption):
leveneTest(residuals(model_weight4) ~ wolves_weight$Weight_kg)


#Testing age groups
table(SVA$Ålder)
#Deleting all individuals that are not 1 or 2:
SVA_weight1 <- subset(SVA, Ålder > 0)
SVA_weight2 <- subset(SVA_weight1, Ålder < 2) 

SVA_weightold <- subset(SVA, Ålder > 1)

SVA_zero <- subset(SVA, Ålder < 1)

model_age12 <- lm(Weight ~ Ålder, data = SVA_weight2)
model_ageold <- lm(Weight ~ Ålder, data = SVA_weightold)

summary(model_age12)#not significant
summary(model_ageold)#not significant
mean(SVA_weightold$Weight)
mean(SVA_weight2$Weight)
mean(SVA_zero$Weight)

#plotting models
install.packages("sjPlot")
library("sjPlot")
plot_model(model_weight6)
plot(model_weight2)
install.packages("see", dependencies = TRUE)
plot_model(model_weight8SVA, type="pred", terms =c("hunt_county","Age_group", "Sex", "Season"))


p1 <- plot_model(model_weight3SVA, type="pred",
                 terms=c("Sex"), 
                 colors = c( "purple3", "coral1", "dodgerblue"), axis.title = "Body weight (kg)")
p2 <- plot_model(model_weight3SVA, type="pred",
                 terms=c("Age_group"), 
                 colors = c( "purple3", "coral1", "dodgerblue"), axis.title = "Body weight (kg)")
p3 <- plot_model(model_weight3SVA, type="pred",
                 terms=c("Inavelskoefficient"), 
                 colors = c( "purple3", "coral1", "dodgerblue"), axis.title = "Body weight (kg)")
p4 <- plot_model(model_weight4SVA, type="pred",
                 terms=c("Season"), 
                 colors = c( "purple3", "coral1", "dodgerblue"), axis.title = "Body weight (kg)")


grid.arrange(p1, p2, ncol = 2)      

plot_grid(list(p1, p2, p3), tags = NULL)

