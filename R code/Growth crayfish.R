setwd("~/OneDrive/RPTU PolyRAS/Previous work unpublished/Revision/Data processing")
library(ggplot2)
library(readxl)
library(ggpubr)
library(readr)

# Experiment 1
Crayfish_feed_difference <- read_excel("Crayfish feed difference.xlsx")
View(Crayfish_feed_difference)  

# glm analysis with elements
model_feed <- glm(SGR ~ Feed + Survival, data = Crayfish_feed_difference, family = gaussian)
summary(model_feed)
Anova(model_feed)

model_carbon <- glm(SGR ~ Carbon + Survival, data = Crayfish_feed_difference, family = gaussian)
summary(model_carbon)
Anova(model_carbon)

model_nitrogen <- glm(SGR ~ Nitrogen + Survival, data = Crayfish_feed_difference, family = gaussian)
summary(model_nitrogen)
Anova(model_nitrogen)

# plot SGR vs feed, carbon or nitrogen
#plot
library(ggpubr)
plot1 <- ggboxplot(Crayfish_feed_difference, x = "Feed", y = "SGR", color = "Feed", title ="SGR ~ Feed* + Survival")
plot1

plot2 <- ggscatter(Crayfish_feed_difference, x = "Carbon", y = "SGR", title = "SGR ~ Carbon* + Survival", add = "reg.line", add.params = list(color = "blue", fill = "lightgray"), conf.int = TRUE) 
plot2

plot3 <- ggscatter(Crayfish_feed_difference, x = "Nitrogen", y = "SGR", title = "SGR ~ Nitrogen* + Survival", add = "reg.line", add.params = list(color = "blue", fill = "lightgray"), conf.int = TRUE)
plot3

combplot <- ggarrange(plot1, plot2, plot3, labels = c("A", "B", "C"), ncol = 3)
ggsave("experiment1.png", combplot, width = 15, height = 5, dpi = 300)

#Experiment 2
mono_vs_poly <- read_excel("mono vs poly.xlsx")
View(mono_vs_poly)

# glm model fish SGR predicted by treatment and survival
model_fish <- glm(Fish_SGR ~ Fish_Survival + Treat_Fish, data = mono_vs_poly, family = gaussian)
summary(model_fish)
# test for siginificant predictor
library(car)
Anova(model_fish)

# glm model crayfish SGR predicted by treatment, survival, moulting and chelipeds losses
model_crayfish <- glm(Crayfish_SGR ~ Crayfish_survival + Treat_Crayfish + Crayfish_moulting + Crayfish_cheliped_loss, data = mono_vs_poly, family = gaussian)
summary(model_crayfish)
# test for significant predictor
Anova(model_crayfish)

# search for correlation in total gain and mortality between fish and crayfish in polyculture
Crayfish_vs_fish_tank_gain <- read_excel("~/OneDrive/RPTU PolyRAS/Previous work unpublished/Revision/Data processing/Crayfish vs fish tank gain.xlsx")
View(Crayfish_vs_fish_tank_gain) 
library(scales)
interactplot <- ggscatter(Crayfish_vs_fish_tank_gain, x = "Fish_survival", y = "Crayfish_survival", 
                          color = "Treatment", add = "reg.line", 
                          add.params = list(color = "pink", fill = "lightgray"), conf.int = TRUE) +
  scale_x_continuous(labels = scales::percent) + 
  scale_y_continuous(labels = scales::percent)
interactplot
ggsave("experiment2interaction.png",interactplot, height = 5, width = 6, dpi = 300) 
# glm model to compare the prediction between AC and ACsep
model_polyculture_SGR <- glm(Fish_SGR ~ Crayfish_SGR/Treatment, data = Crayfish_vs_fish_tank_gain)
summary(model_polyculture_SGR)

model_polyculture_survival <- glm(Fish_survival ~ Crayfish_survival/Treatment, data = Crayfish_vs_fish_tank_gain)
summary(model_polyculture_survival)
# test for significant predictor
Anova(model_polyculture_SGR)
Anova(model_polyculture_survival)


