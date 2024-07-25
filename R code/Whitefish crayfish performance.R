setwd("~/OneDrive/RPTU PolyRAS/Previous work unpublished/Revision/Data processing")
library(ggplot2)
library(readxl)
library(ggpubr)
library(readr)

# Experiment 1
Crayfish_feed_difference <- read_excel("Crayfish feed difference.xlsx")
View(Crayfish_feed_difference)  

glm_carbon <- glm(SGR ~ Carbon, data = Crayfish_feed_difference, family = gaussian)
summary(glm_carbon)
library(car)
Anova(glm_carbon)

glm_nitrogen <- glm(SGR ~ Nitrogen, data = Crayfish_feed_difference, family = gaussian)
summary(glm_nitrogen)
Anova(glm_nitrogen)

glm_cn <- glm(SGR ~ Carbon + Nitrogen, data = Crayfish_feed_difference, family = gaussian)
summary(glm_cn)
Anova(glm_cn)

# Compare SGR or tank gain
library(stats)

#normality test
split_df <- split(Crayfish_feed_difference, Crayfish_feed_difference$Feed)
shapiro_results_SGR <- lapply(split_df, function(sub_df) {
  shapiro.test(sub_df[["SGR"]])
})
print(shapiro_results_SGR)

shapiro_results_Survival <- lapply(split_df, function(sub_df) {
  shapiro.test(sub_df[["Survival"]])
})
print(shapiro_results_Survival)


#test homogeneity of variances
library(car)
levene_result_SGR <- leveneTest(SGR ~ Feed, data = Crayfish_feed_difference)
print(levene_result_SGR)

levene_result_Survival <- leveneTest(Survival ~ Feed, data = Crayfish_feed_difference)
print(levene_result_Survival)

kruskal.test(Survival ~ Feed, data = Crayfish_feed_difference)



#welch Anova SGR
welch_result <- oneway.test(SGR ~ Feed, data = Crayfish_feed_difference, var.equal = FALSE)
print(welch_result)

# post-hoc test for welch Anova
install.packages("rstatix")

# Load the rstatix package
library(rstatix)

welch_post <- games_howell_test(Crayfish_feed_difference, SGR ~ Feed, conf.level = 0.95, detailed = T)
print(welch_post, n=Inf)

# plot SGR vs feed, carbon or nitrogen
#plot
library(ggpubr)
survival <- ggboxplot(
  Crayfish_feed_difference, 
  x = "Feed", 
  y = "Survival", 
  color = "Feed") +
  theme(legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
  )
survival

plot <- ggboxplot(
  Crayfish_feed_difference, 
  x = "Feed", 
  y = "SGR", 
  color = "Feed") +
  theme(legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
  )
  

annotations <- data.frame(
    x = 1,      # x-coordinate of the first group
    xend = 2,   # x-coordinate of the second group
    y = 0.55,    # y-coordinate for the annotation
    label = "*" # Significance label
  )
plot1 <- plot + geom_segment(data = annotations, aes(x = x, xend = xend, y = y, yend = y), color = "black") +
  geom_text(data = annotations, aes(x = (x + xend) / 2, y = y, label = label), vjust = -0.5, size = 5) +
  ylim(0, 0.65)
plot1

plot2 <- ggscatter(Crayfish_feed_difference, 
                   x = "Carbon", 
                   y = "SGR", 
                   color = "Feed", 
                   title = "GLM: SGR ~ Carbon*", 
                   add = "reg.line", 
                   add.params = list(color = "black", fill = "lightgray"), 
                   conf.int = TRUE) +
  theme(legend.title = element_blank(),
        plot.title.position = "panel")
plot2

plot3 <- ggscatter(Crayfish_feed_difference, 
                   x = "Nitrogen", 
                   y = "SGR", 
                   color = "Feed",
                   title = "GLM: SGR ~ Nitrogen", 
                   add = "reg.line", 
                   add.params = list(color = "black", fill = "lightgray"), conf.int = TRUE) +
  theme(legend.title = element_blank(),
        plot.title.position = "panel")
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
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Fish survival", y = "Crayfish survival")
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


