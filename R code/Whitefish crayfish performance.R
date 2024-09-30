setwd("~/OneDrive/RPTU PolyRAS/Previous work unpublished/Revision/Data processing")
library(ggplot2)
library(readxl)
library(ggpubr)
library(readr)
library(tidyverse)

# Experiment 1
Crayfish_feed_difference <- read_excel("Crayfish feed difference.xlsx")
View(Crayfish_feed_difference)  

#normality test
shapiro.test(Crayfish_feed_difference$Survival)
shapiro.test(Crayfish_feed_difference$SGR)

# Test the effect of feed on individual survival (live=1, dead=0, individual crayfish as observation)
Crayfish_feed_difference_individual_mort <- read_excel("Crayfish feed difference individual mortality.xlsx")
View(Crayfish_feed_difference_individual_mort)  
Crayfish_feed_difference_individual_mort$Feed <- as.factor(Crayfish_feed_difference_individual_mort$Feed)
library(lme4)
Crayfish_feed_difference$Feed <- as.factor(Crayfish_feed_difference$Feed)
glmm_ind_sur <- glmer(live ~ Feed + (1 | Tank), data = Crayfish_feed_difference_individual_mort, family = binomial(link = "logit"))
summary(glmm_ind_sur)
Anova(glmm_ind_sur)


#test the effect of feed on SGR (aquarium as observation)
# Fit Gamma distribution to the data
install.packages("MASS")
library(MASS)
fit <- fitdistr(Crayfish_feed_difference$SGR, "gamma")
print(fit)

shape_param <- fit$estimate["shape"]
rate_param <- fit$estimate["rate"]

# Kolmogorov-Smirnov test
ks_test_result <- ks.test(Crayfish_feed_difference$SGR, "pgamma", shape = shape_param, rate = rate_param)
print(ks_test_result)


Crayfish_feed_difference$Feed <- as.factor(Crayfish_feed_difference$Feed)
glm_SGR <- glm(SGR ~ Feed, data = Crayfish_feed_difference, family = Gamma (link = "log"))
summary(glm_SGR)
Anova(glm_SGR)

library(multcomp)

# Fit the post-hoc test with Tukey's adjustment
posthoc_feed <- glht(glm_SGR, linfct = mcp(Feed = "Tukey"))

# Summary of the post-hoc comparisons
summary(posthoc_feed)

# plot SGR vs feed
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
set_palette(survival, scico(4, palette = "berlin"))
Crayfish_feed_difference$Feed <- factor(Crayfish_feed_difference$Feed, levels = c("Fish Faeces", "Pellet", "Wheat", "Wheat+Fish Faeces"))
plot <- ggboxplot(
  Crayfish_feed_difference, 
  x = "Feed", 
  y = "SGR",
  ylab = "Crayfish SGR",
  color = "Feed",
  add = "jitter",
  add.params = list(size = 3, alpha = 0.7)) +
  theme(legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
  ) +
  ylim(0,0.6) +
 geom_signif(comparisons = list(c("Fish Faeces", "Pellet")), 
                annotations = "*", 
                y_position = 0.55,  # Adjust based on your data
                tip_length = 0.05,
                size = 0.5)
  
plot
library(scico)
pubplot <- set_palette(plot, scico(4, palette = "berlin"))
pubplot

ggsave("experiment1 revised.png", pubplot, width = 6, height = 5, dpi = 300, units = "cm", scale = 2.4)

#Experiment 2
mono_vs_poly <- read_excel("mono vs poly.xlsx")
View(mono_vs_poly)

# normality test
shapiro.test(mono_vs_poly$Fish_SGR)
shapiro.test(mono_vs_poly$Fish_Survival)

# equal variance test
leveneTest(Fish_SGR ~ Treat_Fish, data = mono_vs_poly)
leveneTest(Fish_Survival ~ Treat_Fish, data = mono_vs_poly)

# glm analysis of fish SGR
mono_vs_poly$Treat_Fish <- as.factor(mono_vs_poly$Treat_Fish)
glm_fish_poly <- glm(Fish_SGR ~ Treat_Fish, data = mono_vs_poly)
summary(glm_fish_poly)
Anova(glm_fish_poly)
posthoc_poly <- glht(glm_fish_poly, linfct = mcp(Treat_Fish = "Tukey"))
summary(posthoc_poly)

fishgrowth <- ggboxplot(data = mono_vs_poly, 
                        x = "Treat_Fish", 
                        y = "Fish_SGR", 
                        xlab = "Treatment",
                        ylab = "Whitefish SGR",
                        color = "Treat_Fish",
                        add = "jitter",
                        add.params = list(fill = "Treat_Fish", size = 3, alpha = 0.7)) +
  theme(legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
                        ylim(0, 1.3) 
sigplot <- fishgrowth + geom_signif(comparisons = list(c("monoculture", "separated")), 
                         annotations = "*", 
                         y_position = 1.25,  # Adjust based on your data
                         tip_length = 0.1,
                        size = 0.5)
sigplot
library(scico)
pubplot <- set_palette(sigplot, scico(3, palette = "roma"))
pubplot
ggsave("whitefish SGR polyculture.png", pubplot, width = 6, height = 5, dpi = 300, units = "cm", scale = 2.4)

#glmm model individual fish survival predicted by treatment
mono_vs_poly_ind_sur <- read_excel("mono vs poly individual survival.xlsx")
View(mono_vs_poly_ind_sur)

library(lme4)
mono_vs_poly_ind_sur$Treat_Fish <- as.factor(mono_vs_poly_ind_sur$Treat_Fish)
glmm_fish_sur <- glmer(Live_Fish ~ Treat_Fish + (1 | Tank_Fish), data = mono_vs_poly_ind_sur, family = binomial)
# glmer failed due to all 1s in two treatments, data were discussed in descriptive way

ggboxplot(mono_vs_poly_ind_sur,
          x = "Treat_Fish",
          y = "Live_Fish",
          fill = "Treat_Fish",
          add = "jitter",
          color = "Treat_Fish",
          add.params = list(size = 3, alpha = 0.7))

# glmer analysis of individual crayfish survival
crayfish_ind_sur$Treat_Crayfish <- as.factor(crayfish_ind_sur$Treat_Crayfish)
glmm_ind_crayfish_sur <- glmer(Live_Crayfish ~ Treat_Crayfish + (1 | Tank_Crayfish) , data = crayfish_ind_sur,  family = binomial(link = "logit"))
summary(glmm_ind_crayfish_sur)
Anova(glmm_ind_crayfish_sur)

# crayfish growth
separated_vs_nonseparated <- read_excel("separated vs nonseparated.xlsx")
View(separated_vs_nonseparated)
#normality
shapiro.test(separated_vs_nonseparated$Crayfish_SGR)

glm_crayfish_SGR_poly <- glm(Crayfish_SGR ~ Treat_Crayfish, data = separated_vs_nonseparated)
summary(glm_crayfish_SGR_poly)
Anova(glm_crayfish_SGR_poly)

ggboxplot(separated_vs_nonseparated,
          x = "Treat_Crayfish",
          y = "Crayfish_SGR",
          fill = "Treat_Crayfish",
                  add = "jitter") +
  ylim(0,0.75)
# crayfish survival
crayfish_ind_sur <- read_excel("mono vs poly individual crayfish survival.xlsx")
View(crayfish_ind_sur)

glmm_crayfish_ind_sur <- glmer(Live_Crayfish ~ Treat_Crayfish + (1 | Tank_Crayfish), data = crayfish_ind_sur, family = binomial(link = "logit"))
summary(glmm_crayfish_ind_sur)
Anova(glmm_crayfish_ind_sur)

ggboxplot(separated_vs_nonseparated,
          x = "Treat_Crayfish",
          y = "Crayfish_survival",
          fill = "Treat_Crayfish",
          add = "jitter") +
  ylim(0,1)


