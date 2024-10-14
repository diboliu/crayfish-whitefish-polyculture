setwd("~/OneDrive/RPTU PolyRAS/Previous work unpublished/Revision")
library(MixSIAR)

mix <- load_mix_data(filename = "Consumers_Polyculture.csv", iso_names = c ("d13C" , "d15N"), factors = "Treatment", fac_random = FALSE, fac_nested = FALSE, cont_effects = NULL)

source <- load_source_data(filename = "Sources_Polyculture_SW.csv", source_factors = NULL, conc_dep = FALSE, data_type = "raw", mix)

discr <- load_discr_data("TEF_Polyculture_SW_literature.csv", mix)

g <- plot_data(filename = "crayfish_polyculture_SW_wheat", plot_save_pdf = FALSE, plot_save_png = TRUE, mix, source, discr, return_obj = TRUE)
library(ggplot2)
library(scico)
g1 <- g + 
  scale_color_manual(values = c("#8C0172" , "#B2F2FD"), labels = c("nonseparated", "separated")) 
g1
plotpub <- g1  +
theme(legend.background = element_blank(), # remove legend Background 
legend.title = element_blank(),# remove legend title 
legend.text = element_text(size = 10, face = "bold"), # Adjust legend text size
plot.title = element_text(size = 14, face = "bold", hjust = 0.5), # Center and bold title
axis.title = element_text(size = 12, face = "bold"), # Axis titles
axis.text = element_text(size = 10), # Axis text
panel.grid.major = element_blank(), # remove Major grid lines
panel.grid.minor = element_blank(), # remove Minor grid lines
panel.border = element_blank(),
plot.background = element_rect(color = "white"), # white panel background
axis.line = element_line(color = "black"), # Axis lines
aspect.ratio = 1) 
plotpub

plot_prior(alpha.prior = 1, source, plot_save_pdf = FALSE, plot_save_png = TRUE, filename = "prior_plot_2diets_TEFlit")

write_JAGS_model(filename = "MixSIAR_Crayfish_2diets_TEFlit.txt", resid_err = TRUE, process_err = TRUE, mix, source)

jags.uninf <- run_model(run="normal", mix, source, discr, "MixSIAR_Crayfish_2diets_TEFlit.txt", alpha.prior = 1)

output_JAGS(jags.uninf, mix, source)

#customize posterior density plots
# Assuming you have already run your MixSIAR model and have the `jags.1` object
samples <- jags.uninf$BUGSoutput$sims.list
# Convert samples to data frame
samples_df <- as.data.frame(samples)
print(colnames(samples_df))
print(samples_df)
# Create a long format data frame for p.fac1.1 and p.fac1.3
samples_long <- data.frame(
  Value = c(samples_df$p.fac1.1, samples_df$p.fac1.3),
  Parameter = factor(rep(c("p.fac1.1", "p.fac1.3"), each = nrow(samples_df)))
)
library(ggpubr)
library(ggplot2)
# Create the overlapping density plot using ggdensity <- ggdensity(samples_long, 
plotac <- ggdensity(samples_long, 
          x = "Value", 
          y = "..scaled..", 
          add = "mean", 
          rug = TRUE, 
          color = "Parameter", 
          fill = "Parameter", 
          legend.title = "Diet",
          palette = "jco") +
  labs(title = "nonseparated", x = "Proportion of Diet", y = "Scaled Posterior Density") +
  scale_color_manual(values = c("p.fac1.1" = "#996330", "p.fac1.3" = "#7FC55F"), 
                     labels = c("Solid Waste", "Wheat")) +
  scale_fill_manual(values = c("p.fac1.1" = "#996330", "p.fac1.3" = "#7FC55F"), 
                    labels = c("Solid Waste", "Wheat")) +
  theme_minimal() 

plot2 <- plotac + 
  theme(
    legend.position = "inside", # Adjust these coordinates to move the legend
    legend.position.inside = c(0.9, 0.8), 
    legend.background = element_blank(), # remove legend Background 
    legend.title = element_text(size = 10, face = "bold"), # Adjust legend title size and make bold
    legend.text = element_text(size = 9), # Adjust legend text size
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5), # Center and bold title
    axis.title = element_text(size = 12, face = "bold"), # Axis titles
    axis.text = element_text(size = 10), # Axis text
    panel.grid.major = element_blank(), # remove Major grid lines
    panel.grid.minor = element_blank(), # remove Minor grid lines
    plot.background = element_rect(color = "white"), #white background
    axis.line = element_line(color = "black"), # Axis lines
    aspect.ratio = 1
  )
plot2

# Create a long format data frame for p.fac1.2 and p.fac1.4
samples_longsep <- data.frame(
  Value = c(samples_df$p.fac1.2, samples_df$p.fac1.4),
  Parameter = factor(rep(c("p.fac1.2", "p.fac1.4"), each = nrow(samples_df)))
)

# Create the overlapping density plot using ggdensity <- ggdensity(samples_long, 
plotacsep <- ggdensity(samples_longsep, 
                    x = "Value", 
                    y = "..scaled..", 
                    add = "mean", 
                    rug = TRUE, 
                    color = "Parameter", 
                    fill = "Parameter", 
                    legend.title = "Diet",
                    palette = "jco") +
  labs(title = "separated", x = "Proportion of Diet", y = "Scaled Posterior Density") +
  scale_color_manual(values = c("p.fac1.2" = "#996330", "p.fac1.4" = "#7FC55F"), 
                     labels = c("Solid Waste", "Wheat")) +
  scale_fill_manual(values = c("p.fac1.2" = "#996330", "p.fac1.4" = "#7FC55F"), 
                    labels = c("Solid Waste", "Wheat")) +
  theme_minimal() 

plot3 <- plotacsep + 
  theme(
    legend.position = "inside", # Adjust these coordinates to move the legend
    legend.position.inside = c(0.9, 0.8), 
    legend.background = element_blank(), # remove legend Background 
    legend.title = element_text(size = 10, face = "bold"), # Adjust legend title size and make bold
    legend.text = element_text(size = 9), # Adjust legend text size
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5), # Center and bold title
    axis.title = element_text(size = 12, face = "bold"), # Axis titles
    axis.text = element_text(size = 10), # Axis text
    panel.grid.major = element_blank(), # remove Major grid lines
    panel.grid.minor = element_blank(), # remove Minor grid lines
    plot.background = element_rect(color = "white"), # white panel background
    axis.line = element_line(color = "black"), # Axis lines
    aspect.ratio = 1
  )
plot3

library(patchwork)
combplot <- plotpub + plot2 + plot3 + plot_annotation(tag_levels = "A")
combplot
ggsave("SIA.png", combplot, width = 15, height = 6, unit="cm", dpi = 300, scale = 2.5)

#statistical comparison 
# nonseparated solid waste vs. wheat
# median and hdi
median_SW_nonsep <- median(samples_long$Value[samples_long$Parameter == "p.fac1.1"])
print(median_SW_nonsep)
hdi_SW_nonsep <- quantile(samples_long$Value[samples_long$Parameter == "p.fac1.1"], probs=c(.025,.975))
print(hdi_SW_nonsep)
median_W_nonsep <- median(samples_long$Value[samples_long$Parameter == "p.fac1.3"])
print(median_W_nonsep)
hdi_W_nonsep <- quantile(samples_long$Value[samples_long$Parameter == "p.fac1.3"], probs=c(.025,.975))
print(hdi_W_nonsep)

#probability wheat over solid waste when nonseparated
prob_nonsep <- mean(samples_long$Value[samples_long$Parameter == "p.fac1.3"] > samples_long$Value[samples_long$Parameter == "p.fac1.1"])
print(prob_nonsep)                                                  

diff_nonsep <- samples_long$Value[samples_long$Parameter == "p.fac1.3"] - samples_long$Value[samples_long$Parameter == "p.fac1.1"]
diff_interval_nonsep <- quantile(diff, probs = c(0.025, 0.975))
print(diff_interval_nonsep)

#bayes factor
install.packages("BayesFactor")
library(BayesFactor)
bf_ttest_nonsep <- ttestBF(x = samples_long$Value[samples_long$Parameter == "p.fac1.3"], 
                    y = samples_long$Value[samples_long$Parameter == "p.fac1.1"], 
                    paired = FALSE)
summary(bf_ttest_nonsep)
#separated solid waste vs wheat
# median and hdi
median_SW_sep <- median(samples_longsep$Value[samples_longsep$Parameter == "p.fac1.2"])
print(median_SW_sep)
hdi_SW_sep <- quantile(samples_longsep$Value[samples_longsep$Parameter == "p.fac1.2"], probs=c(.025,.975))
print(hdi_SW_sep)
median_W_sep <- median(samples_longsep$Value[samples_longsep$Parameter == "p.fac1.4"])
print(median_W_sep)
hdi_W_sep <- quantile(samples_longsep$Value[samples_longsep$Parameter == "p.fac1.4"], probs=c(.025,.975))
print(hdi_W_sep)

#probability wheat over solid waste
probsep <- mean(samples_longsep$Value[samples_longsep$Parameter == "p.fac1.4"] > samples_longsep$Value[samples_longsep$Parameter == "p.fac1.2"])
print(probsep)
diffsep <- samples_longsep$Value[samples_longsep$Parameter == "p.fac1.4"] - samples_longsep$Value[samples_longsep$Parameter == "p.fac1.2"]
diffsep_interval <- quantile(diffsep, probs = c(0.025, 0.975))
print(diffsep_interval)

#bayes factor
bf_ttest_sep <- ttestBF(x = samples_longsep$Value[samples_longsep$Parameter == "p.fac1.4"], 
                    y = samples_longsep$Value[samples_longsep$Parameter == "p.fac1.2"], 
                    paired = FALSE,
                  )
summary(bf_ttest_sep)

#compare the nutritional contribution of solid waste between nonseparate and separated
prob_SW_between <- mean(samples_longsep$Value[samples_longsep$Parameter == "p.fac1.2"] > samples_long$Value[samples_long$Parameter == "p.fac1.1"])
print(prob_SW_between)
diff_SW_between <- samples_longsep$Value[samples_longsep$Parameter == "p.fac1.2"] - samples_long$Value[samples_long$Parameter == "p.fac1.1"]
diff_SW_between_interval <- quantile(diff_SW_between, probs = c (0.025, 0.975))
print(diff_SW_between_interval)

#bayes factor
bf_ttest_SW_between <- ttestBF(x = samples_longsep$Value[samples_longsep$Parameter == "p.fac1.2"],
                               y = samples_long$Value[samples_long$Parameter == "p.fac1.1"],
                               paired = FALSE
)
summary(bf_ttest_SW_between)

