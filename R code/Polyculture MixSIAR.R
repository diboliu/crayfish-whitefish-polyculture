setwd("~/OneDrive/RPTU PolyRAS/Previous work unpublished/Revision")
library(MixSIAR)

mix <- load_mix_data(filename = "Consumers_Polyculture.csv", iso_names = c ("d13C" , "d15N"), factors = "Treatment", fac_random = FALSE, fac_nested = FALSE, cont_effects = NULL)

source <- load_source_data(filename = "Sources_Polyculture_SW.csv", source_factors = NULL, conc_dep = FALSE, data_type = "raw", mix)

discr <- load_discr_data("TEF_Polyculture_SW_literature.csv", mix)

g <- plot_data(filename = "crayfish_polyculture_SW_wheat", plot_save_pdf = FALSE, plot_save_png = TRUE, mix, source, discr, return_obj = TRUE)
library(ggplot2)
plotpub <- g + 
  theme(
    legend.background = element_blank(), # remove legend Background 
legend.title = element_blank(), # remove legend title 
legend.text = element_text(size = 9), # Adjust legend text size
plot.title = element_text(size = 14, face = "bold", hjust = 0.5), # Center and bold title
axis.title = element_text(size = 12, face = "bold"), # Axis titles
axis.text = element_text(size = 10), # Axis text
panel.grid.major = element_blank(), # remove Major grid lines
panel.grid.minor = element_blank(), # remove Minor grid lines
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
  labs(title = "C+A", x = "Proportion of Diet", y = "Scaled Posterior Density") +
  scale_color_manual(values = c("p.fac1.1" = "pink", "p.fac1.3" = "cyan"), 
                     labels = c("Solid Waste", "Wheat")) +
  scale_fill_manual(values = c("p.fac1.1" = "pink", "p.fac1.3" = "cyan"), 
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
  labs(title = "C+A_sep", x = "Proportion of Diet", y = "Scaled Posterior Density") +
  scale_color_manual(values = c("p.fac1.2" = "pink", "p.fac1.4" = "cyan"), 
                     labels = c("Solid Waste", "Wheat")) +
  scale_fill_manual(values = c("p.fac1.2" = "pink", "p.fac1.4" = "cyan"), 
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
combplot <- ggarrange(plotpub, plot2, plot3, labels = c("A", "B", "C"), ncol = 3)
ggsave("SIA.png", combplot, width = 15, height = 5, dpi = 300)
