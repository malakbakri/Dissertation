#Load required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS)
library(ISLR)
library(MuMIn)

# Load the correct file
species_data <- diss_fish

# Pivot the raw data into long format
fish_long <- diss_fish %>%
  pivot_longer(
    cols = c(Snappers, Butterfly, Parrot, Jack, Surgeon, Rabbit, Angel, Bat),
    names_to = "Species",
    values_to = "Abundance"
  )
view(fish_long)

# Convert all species abundance columns to numeric, handling non-numeric entries
species_data[ , c("Snappers", "Butterfly", "Parrot", "Jack", "Surgeon", "Rabbit", "Angel", "Bat")] <- 
  lapply(species_data[ , c("Snappers", "Butterfly", "Parrot", "Jack", "Surgeon", "Rabbit", "Angel", "Bat")], 
         function(x) as.numeric(as.character(x)))

# Ensure data strings are all correct, if not adjust them
str(species_data)
species_data$Visit<- as.factor(species_data$Visit)
species_data$Reef_complex<- as.factor(species_data$Reef_complex)

# Select only species abundance columns and Visit
species_abundance <- species_data %>%
  dplyr::select(Visit, Snappers, Butterfly, Parrot, Jack, Surgeon, Rabbit, Angel, Bat)
view(species_abundance)

# Summarize total abundance per species for tourist and non-tourist sites
total_abundance <- species_abundance %>%
  group_by(Visit) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))  # Sum across numeric columns
view(total_abundance)

## Statistical analysis
# Check normality of residuals
hist(residuals(lm(Abundance ~ Visit, data = fish_long))) # Right skewed - poisson

# Pivot data into long format for modelling
long_data_extra <- species_data %>%
  dplyr::select(Visit, Snappers, Butterfly, Parrot, Jack, Surgeon, Rabbit, Angel, Bat, Reef_complex, COTS_abund, `Distance (meters)`) %>%
  pivot_longer(cols = Snappers:Bat, 
               names_to = "Species", 
               values_to = "Abundance")
view(long_data_extra)

# Check the fit of poisson distribution model
poisson_model <- glm(Abundance ~ Visit + Reef_complex, data = long_data_extra, family = poisson)
summary(poisson_model)
# Pseudo R2
with(summary(poisson_model), 1 - deviance/null.deviance)

overdispersion_ratio <- poisson_model$deviance / poisson_model$df.residual
print(overdispersion_ratio) # dispersion >1 

poisson_model2 <- glm(Abundance ~ Visit * Reef_complex, data = long_data_extra, family = poisson)
summary(poisson_model2)
# Pseudo R2
with(summary(poisson_model2), 1 - deviance/null.deviance)

overdispersion_ratio2 <- poisson_model2$deviance / poisson_model2$df.residual
print(overdispersion_ratio2) # dispersion >1 lets go for negative binomial glm

poisson_model3 <- glm(Abundance ~ Visit * `Distance (meters)`, data = long_data_extra, family = poisson)
summary(poisson_model3)
# Pseudo R2
with(summary(poisson_model3), 1 - deviance/null.deviance)

overdispersion_ratio3 <- poisson_model3$deviance / poisson_model3$df.residual
print(overdispersion_ratio3) # dispersion >1 lets go for negative binomial glm

# Fit the Negative Binomial GLM (accounts for over dispersion)
nb_glm <- glm.nb(Abundance ~ Visit + Reef_complex, data = long_data_extra)
summary(nb_glm)  # Summary of the model
# Pseudo R2
with(summary(nb_glm), 1 - deviance/null.deviance)

# Check over dispersion
deviance(nb_glm) / df.residual(nb_glm) # model does account for over dispersion

# Run different versions of model to see best fit
nb_glm2 <- glm.nb(Abundance ~ Visit * Reef_complex, data = long_data_extra)
summary(nb_glm2)  # Summary of the model
# Pseudo R2
with(summary(nb_glm2), 1 - deviance/null.deviance)

# Check over dispersion
deviance(nb_glm2) / df.residual(nb_glm2) 

nb_glm3 <- glm.nb(Abundance ~ Visit * `Distance (meters)`, data = long_data_extra)
summary(nb_glm3)  # Summary of the model
# Pseudo R2
with(summary(nb_glm3), 1 - deviance/null.deviance)

# Check over dispersion
deviance(nb_glm3) / df.residual(nb_glm3)

# Compare AIC as a fit metric to select best model
AICc(nb_glm, nb_glm2, nb_glm3, poisson_model, poisson_model2, poisson_model3)

# Plot the residuals
plot(nb_glm3$residuals)

# Add predicted values to the original data
fish_long$predicted_abundance <- predict(nb_glm3, type = "response")

# Aggregate by Site and Visit to match the box plot structure
predicted_df <- fish_long %>%
  group_by(Reef_no, Visit) %>%  
  summarise(predicted_abundance = sum(predicted_abundance), .groups = "drop")

total_abundance_summary <- fish_long %>%
  group_by(Visit, Reef_no) %>%  
  summarise(total_abundance = sum(Abundance, na.rm = TRUE), .groups = "drop")

# Make the plot
totalfish_box <- ggplot(total_abundance_summary, aes(x = Visit, y = total_abundance, fill = Visit)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_point(data = predicted_df, aes(x = Visit, y = predicted_abundance),
             shape = 21, size = 3, fill = "red", color = "black") +
  scale_fill_manual(values = c("T" = "forestgreen", "NT" = "orange")) +
  theme_classic() +
  labs(
    x = "Tourist Status",
    y = "Total Fish Abundance"
  )

# View the plot
totalfish_box

# Save the plot
ggsave(totalfish_box, filename = "fish_box.png")

