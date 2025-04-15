# Load required packages
library(dplyr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ISLR)
library(MuMIn)

# Summarize total abundance for each species across Tourist and Non-Tourist sites
total_abundance <- diss_fish %>%
  group_by(Visit) %>%  # Group by TouristStatus (Tourist or Non-Tourist)
  summarise(
    Snappers_total = sum(Snappers, na.rm = TRUE),
    Butterfly_total = sum(Butterfly, na.rm = TRUE),
    Parrot_total = sum(Parrot, na.rm = TRUE),
    Jack_total = sum(Jack, na.rm = TRUE),
    Surgeon_total = sum(Surgeon, na.rm = TRUE),
    Rabbit_total = sum(Rabbit, na.rm = TRUE),
    Angel_total = sum(Angel, na.rm = TRUE),
    Bat_total = sum(Bat, na.rm = TRUE),
    Other_total = sum(Other, na.rm = TRUE),
    .groups = "drop",
    mean_abundance = mean(Abundance),
    sd_abundance = sd(Abundance),
    se_abundance = sd_abundance / sqrt(n())
  )
print(total_abundance)

# Pivot the raw data into long format
fish_long <- diss_fish %>%
  pivot_longer(
    cols = c(Snappers, Butterfly, Parrot, Jack, Surgeon, Rabbit, Angel, Bat),
    names_to = "Species",
    values_to = "Abundance"
  )
view(fish_long)

# Calculate total abundance per species at each Visit 
summary_data <- fish_long %>%
  group_by(Visit, Species) %>%
  summarise(
    total_abundance = sum(Abundance, na.rm = TRUE),  # Calculate total abundance
    mean_abundance = mean(Abundance, na.rm = TRUE),  # Calculate mean abundance 
    sd_abundance = sd(Abundance, na.rm = TRUE),      # Calculate SD
    n = n(),
    se_abundance = sd_abundance / sqrt(n),           # Calculate SE
    .groups = "drop"
  )

# Check summary data is correct
print(summary_data)

# Plot total abundances for each family with error bars
fish_bar <- ggplot(summary_data, aes(x = Species, y = total_abundance, fill = Visit)) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.8),  # Spacing between bars
    width = 0.6  # Bar width
  ) + geom_errorbar(
    aes(
      ymin = total_abundance - se_abundance,
      ymax = total_abundance + se_abundance
    ),
    width = 0.2,
    position = position_dodge(width = 0.8)  # Match dodge with bars
  ) + theme_classic() +
    scale_y_continuous(
    breaks = seq(0, ceiling(max(summary_data$total_abundance, na.rm = TRUE) / 20) * 20, by = 20),  # y-axis breaks in increments of 20
    labels = scales::comma_format(),  # Format y-axis labels with commas
    expand = c(0, 0)  # Remove extra space between bars and x-axis
  ) +
  scale_fill_manual(values = c("T" = "forestgreen", "NT" = "orange")) +
  labs(
    x = "Fish Family",
    y = "Abundance"
  ) +
  theme(
    axis.ticks.y = element_line(),  # Ensure y-axis ticks are visible
    axis.title.y = element_text(size = 12),  # Improve y-axis title readability
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),  # Ensure x-axis labels are black
    axis.text.y = element_text(size = 10, color = "black"),  # Ensure y-axis labels are black
  ) +
  coord_cartesian(ylim = c(0, max(summary_data$total_abundance, na.rm = TRUE) + 20))  # Adjust y-axis range

# View the plot
fish_bar

# Save the plot to desktop
ggsave(fish_bar, filename = "fish_bar.png")

## Statistical analysis 
# Normality test for each species and Site type
shapiro_results <- fish_long %>%
  group_by(Species, Visit) %>%
  summarise(
    shapiro_test = list(shapiro.test(Abundance)),
    .groups = 'drop'
  )

# Extract the p-values
shapiro_results <- shapiro_results %>%
  mutate(
    p_value = sapply(shapiro_test, function(x) x$p.value)  # Extract p-value from test results
  )

# View the results
print(shapiro_results)  # Not normally distributed

## Now we will use NB GLM to account for more model parameters
glm_nb_model <- glm.nb(Abundance ~ Visit, data = fish_long)
summary(glm_nb_model)
# Pseudo R2
with(summary(glm_nb_model), 1 - deviance/null.deviance)

glm_nb_2 <- glm.nb(Abundance ~ Visit * `Distance (meters)`, data = fish_long)
summary(glm_nb_2)
# Pseudo R2
with(summary(glm_nb_2), 1 - deviance/null.deviance)

glm_nb_3 <- glm.nb(Abundance ~ Visit * `Distance (meters)` * Reef_complex, data = fish_long)
summary(glm_nb_3)
# Pseudo R2
with(summary(glm_nb3), 1 - deviance/null.deviance)

glm_nb_4 <- glm.nb(Abundance ~ Visit * Reef_complex, data = fish_long)
summary(glm_nb_4)
# Pseudo R2
with(summary(glm_nb4), 1 - deviance/null.deviance)

AICc(glm_nb_model, glm_nb_2, glm_nb_3, glm_nb_4)  

# Check overdispersion
deviance(glm_nb_2) / df.residual(glm_nb_2) # model does account for overdispersion

# Plot the residuals
plot(glm_nb2$residuals)

# Predicted fish abundance based on the model
fish_long$predicted_abundance <- predict(glm_nb2, type = "response")

# Plot observed vs predicted abundance
ggplot(fish_long, aes(x = Abundance, y = predicted_abundance)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +  
  theme_minimal() +
  labs(
    title = "Observed vs Predicted Fish Abundance",
    x = "Observed Abundance",
    y = "Predicted Abundance"
  )



