# Load required packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ISLR)
library(MuMIn)

# Load the data
coral_data <- diss
head(coral_data)  # Look at the start of the data 

coral.cots <- coral_data %>%
  dplyr::select(Visit, COTS_abund, `Distance (meters)`, Reef_no)
coral.cots$Visit <- as.factor(coral.cots$Visit)  # turn Visit column into factor
view(coral.cots)

# Rescale Distance for visualisation
coral.cots$Distance_m_scaled <- as.numeric(scale(coral.cots$`Distance (meters)`))

## Fit a Poisson GLM
poisson_model<- glm(COTS_abund ~ Visit, 
                    family = poisson(), data = coral.cots)
summary(poisson_model)

# Calculate the over dispersion statistic
deviance <- poisson_model$deviance
df_residual <- poisson_model$df.residual
overdispersion_statistic <- deviance / df_residual

# Print the dispersion statistic
overdispersion_statistic  # 0.92 is slightly underdispersed but reasonable fit

# Fit another poisson model with random effect
poisson_model2 <- glm(COTS_abund ~ Visit + Distance_m_scaled, 
                      family = poisson(), 
                      data = coral.cots)
summary(poisson_model2)

# Calculate the over dispersion statistic
deviance2 <- poisson_model2$deviance
df_residual2 <- poisson_model2$df.residual
overdispersion_statistic2 <- deviance2 / df_residual2

# Print the dispersion statistic
overdispersion_statistic2  # 0.76 more underdispersed

# Fit the model without random effects
poisson_model3 <- glm(COTS_abund ~ Visit * Distance_m_scaled, 
                           family = poisson, 
                           data = coral.cots)
summary(poisson_model3)

deviance3 <- poisson_model3$deviance
df_residual3 <- poisson_model3$df.residual
overdispersion_statistic3 <- deviance2 / df_residual2

# Compare AIC for best fit model
AICc(poisson_model, poisson_model2, poisson_model3)
vuong(poisson_model, poisson_model2, poisson_model3)  # Lets use poisson2

# Calculate goodness of fit with pseudo r2
with(summary(poisson_model2), 1 - deviance/null.deviance) # pseudo r2 value < 0.40 so model is not very well fit to data
with(summary(poisson_model), 1 - deviance/null.deviance)


##Visualisation
#Plot predicted values of cots abundance against distance scaled for T/NT sites
# Get predicted values across a range of scaled distances for each Visit group
pred <- ggpredict(poisson_model2, terms = c("Distance_m_scaled", "Visit"))
head(pred)

# Create distance sequence
distance_seq <- seq(from = min(coral.cots$Distance_m_scaled),
                    to = max(coral.cots$Distance_m_scaled),
                    length.out = 100)

# Create prediction grid for both Visit levels
newdata <- expand.grid(
  Distance_m_scaled = distance_seq,
  Visit = c("T", "NT")  
)

# Add predictions and confidence intervals
newdata$predicted <- predict(poisson_model2, newdata = newdata, type = "response")

# Assign confidence intervals
pred_link <- predict(poisson_model2, newdata = newdata, se.fit = TRUE, type = "link")
newdata$conf.low <- exp(pred_link$fit - 1.96 * pred_link$se.fit)
newdata$conf.high <- exp(pred_link$fit + 1.96 * pred_link$se.fit)

# Make sure the y-axis range matches the predicted values
y_min <- min(newdata$predicted, na.rm = TRUE)
y_max <- max(newdata$predicted, na.rm = TRUE)
x_min<- min(newdata$predicted, na.rm = TRUE)
x_max <- max(newdata$predicted, na.rm = TRUE)

# Make the plot
cots.plot<- ggplot(newdata, aes(x = Distance_m_scaled, y = predicted, color = Visit, fill = Visit)) +
  geom_line(size = 0.9) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  scale_color_manual(values = c("NT" = "orange", "T" = "forestgreen")) +
  scale_fill_manual(values = c("NT" = "orange", "T" = "forestgreen")) +
  coord_cartesian(xlim = c(-1, 2.6)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-1, 2.6, by = 0.5)) +  # Set x-axis increments
  labs(
    x = "Distance to shore (meters scaled)",
    y = "Predicted COTS Abundance",
    color = "Site Type",
    fill = "Site Type"
  ) + theme_classic()

# View the plot
cots.plot

# Save the plot
ggsave(cots.plot, filename = "cots.plot.png")
