# Load packages
library(tidyverse)
library(lme4)
library(MuMIn)

# Load the data
coral<-diss

#Check the structure of data
str(coral)
coral$Dead<- as.numeric(coral$Dead)  # Convert to numeric
coral$COTS_abund<- as.numeric(coral$COTS_abund)  # Convert to numeric

# subset data for plotting
data <- coral %>%
  mutate(across(Branched:Dead,))
view(data)

bleach.data <- data %>%
  dplyr::select(Reef_no, Visit, Dead, `Distance (meters)`, COTS_abund, Snorkel_code)
# Convert from numeric to factor for nesting in models
bleach.data$Reef_no <- as.factor(bleach.data$Reef_no)  
bleach.data$Snorkel_code <- as.factor(bleach.data$Snorkel_code)

bleach.data <- bleach.data %>%
  mutate(Dead_Percent = Dead * 10)  # Convert from decimal to integer

# Visualize bleached coral cover in tourist vs non tourist sites
damaged_coral<-ggplot(bleach.data, aes(x = Visit, y = Dead_Percent, fill = Visit)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +  # Boxplot with visible outliers
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +  # Adds mean as a black diamond
  scale_fill_manual(values = c("T" = "forestgreen", "NT" = "orange")) +  
  geom_jitter(width = 0.2, alpha = 0.6, color = "black") +
  labs(
    x = "Site Type",
    y = "Damaged Coral Cover (%)"
  ) +
  theme_classic(base_size = 14)
damaged_coral

ggsave(damaged_coral, filename = "damaged_coral.png")

## Statistical Analysis
# Plot histogram
hist(bleach.data$Dead_Percent)
# Q-Q plot
qqnorm(bleach.data$Dead_Percent)
qqline(bleach.data$Dead_Percent, col = "red")

# Shapiro-Wilk test for normality
shapiro.test(bleach.data$Dead_Percent)

table(bleach.data$Reef_no)   # Number of levels of Reef_no
table(bleach.data$Snorkel_code)   # Number of levels of Snorkel_code
table(bleach.data$Reef_no:bleach.data$Snorkel_code)   # Number of levels of Reef_no and Snorkel_code combined
# Re scale distance from shore
bleach.data$Distance_scaled <- scale(bleach.data$`Distance (meters)`)

## Data is normally distributed so I will run a linear model 
# Run multiple models with various effects
lm1<- lm(Dead_Percent ~ Visit * COTS_abund + Reef_no, data = bleach.data)
summary(lmer)

lmer1 <- lmer(Dead_Percent ~ Visit * COTS_abund + 
              (1 | Reef_no), 
            data = bleach.data)
summary(lmer1)
r.squaredGLMM(lmer1)  # get the R sq value

lmer2<- lmer(Dead_Percent ~ Visit * Distance_scaled + 
             (1 | Snorkel_code), data = bleach.data)
summary(lmer2)
r.squaredGLMM(lmer2)

lm2<- lm(Dead_Percent ~ Visit * Distance_scaled * COTS_abund, data = bleach.data)
summary(lm3)

lm3<- lm(Dead_Percent ~ Visit + Reef_no, data = bleach.data)
summary(lm4)

lm4<- lm(Dead_Percent ~ Visit, data = bleach.data)
summary(lm5)

# Check which model is best fit by comparing AIC values
AICc(lm1, lmer1, lmer2, lm2, lm3, lm4)  #  Visit status does not explain bleached coral cover as well as distance from shore

# Check assumptions for best fit model
shapiro.test(residuals(lmer1)) # normally distributed residuals 
plot(lmer1, which = 3)  # Residuals vs Fitted plot to check for homoschedasticity

###
# Test for significance of COTS abundance on damaged coral cover
cotslm <- lm(Dead_Percent ~ COTS_abund * Distance_scaled, data = bleach.data)
summary(cotslm)

cotslm2 <- lm(Dead_Percent ~ COTS_abund, data = bleach.data)
summary(cotslm)

AIC(cotslm, cotslm2)






