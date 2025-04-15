# Relative coral cover stacked bar plot and statistical analysis
library(tidyverse)
library(gamlss)
library(dplyr)
library(ggnewscale)
library(ggplot2)
library(RColorBrewer)
library(MuMIn)

# Load the data set
coral<-diss

##Data cleaning
# Check for invalid values in the response variable - should return FALSE
any(is.na(coral_data$Visit))  
any(is.nan(coral_data$Visit)) 
any(is.infinite(coral_data$Visit)) 
view(coral)

# Reshape data frame for analysis
data <- coral %>%
  dplyr::mutate(across(Reef_no:`Distance (meters)`)) %>%
  dplyr::rename(`Free Living` = Free_L)

data<- data %>%
  dplyr::mutate(across(Branched:Dead))  # Subset analysis data
view(data)

long_data <- data %>%
  pivot_longer(
    cols = Branched:Dead,
    names_to = "Coral Type",
    values_to = "Percentage Cover"
    ) %>%
  mutate(`Percentage Cover` = `Percentage Cover` * 10)  

view(long_data) # View the reshaped data

# Plot a stacked bar
stacked_bar<- ggplot(long_data, aes(x = Visit, y = `Percentage Cover`, fill = `Coral Type`))+
  geom_bar(stat = "identity", position = "fill") + 
  labs(
    x = "Site Type",
    y = "Percentage Cover (%)", 
    fill = "Coral Type"
  ) +
  scale_y_continuous(labels = function(x) paste0(x * 100),
                     expand = c(0, 0)) +
  theme_classic() +
scale_fill_brewer(palette = "BuGn")

# View the plot
stacked_bar 

# Save the plot to desktop
ggsave(stacked_bar, file = "stacked_bar.pdf")

##Statistical Analysis
# Histogram to look at spread of relative cover data
percent_cov <- long_data$`Percentage Cover`
hist(long_data$`Percentage Cover`) # reveals relative percent cover is very right skewed # check for normality of data
shapiro.test(long_data$`Percentage Cover`) # Not normally distributed

# Re-scale data to make it into decimal form
long_data$`Percentage Cover` <- long_data$`Percentage Cover` / 10
view(long_data$`Percentage Cover`)

# Zero-one inflated beta regression - accounts for non-normal distribution and zero inflation
str(long_data)  # Check the levels in each column
sum(is.na(long_data$Visit)) # Check if there are NA values
levels(long_data$Visit)  # Check the levels of the 'Visit' factor variable

# Convert variables to factors
long_data$Visit <- as.factor(long_data$Visit)
long_data$`Coral Type`<- as.factor(long_data$`Coral Type`)
long_data$reef_site <- as.factor(long_data$Reef_no)  # Convert from numeric to factor for nesting in models
long_data$snorkel_code <- as.factor(long_data$Snorkel_code)

summary(long_data$`Percentage Cover`)  # See summary of data
table(long_data$`Percentage Cover` == 0)  # Return number of zeros in data frame
table(long_data$`Percentage Cover` == 1)  # Return number of ones in data frame 

## Since there are many zeros and ones, we will use a zero inflated beta regression model
# Create data frame to fit ZOIB model
zib_data <- long_data %>%
  dplyr::select(`Percentage Cover`, Reef_no, Snorkel_code, Visit, Reef_complex, `Coral Type`, `Distance (meters)`, COTS_abund)
view(zib_data)  # Check if new data frame looks correct

# Ensure values are between 0 and 1 (adjust scale before fitting model)
zib_data$`Percentage Cover`[zib_data$`Percentage Cover` > 1] <- 
  zib_data$`Percentage Cover`[zib_data$`Percentage Cover` > 1] / 10
# Should return numeric(0) - meaning no values >1
zib_data$`Percentage Cover`[zib_data$`Percentage Cover` > 1]

# Convert from numeric to factor for nesting in models
zib_data$Reef_no <- as.factor(zib_data$Reef_no)  
zib_data$Snorkel_code <- as.factor(zib_data$Snorkel_code)

# Create an object as an interaction term
zib_data$reef_transect <- interaction(zib_data$Reef_no, zib_data$Snorkel_code)

zib_model1 <- gamlss(`Percentage Cover` ~ Visit +  random(Reef_no) + random(reef_transect), 
                     family = BEINF, data = zib_data)
summary(zib_model1)  # Check summary
plot(zib_model1)  # Look at the assessment plots

# Repeat this process for all the models
zib_model2 <- gamlss(`Percentage Cover` ~ Visit * 
                       random(Reef_no) + random(reef_transect), family = BEINF, data = zib_data)
summary(zib_model2)
plot(zib_model2)

zib_model3 <- gamlss(`Percentage Cover` ~ Visit * `Distance (meters)` + 
                       random(Reef_no) + random(reef_transect) , family = BEINF, data = zib_data)
summary(zib_model3)
plot(zib_model3)

zib_model4 <- gamlss(`Percentage Cover` ~ Visit * `Distance (meters)` * 
                       random(Reef_no) + random(reef_transect), family = BEINF, data = zib_data)
summary(zib_model4)


zib_model5<- gamlss(`Percentage Cover` ~ `Distance (meters)`* 
                      COTS_abund * random(Reef_no) + random(reef_transect), family = BEINF, data = zib_data)
summary(zib_model5)

# Compare AIC as a fit metric
AICc(zib_model1, zib_model2, zib_model3, zib_model4, zib_model5)

