# Load required packages
library(dplyr)
library(vegan)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(ggrepel)

# Load the data
fish_species<- diss_fish

# Subset data for NMDS 
nmds.data <- diss_fish %>%
  dplyr::select(Visit, Snappers, Butterfly, Parrot, Jack, Surgeon, Rabbit, Angel, Bat, `Distance (meters)`, Reef_no, COTS_abund)

# Filter out non count data
fish_species<- nmds.data%>%
  dplyr::select(Snappers, Butterfly, Parrot, Jack, Surgeon, Rabbit, Angel, Bat)
view(fish_species)  # Check this is correct

# Compute Bray-Curtis Dissimilarity Matrix
species_matrix <- as.matrix(fish_species)  # Convert to matrix
diss_matrix <- vegdist(species_matrix, method = "bray")

# Run NMDS
set.seed(123)  # Ensure reproducibility
nmds_result <- metaMDS(diss_matrix, k = 2, trymax = 100)

# Create data frames for plotting NMDS results
nmds_scores <- as.data.frame(scores(nmds_result))  # Extract NMDS coordinates
nmds_scores$Visit <- nmds.data$Visit  # Add Visit status

# Fit environmental variables onto NMDS ordination
env_variables <- nmds.data %>%
  dplyr::select(`Distance (meters)`, COTS_abund, Reef_no)

env_fit <- envfit(nmds_result, env_variables, permutations = 999)  # 999 permutations for significance testing
print(env_fit)  # View significance of variables

# Distance from shore is the only significant environmental driver
# Extract scores for only significant environmental variables
env_vectors <- as.data.frame(scores(env_fit, display = "vectors"))  
env_vectors$variable <- rownames(env_vectors)  # Add row names as a column  

# Filter only the significant variables
env_vectors <- env_vectors %>%
  filter(variable %in% c("Distance (meters)"))  
print(env_vectors)  # Print to check

# Check if stress is lower, then NMDS is better fit with env variable
nmds_result$stress

##Visualisation
# Create function to compute convex hull for polygons
compute_hull <- function(df) {
  df[chull(df$NMDS1, df$NMDS2), ]  
}

# Compute convex hulls and preserve Visit info
hull_data <- nmds_scores %>%
  group_by(Visit) %>%
  group_split() %>%
  map_df(~ .x[chull(.x$NMDS1, .x$NMDS2), ])

# Plot the NMDS with polygons 
fish_nmds <- ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2)) +
  geom_polygon(
    data = hull_data,
    aes(fill = Visit, group = Visit, color = Visit),  # convex hulls outlined and colored by Visit
    alpha = 0.2,
    linewidth = 1
  ) +  
  geom_point(aes(color = Visit, shape = Visit), size = 3) +  # Points with color and shape by Visit
  geom_segment(data = as.data.frame(scores(envfit_result, display = "vectors")), 
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               arrow = arrow(length = unit(0.2, "inches")), color = "black") +
  geom_text_repel(data = as.data.frame(scores(envfit_result, display = "vectors")), 
                  aes(x = NMDS1 * 1.2, y = NMDS2 * 1.2, label = rownames(scores(envfit_result, display = "vectors"))), 
                  color = "black", size = 5) +
  scale_color_manual(values = c("T" = "forestgreen", "NT" = "orange")) +  # Custom colors for points
  scale_fill_manual(values = c("T" = "forestgreen", "NT" = "orange")) +  # Custom fill colors for hulls
  scale_shape_manual(values = c("T" = 17, "NT" = 4)) +  
  labs(title = "NMDS of Fish Assemblages with Environmental Vectors",
       x = "NMDS Axis 1",
       y = "NMDS Axis 2",
       color = "Visit Status",
       shape = "Visit Status") +
  theme_classic()

# View the plot
fish_nmds

# Save the plot to desktop
ggsave(fish_nmds, filename = "fish_nmds.png")

## Statistical Analysis
## Check that PERMANOVA is the correct statistical analysis
# Calculate Bray-Curtis dissimilarity
diss_matrix <- vegdist(species_matrix, method = "bray")

# Perform homogeneity of dispersion test (betadisper)
dispersion_test <- betadisper(diss_matrix, group = nmds.data$Visit)

# View the test result
print(dispersion_test)

# Check if the dispersion is significantly different between groups
anova(dispersion_test) # dispersion not significantly different - homoschedastic 

## Set up PERMANOVA
# Run PERMANOVA using Bray-Curtis dissimilarity
permanova_result <- adonis2(fish_species ~ Visit, data = nmds.data, method = "bray", 
                            permutations = 999)

# Print result
print(permanova_result)







