
# Load required packages
library(vegan)
library(dplyr)
library(ggplot2)
library(purrr)
library(fmsb)

# NMDS for coral growth form relative percent cover
coral<-diss
view(coral)

# Subset data 
reef <- coral %>%
  mutate(across(Reef_no:`Distance (meters)`))
view(reef)

spiderdata<- reef %>% 
  dplyr::select(Reef_no, Visit, Branched, Foliose, Tabular, Boulder,
                             Encrusting, Free_L, Dead)

# Ensure categories are in the correct format
spiderdata$Visit <- as.factor(spiderdata$Visit)
spiderdata$Dead<- as.numeric(spiderdata$Dead)
view(spiderdata)

# Check structure
str(spiderdata)

# Check for missing values
sum(is.na(spiderdata))

# Check if any rows are completely zero
sum(rowSums(spiderdata[, -c(1,2)]) == 0)  # Should return 0

# Check dimensions
dim(spiderdata)

# Keep only metadata 
reef_metadata <- spiderdata[, c(1,2)]  
# Add Distance from shore and Reef complexity to meta data
reef_metadata$Distance <- coral$`Distance (meters)` 
reef_metadata$Reef_Complexity <- as.factor(coral$Reef_complex)
view(reef_metadata)

# Ensure coral cover columns are numeric
spiderdata[, -c(1,2)] <- lapply(spiderdata[, -c(1,2)], as.numeric)

# Apply Hellinger transformation to coral cover data
reef_data_transformed <- decostand(spiderdata[, -c(1,2)], method = "hellinger")

# Recombine metadata with transformed coral data
reef_data_final <- cbind(reef_metadata, reef_data_transformed)

# Check the structure is correct
str(reef_data_final)

## Run NMDS
# Select only transformed coral data (excluding metadata)
coral_data_for_nmds <- reef_data_final[, -c(1,2,3,4)]  # Remove Reef_no and Visit
set.seed(123)  # Ensure reproducibility
nmds <- metaMDS(coral_data_for_nmds, distance = "bray", k = 2, trymax = 200)  # Run the model
nmds # Check NMDS results

# Fit environmental variables (Distance and Reef Complexity) to the NMDS
envfit_result <- envfit(nmds, reef_metadata[, c("Reef_Complexity", "Distance")], perm = 999)
summary(envfit_result)

# Extract vectors - environmental variables fitted to the NMDS axes
vectors <- scores(envfit_result, display = "vectors")
# View the fitted environmental vectors
print(vectors)  # Only distance seems to drive some change in NMDS

# Extract NMDS scores 
nmds_scores <- as.data.frame(scores(nmds, display = "sites"))

# Add metadata back to the NMDS scores for coloring
nmds_scores$Visit <- reef_metadata$Visit
nmds_scores$Reef_no <- reef_metadata$Reef_no
nmds_scores$Visit <- reef_metadata$Visit
nmds_scores$Distance <- reef_metadata$Distance
nmds_scores$Reef_Complexity <- reef_metadata$Reef_Complexity

## For the polygon groupings
# Function to compute convex hull and retain Visit info
compute_hull <- function(df, group) {
  df[chull(df$NMDS1, df$NMDS2), ] %>%
    mutate(Visit = unique(group))
}

# Compute convex hulls and preserve Visit info
hull_data <- nmds_scores %>%
  group_by(Visit) %>%
  group_split() %>%
  map_df(~ .x[chull(.x$NMDS1, .x$NMDS2), ])

## Plot NMDS with polygons around T/NT sites and arrows for environmental vectors
nmds_coral<- ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2)) +
  geom_polygon(
    data = hull_data,
    aes(fill = Visit, group = Visit, color = Visit),
    alpha = 0.2,
    linewidth = 1
  ) +
  geom_point(aes(color = Visit, shape = Visit), size = 3) +
  labs(
    title = "NMDS Plot of Coral Data with Visit Status",
    x = "NMDS Dimension 1",
    y = "NMDS Dimension 2",
    color = "Visit",
    shape = "Visit"
  ) +
  theme_classic(base_size = 14) +
  geom_segment(
    data = as.data.frame(scores(envfit_result, display = "vectors")),
    aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
    arrow = arrow(length = unit(0.2, "inches")),
    color = "black"
  ) + geom_text(
    data = as.data.frame(scores(envfit_result, display = "vectors")),
    aes(x = NMDS1, y = NMDS2, label = rownames(scores(envfit_result, display = "vectors"))),
    color = "black", vjust = 1, hjust = 1
  ) + scale_color_manual(values = c("T" = "forestgreen", "NT" = "orange")) +
      scale_fill_manual(values = c("T" = "forestgreen", "NT" = "orange")) +
      scale_shape_manual(values = c("T" = 17, "NT" = 4))

# View the plot
nmds_coral

# Save the plot to desktop
ggsave(nmds_coral, filename = "nmds_coral.png")

# Run PERMANOVA including Distance as a predictor
permanova_result <- adonis2(coral_data_for_nmds ~ Visit * Distance, 
                         data = reef_metadata, method = "bray")

# Check the summary of the result
permanova_result

