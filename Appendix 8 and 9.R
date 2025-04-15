# Load packages
library(fmsb)
library(dplyr)
library(dplyr)
library(tidyr)

## Radar plot of mean coral by site type 
# Load data set
spider.data<- diss
spider.data$Dead<- as.numeric(spider.data$Dead)  # Make sure it is numeric

# Subset data 
spider.data.refined<- spider.data%>%
  dplyr::select(Branched, Foliose, Tabular, Boulder, Encrusting, Free_L, Dead)

radar_data <- spider.data %>%
  group_by(Visit) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(Visit, Branched, Foliose, Tabular, Boulder, Encrusting, Free_L)
view(radar_data)  # Make sure data looks right

# Create groups for data in T and NT Sites
group1 <- radar_data %>% filter(Visit == "T") %>% dplyr::select(-Visit)  
group2 <- radar_data %>% filter(Visit == "NT") %>% dplyr::select(-Visit)

# if percentages are 0-100, convert to 0-10
if (max(radar_data[-1], na.rm = TRUE) > 10) {
  radar_data[-1] <- radar_data[-1] / 10
}
view(radar_data)  # Check this is correct

# Define max and min values (for 0-10 scale)
max_vals <- rep(10, ncol(radar_data) - 1)  # Set max to 10
min_vals <- rep(0, ncol(radar_data) - 1)   # Set min to 0

# Combine into final radar data
radar_data_final <- rbind(max_vals, min_vals, radar_data[-1]) %>%
  dplyr::rename(`Free Living` = Free_L)  # Rename for visualisation purposes
view(radar_data_final)  # Check everything is correct

# Plot the radar 
radar<- radarchart(radar_data_final, axistype = 1,
                   pcol = c("forestgreen", "orange"),    # Colors for the two groups
                   plwd = 2,                   # Line width
                   plty = 1,                   # Line type
                   axislabcol = "black",       # Axis label color
                   cglcol = "black",            # Grid color
                   cglty = 1,                   # Grid line type
                   caxislabels = seq(0, 10, 2), # Axis labels from 0 to 10
                   title = "Mean Coral Cover") 

legend("topright", legend = unique(reef$Visit), 
       col = c("forestgreen", "orange"), lty = 1, lwd = 2)
ggsave(radar, filename = "radar.png")

## Summary table
# Print a table of mean percent cover of each growth form category
view(long_data)
summary_table <- long_data %>%
  group_by(`Coral Type`, Visit) %>%
  summarise(
    mean_cover = round(mean(`Percentage Cover`, na.rm = TRUE), 1),
    sd_cover = round(sd(`Percentage Cover`, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>% pivot_wider(
    names_from = Visit,
    values_from = c(mean_cover, sd_cover),
    names_glue = "{.value}_{Visit}"
  ) %>% select(`Coral Type`, 
         `mean_cover_T`, `sd_cover_T`, 
         `mean_cover_NT`, `sd_cover_NT`) %>%
  rename(
    `Mean % Cover (T)` = mean_cover_T,
    `SD (T)` = sd_cover_T,
    `Mean % Cover (NT)` = mean_cover_NT,
    `SD (NT)` = sd_cover_NT
  )
summary_table