# Forest mensuration problems SFR 601
# University of Maine
# Xinyuan Wei, 2024.2.12

#install.packages("tidyverse")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# Print the current working directory
current_directory <- getwd()
print(current_directory)


# Set the working directory
setwd("/Users/xinyuanwei/Library/CloudStorage/OneDrive-UniversityofMaineSystem/Course Documents/Inventory Data Process")

# Read the CSV files
howland_inventory <- read.csv("Howland_Inventory.csv")

### Visualize tree distribution in each plot
# Define the plot ID
plot_number <- 12

# Filter the data for the specified plot
plot_data <- howland_inventory %>% filter(Plot == plot_number)

# Convert azimuth and distance to Cartesian coordinates
plot_data <- plot_data %>%
  mutate(Azimuth = Azimuth * pi / 180,  # Convert to radians
         x = Distance * sin(Azimuth),
         y = Distance * cos(Azimuth),
         Species = as.factor(Species))   # Ensure Species is a factor

# Define the radius of the plot (m)
plot_radius <- 12.61

# Create a data frame for the circle
circle_data <- data.frame(
  x = plot_radius * cos(seq(0, 2 * pi, length.out = 100)),
  y = plot_radius * sin(seq(0, 2 * pi, length.out = 100))
)

# Create the plot
ggplot(plot_data, aes(x = x, y = y, size = DBH, fill = Species)) +
  geom_point(shape = 21, color = "black") +
  geom_path(data = circle_data, aes(x = x, y = y), color = "black", linetype = "dashed", inherit.aes = FALSE) +
  scale_size_continuous(name = "DBH (cm)") +
  scale_fill_manual(values = scales::hue_pal()(length(unique(plot_data$Species))),
                    name = "Species",
                    guide = guide_legend(override.aes = list(size = 6),  # Increase legend dot size
                                         keyheight = unit(2, 'lines'),
                                         keywidth = unit(2, 'lines'))) +
  labs(title = paste("Tree Distribution in Plot", plot_number), x = "X Coordinate", y = "Y Coordinate") +
  coord_fixed() +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12),  # Increase legend text size
    legend.title = element_text(size = 14)  # Increase legend title size
  )

### Calculate average tree size per plot
# Calculate the average DBH and Height for each plot
average_data <- howland_inventory %>%
  group_by(Plot) %>%
  summarise(
    Average_DBH = mean(DBH, na.rm = TRUE),
    SD_DBH = sd(DBH, na.rm = TRUE),
    Average_Height = mean(Height, na.rm = TRUE),
    SD_Height = sd(Height, na.rm = TRUE)
  )

# Show the table
print(average_data)

# Save the results to a CSV file
write.csv(average_data, "Plot_statistics.csv", row.names = FALSE)

# Create the DBH histogram
p1 <- ggplot(average_data, aes(x = factor(Plot), y = Average_DBH)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.5) +
  labs(title = "Average DBH per Plot", x = "Plot", y = "Average DBH (cm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Create the Height histogram
p2 <- ggplot(average_data, aes(x = factor(Plot), y = Average_Height)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black", width = 0.5) +
  labs(title = "Average Height per Plot", x = "Plot", y = "Average Height (m)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Combine the two plots into one figure
grid.arrange(p1, p2, ncol = 2)

### Analyze DBH and height relationship
# Fit the linear regression model
lm_model <- lm(Height ~ DBH, data = howland_inventory)
lm_eqn <- paste("Linear: y = ", round(coef(lm_model)[1], 2), " + ", round(coef(lm_model)[2], 2), "x", sep = "")
lm_r2 <- paste("R² = ", round(summary(lm_model)$r.squared, 2), sep = "")

# Fit the polynomial regression model
poly_model <- lm(Height ~ poly(DBH, 2), data = howland_inventory)
poly_eqn <- paste("Polynomial: y = ", round(coef(poly_model)[1], 2), " + ", round(coef(poly_model)[2], 2), "x + ", round(coef(poly_model)[3], 2), "x²", sep = "")
poly_r2 <- paste("R² = ", round(summary(poly_model)$r.squared, 2), sep = "")

# Generate scatter plot showing the relationship between DBH and height using all plot data
scatter_plot <- ggplot(howland_inventory, aes(x = DBH, y = Height)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Linear regression line
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "green", se = FALSE) +  # Polynomial regression line
  labs(title = "Relationship Between DBH and Height", x = "DBH (cm)", y = "Height (m)") +
  theme_minimal() +
  annotate("text", x = max(howland_inventory$DBH), y = min(howland_inventory$Height), 
           label = paste(lm_eqn, lm_r2, sep = "\n"), hjust = 1, vjust = -2, size = 5, color = "black") +
  annotate("text", x = max(howland_inventory$DBH), y = min(howland_inventory$Height) - 2, 
           label = paste(poly_eqn, poly_r2, sep = "\n"), hjust = 1, vjust = -1, size = 5, color = "black")

# Display the scatter plot
print(scatter_plot)

### Estimate tree/plot biomass
# Define the biomass calculation function
calculate_biomass <- function(dbh, species) {
  # Define species-specific coefficients
  coefficients <- list(
    "balsam fir" = c(-2.642, 2.475),
    "eastern hemlock" = c(-2.538, 2.452),
    "eastern white pine" = c(-2.5356, 2.4349),
    "northern red oak" = c(-2.47, 2.434),
    "paper birch" = c(-2.509, 2.453),
    "red maple" = c(-2.497, 2.456),
    "red spruce" = c(-2.5356, 2.4349),
    "white spruce" = c(-2.5356, 2.4349),
    "yellow birch" = c(-2.441, 2.438)
  )
  
  # Get the coefficients for the species
  coef <- coefficients[[tolower(species)]]
  if (is.null(coef)) {
    return(NA)  # Return NA for unknown species
  }
  
  # Calculate and return the biomass
  exp(coef[1] + coef[2] * log(dbh))
}

# Add the biomass column
howland_inventory <- howland_inventory %>%
  mutate(Biomass_kg = mapply(calculate_biomass, DBH, Species))

# Save the updated data set to a new CSV file
#write.csv(howland_inventory, "Howland_Inventory_with_Biomass.csv", row.names = FALSE)

# Define the area of each plot in square meters (m²)
area_per_plot_m2 <- plot_radius*plot_radius*3.14  # 500 m²

# Calculate the biomass density for each plot in kg/m²
biomass_density_data <- howland_inventory %>%
  group_by(Plot) %>%
  summarise(
    Total_Biomass_kg = sum(Biomass_kg, na.rm = TRUE),
    Biomass_Density_kg_m2 = Total_Biomass_kg / area_per_plot_m2
  )
  
# Save the biomass density data to a CSV file
write.csv(biomass_density_data, "Plot_Biomass.csv", row.names = FALSE)
  
# Plot the biomass density per plot
p1 <- ggplot(biomass_density_data, aes(x = factor(Plot), y = Biomass_Density_kg_m2)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.5) +
  labs(title = "Biomass density per plot", x = "Plot", y = "Biomass Density (kg/m²)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
# Display the plot
print(p1)

### Estimate tree/plot merchantable volume
# Define the species into softwoods and hardwoods
species_classification <- list(
  "softwoods" = c("balsam fir", "eastern hemlock", "eastern white pine", "red spruce", "white spruce"),
  "hardwoods" = c("northern red oak", "paper birch", "red maple", "yellow birch")
)

# Define the merchantable volume calculation function
calculate_volume <- function(dbh, species) {
  species <- tolower(species)
  if (species %in% species_classification$softwoods) {
    return(0.001 * dbh^2.5)  # Softwood volume equation
  } else if (species %in% species_classification$hardwoods) {
    return(0.002 * dbh^2.4)  # Hardwood volume equation
  } else {
    return(NA)  # Return NA for unknown species
  }
}

# Add the merchantable volume column
howland_inventory <- howland_inventory %>%
  mutate(Volume_m3 = mapply(calculate_volume, DBH, Species))

# Classify and summarize the volume data for each plot
volume_data <- howland_inventory %>%
  mutate(Classification = ifelse(tolower(Species) %in% species_classification$softwoods, "Softwood", "Hardwood")) %>%
  group_by(Plot, Classification) %>%
  summarise(Total_Volume_m3 = sum(Volume_m3, na.rm = TRUE)) %>%
  pivot_wider(names_from = Classification, values_from = Total_Volume_m3, values_fill = list(Total_Volume_m3 = 0))

# Save the volume data to a CSV file
write.csv(volume_data, "Plot_Volume.csv", row.names = FALSE)

# Plot the merchantable volume per plot
volume_data_long <- volume_data %>%
  pivot_longer(cols = c("Softwood", "Hardwood"), names_to = "Type", values_to = "Volume_m3")

ggplot(volume_data_long, aes(x = factor(Plot), y = Volume_m3, fill = Type)) +
  geom_bar(stat = "identity", position = "stack", color = "black", width = 0.7) +
  labs(title = "Merchantable volume per plot", x = "Plot", y = "Volume (m³)") +
  scale_fill_manual(values = c("Softwood" = "skyblue", "Hardwood" = "lightgreen")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

