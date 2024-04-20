# Oil concentration rate 

# This code performs the oil concentration rate in marine water due to natural degradation processes over time. 
# The script sets initial parameters such as the volume of spilled oil, the density of oil, and the area 
# and depth of the affected marine region. It then calculates the initial concentration of oil in mg/L 
# and simulates the degradation of oil concentration in water, assuming a 5% daily degradation rate, 
# until it reaches a concentration effectively considered as zero (below 0.001 mg/L). The results are
# then prepared for visualization, and the final plot displays the daily decrease in oil concentration over
# time with an accompanying caption indicating the basis of the data model. The code is structured to facilitate 
# adjustments to the initial conditions, allowing for flexible application to various spill scenarios.


# Load necessary library
library(ggplot2)

# Define initial conditions and parameters
initial_release_barrels <- 53000  # Daily initial release in barrels
barrel_to_liter <- 159  # Conversion from barrels to liters
oil_density <- 880  # Oil density in g/L (grams per liter)
mg_per_g <- 1000  # Conversion from grams to milligrams
area_km2 <- 10  # Affected area in square kilometers
depth_m <- 1500  # Ocean depth in meters
degradation_rate <- 0.95  # 5% degradation per day

# Convert area to square meters
area_m2 <- area_km2 * 1e6

# Calculate total volume of water affected in liters
total_volume_liters <- area_m2 * depth_m * 1000  # m² * m * L/m³

# Calculate initial oil mass in milligrams
initial_oil_volume_liters <- initial_release_barrels * barrel_to_liter
initial_oil_mass_mg <- initial_oil_volume_liters * oil_density * mg_per_g

# Calculate initial concentration in mg/L
initial_concentration_mg_per_L <- initial_oil_mass_mg / total_volume_liters

# Calculate oil concentration over an extended period
# Assume we model until the concentration is effectively zero (e.g., below 0.001 mg/L)
days <- 1
concentrations <- numeric()
current_concentration <- initial_concentration_mg_per_L

while(current_concentration > 0.001) {
  concentrations <- c(concentrations, current_concentration)
  current_concentration <- current_concentration * degradation_rate
  days <- days + 1
}

# Prepare data for plotting
concentration_data <- data.frame(Day = 1:length(concentrations), Concentration_mg_per_L = concentrations)

# Plot the data
ggplot(concentration_data, aes(x = Day, y = Concentration_mg_per_L)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Daily Decrease in Oil Concentration Over Time",
       x = "Day",
       y = "Concentration (mg/L)",
       caption = "Data model based on initial spill conditions and natural degradation.")
