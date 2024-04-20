# This script produce the Mean Daily SST in terms of the temperature approximating the curves in the figure 2 in
# Quinn, B. K., & Rochette, R.,(2015) 

# Create a sequence of days from 120 to 270
x <- 120:270
# Calculate y using the quadratic formula provided
y <- -0.00177*x^2 + 0.7457*x - 62.54

# Create a data frame for plotting
data <- data.frame(x, y)

# Plot the data
p <- ggplot(data, aes(x, y)) +
  geom_line(color="blue", size=1) +  # Add the line
  xlab("Day of year (day)") +  # X-axis label
  ylab("Mean daily SST (SST Celsius)") +  # Y-axis label
  theme_bw() +  # Use a white background theme
  theme(plot.title = element_text(hjust = 0.5),  # Center the plot title
        text = element_text(size=12))  # Default text size

# Annotate with the LaTeX-style expression
p <- p + annotate("text", x = mean(x), y = mean(y), label = expression(-0.00177 * day^2 + 0.7457 * day - 62.54), 
                  parse = TRUE, hjust = 0.5, vjust = 0.5, size = 6)

# Display the plot
print(p)