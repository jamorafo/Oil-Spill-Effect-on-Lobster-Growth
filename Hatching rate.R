# Hatching rate function.

# Firs of all, this script models the effect of temperature on the development time of lobster larvae, referencing Quinn and Rochette (2015). 
# It starts by estimating developmental time as a function of temperature, then computes its rate of change. 
# The script validates this model and updates the hatching rate—a measure of developmental acceleration—over a range of temperatures. 
# It proceeds to fit and visualize these rates using a fifth-degree polynomial model, providing a formula for simulating
# this relationship in  NetLogo. The model serves to predict how varying temperatures could impact
# the growth rate of lobster larvae, crucial information for understanding the potential impacts of climate change on lobster populations.

# Predicted development time (D) stage I in terms of Temperature (t) as in Table 1 Quinn, B. K., & Rochette, R. (2015)
D <- function(t) 39.558-3.659*t+0.092*t^2

ggplot(data.frame(Temperature = 5:25, Development_Time = sapply(5:25, D)), aes(x = Temperature, y = Development_Time)) +
  geom_line() +
  labs(title = "Predicted Development Time of Lobster Larvae",
       x = "Temperature (°C)",
       y = "Development Time (days)") +
  theme_minimal()



# Derivative of D regarding t.
ratio <- function(t) 2*0.092*t-3.659

ggplot(data.frame(Temperature = 5:25, Ratio = sapply(5:25, ratio)), aes(x = Temperature, y = Ratio)) +
  geom_line() +
  labs(title = "Rate of Change in Development Time with Temperature",
       x = "Temperature (°C)",
       y = "Rate of Change in Development Time") +
  theme_minimal()

# Validate the approximation in terms of the derivative
D_dx <- function(x) D(x)*(1+ratio(x)*d_x/D(x))
summary(D_dx(tt)-D(tt+d_x))


h_r <- tt_r <- as.numeric()
# Initial hatching rate at the first day of the hatching period (170th day)
h_r[1] <- 0.001
tt_r[1] <- sst(170)

# Compute the hatching rate updates regarding the ratio of the predicted development time.
for (ll in 1:1000){
  h_r[ll+1] <- h_r[ll]*(1-ratio(tt_r[ll])*d_x/D(tt_r[ll]))
  tt_r[ll+1] <- tt_r[ll]+d_x
}


ggplot(data.frame(Temperature = tt_r, Hatching_Rate = h_r), aes(x = Temperature, y = Hatching_Rate)) +
  geom_line() +
  labs(title = "Hatching Rate of Lobster Larvae Development Over Temperature",
       x = "Temperature (°C)",
       y = "Hatching Rate") +
  theme_minimal()


# Make a dataframe with those new values
hatching_rates <- data.frame(temperature=tt_r,hatching_rate=h_r)
head(hatching_rates)


# Fit a second-degree polynomial model
polynomial_model <- lm(hatching_rate ~ poly(temperature, 5,raw = T), data = hatching_rates)


# Summary of the model to see coefficients and statistics
summary(polynomial_model)


# To visualize the fit
plot(hatching_rates$temperature, hatching_rates$hatching_rate, main="Polynomial Fit", xlab="Temperature", ylab="Hatching Rate")
# Add the curve to the plot
curve(predict(polynomial_model, data.frame(temperature = x)), add = TRUE,col="blue")

# The following formula goes to the hatching rate variable in the netlogo simulation. 
fx <- function(x) - 1.540495e-01  + 4.732802e-02 * x - 5.734975e-03 * x ^ 2 + 3.426111e-04  *  x ^ 3 - 1.002420e-05  *  x ^ 4 + 1.146263e-07 * x ^ 5
fx(14.44473)



ggplot(hatching_rates, aes(x = temperature, y = hatching_rate)) +
  geom_point(aes(color = "Hatching Rate Approximation")) +  # Assign a color for points to appear in the legend
  geom_smooth(method = "lm", formula = y ~ poly(x, 5), 
              aes(linetype = "Predicted Value"), color = "red") +  # Use dotted line for the smooth line
  scale_color_manual(name = "Legend", 
                     values = c("Hatching Rate Approximation" = "black")) +
  scale_linetype_manual(name = "Legend", 
                        values = c("Predicted Value" = "longdash")) +  # Define dotted linetype in the legend
  labs(title = "Polynomial Fit to Hatching Rate Approximation",
       x = "Temperature (°C)",
       y = "Hatching Rate",
       caption = expression( - 1.540495e-01 + 4.732802e-02*t - 5.734975e-03*t^2 + 3.426111e-04*t^3 - 1.002420e-05*t^4 + 1.146263e-07*t^5) )+
  theme_minimal() +
  theme(legend.position = "bottom")









