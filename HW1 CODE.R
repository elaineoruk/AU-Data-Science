library(pracma)  # For numerical integration

# Define the pdf of the Weibull distribution
weibull_pdf <- function(x, theta) {
  (1/theta^3) * 3 * x^2 * exp(-x^3/theta^3)
}

# Function to calculate the cumulative distribution function (CDF) using numerical integration
weibull_cdf <- function(x, theta) {
  integrate(weibull_pdf, lower = 0, upper = x, theta = theta)$value
}

# Function to find the inverse CDF using numerical root-finding algorithm
find_inverse_cdf <- function(u, theta, tol = 1e-6) {
  root <- uniroot(function(x) weibull_cdf(x, theta) - u, lower = 0, upper = 10, tol = tol)
  return(root$root)
}

# Function to generate random observations from the Weibull distribution
generate_weibull <- function(n, theta) {
  U <- runif(n)  # Generate n uniform random numbers
  x <- sapply(U, function(u) find_inverse_cdf(u, theta))  # Apply inverse CDF
  return(x)
}

# Example usage:
theta <- 3  # Shape parameter
n <- 100    # Number of observations
random_sample <- generate_weibull(n, theta)
random_sample




# Define the function to integrate
f <- function(x) 1 / (1 + x)

# Number of simulations
n <- 10000

# Generate n random numbers between 0 and 1
random_numbers <- runif(n)

# Evaluate the function at the random numbers and take the average
approximation <- mean(f(random_numbers))

# Multiply by the interval width to get the integral approximation
integral_approximation <- approximation * 1

# Multiply by ln(2) to convert to base 2 logarithm
log2_estimate <- integral_approximation * log(2)

# Print the result
print(log2_estimate)



# Function to estimate log2 and its error of estimation
estimate_log2 <- function(n) {
  # Generate n random numbers between 0 and 1
  random_numbers <- runif(n)
  
  # Evaluate the function at the random numbers and take the average
  approximation <- mean(1 / (1 + random_numbers))
  
  # Multiply by ln(2) to convert to base 2 logarithm
  log2_estimate <- approximation * log(2)
  
  # Calculate standard error of the mean
  SEM <- sd(1 / (1 + random_numbers)) / sqrt(n)
  
  # Calculate 95% confidence interval
  error_margin <- 1.96 * SEM
  
  # Confidence interval
  lower_bound <- log2_estimate - error_margin
  upper_bound <- log2_estimate + error_margin
  
  # Return the estimate, error of estimation, and confidence interval
  return(list(estimate = log2_estimate, error = error_margin, confidence_interval = c(lower_bound, upper_bound)))
}

# Obtain estimate for 10,000 simulations
result <- estimate_log2(10000)
print(result)