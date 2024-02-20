# Diamonds
library(tidyverse)

# Use dplyr commands to create a diamonds data table that shows columns price, x, y, z.
# Also price values should be greater than 17000.

data <- diamonds%>%
  select("price", "x", "y", "z")%>%
  filter(price > 17000)

data

