library(tidyverse)
library(dplyr)
diamonds

diamonds_edit <- diamonds%>%
select("x", "y", "z")

print(diamonds_edit, n=30)
