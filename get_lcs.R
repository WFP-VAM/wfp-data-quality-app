library(haven)
library(tidyverse)

path = file.path("C:", "Users/alessandra.gherardel/Downloads", "sample_data.sav")
df = read_sav(path)

lcs <- df %>% select(starts_with("Lcs"))
colnames(lcs)

