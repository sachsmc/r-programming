library(palmerpenguins)
library(dplyr)
library(data.table)

mean_sd <- function(x, na.rm = TRUE) {
  list(mean = mean(x, na.rm = na.rm), 
    sd = sd(x, na.rm = na.rm))
}

mean_sd(x = penguins$body_mass_g)

numeric_cols <- sapply(penguins, is.numeric)
apply(penguins[numeric_cols], MARGIN = 2, FUN = mean_sd)


## question 2
## tidyverse

penguins |> group_by(species, sex) |>
  summarize(mean_sd(body_mass_g, na.rm = TRUE))

penguins |> group_by(species, sex) |>
  summarize(mean.bm = mean(body_mass_g, na.rm = TRUE), 
            sd.bm = sd(body_mass_g, na.rm = TRUE))


## data.table

pengdt <- data.table(penguins)

pengdt[, (mean_sd(body_mass_g)), 
       by = .(species, sex)]

pengdt[, .(mean.bm = mean(body_mass_g, na.rm = TRUE), 
           sd.bm = sd(body_mass_g, na.rm = TRUE)), 
       by = .(species, sex)]
