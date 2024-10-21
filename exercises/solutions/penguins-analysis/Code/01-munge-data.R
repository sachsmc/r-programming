library(palmerpenguins) ## required for the data, palmerpenguins_0.1.1
library(here)

penguins$body_mass_index <- penguins$body_mass_g / penguins$flipper_length_mm^2

saveRDS(penguins, file = here("Data", "penguins.rds"))
