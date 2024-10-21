library(palmerpenguins)
library(dplyr)
library(data.table)

mean_sd <- function(x, na.rm = TRUE) {
  
  xname <- deparse1(substitute(x))
  
  res <- c(mean = mean(x, na.rm = na.rm), 
    sd = sd(x, na.rm = na.rm))
  
  class(res) <- "meansd"
  attr(res, "variable") <- xname
  attr(res, "sampsize") <- length(x)
  
  res
  
}

print.meansd <- function(x, digits = 2) {
  
  cat(attr(x, "variable"), ": ",
    paste0(
    round(x["mean"], digits = digits), 
    " (", 
    round(x["sd"], digits = digits), 
    ")"))
  
}



msd_bm <- mean_sd(x = penguins$body_mass_g)

msd_bm
print(msd_bm, digits = 0)

mean_sd(penguins$bill_depth_mm)

ex1 <- c(mean= 1, sd = .5)
class(ex1) <- "meansd"






numeric_cols <- sapply(penguins, is.numeric)
apply(penguins[numeric_cols], MARGIN = 2, FUN = mean_sd)


baseapply <- split(penguins$body_mass_g, list(penguins$species, penguins$sex)) |>
  lapply(FUN = mean_sd) 

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
       by = list(species, sex)]

pengdt[, .(mean.bm = mean(body_mass_g, na.rm = TRUE), 
           sd.bm = sd(body_mass_g, na.rm = TRUE)), 
       by = .(species, sex)]



mean_sd2 <- function(x, na.rm = TRUE) {
  
  xname <- deparse1(substitute(x))
  
  res <- data.frame(mean = mean(x, na.rm = na.rm), 
           sd = sd(x, na.rm = na.rm), 
           variable = xname)
  
  res
  
}


peng_summary <- pengdt[, (mean_sd2(body_mass_g)), 
       by = list(species, sex)]

dcast(peng_summary, species + variable ~ sex, 
      value.var = c("mean", "sd"))

penguins |> group_by(species, sex) |>
  summarize(mean_sd2(body_mass_g, na.rm = TRUE))



mean_sd3 <- function(x, na.rm = TRUE) {
  
  xname <- deparse1(substitute(x))
  
  res <- data.frame(stat_value = c(
    mean(x, na.rm = na.rm), 
    sd(x, na.rm = na.rm)), 
    stat_type = c("mean", "sd"), 
    variable = xname)
  
  res
  
}
pengdt[, (mean_sd3(body_mass_g)), 
       by = list(species, sex)]


penguins |> group_by(species, sex) |>
  summarize(mean_sd2(body_mass_g, na.rm = TRUE))


