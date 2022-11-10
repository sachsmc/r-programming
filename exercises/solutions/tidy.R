library(dplyr)
library(data.table)
library(tidyr)

library(broom)

#Write a tidy method for our mean_sd function and try it out on the penguins dataset.

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

tidy.meansd <- function(x) {
  
  data.frame(variable = attr(x, "variable"), 
             mean = x["mean"], 
             sd = x["sd"], 
             sample.size = attr(x, "sampsize")
             )
  
}


mean_sd(penguins$body_mass_g)
tidy(mean_sd(penguins$body_mass_g))

#Apply the mean_sd function to the penguins body mass in grams by species and sex. Organize the results into a table suitable for publication, where it is easy to compare the two sexes.

pengdt <- data.table(penguins)
peng_summary <- pengdt[, (tidy(mean_sd(body_mass_g))), 
                       by = list(species, sex)]

dcast(peng_summary[!is.na(sex)], species + variable ~ sex, 
      value.var = c("mean", "sd", "sample.size"))


penguins |> group_by(species, sex) |>
  summarize(tidy(mean_sd(body_mass_g))) |> 
  filter(!is.na(sex)) |> 
  pivot_wider(names_from = "sex", 
              values_from = c("mean", "sd", "sample.size"))



lpr <- readRDS("lpr-ex.rds")


# Reshape the data into wide, where the columns are the primary diagnosis (hdia) at each visit number
## ill use data.table

lprdt <- data.table(lpr)
dcast(lprdt[, .(pid, indat, visit, hdia)], 
      pid ~ visit, value.var = "hdia")

# Reshape the data into longer format, where all of the diagnoses are stored in a single variable, with another variable indicating the primary diagnosis.

lprlong <- melt(lprdt, id.vars = c("pid", "indat", "visit"), 
                variable.name = "diagtype", value.name = "diag")
lprlong <- lprlong[!is.na(diag)]
lprlong[, main_diag := diagtype == "hdia"]

lprlong
setorder(lprlong, pid, indat)
lprlong

# Create a new variable for each participant which equals TRUE if they had any diagnosis of either D150, D152, or D159 before the date 1 January 2010.

## i will define the variable and name it "ddiag_pre2010"

summarylpr <- lprlong[, .(ddiag_pre2010 = any(diag[indat <= as.Date("2010-01-01")] %in% 
          c("D150", "D152", "D159"))), 
        by = .(pid)]

summarylpr[ddiag_pre2010 == TRUE]
lprdt[pid == "A056"]
