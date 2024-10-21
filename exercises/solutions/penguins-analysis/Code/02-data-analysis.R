library(here)

## install.packages("broom")
library(broom)


penguins <- readRDS(here("Data", "penguins.rds"))

png(here("Output", "scatterplot.png"))
plot(body_mass_index ~ bill_length_mm, data= penguins, col = factor(species))
dev.off()

png(here("Output", "boxplot.png"))
boxplot(body_mass_index ~ sex, data = penguins)
dev.off()

t.test.res <- t.test(body_mass_index ~ sex, data = penguins)
write.csv(tidy(t.test.res), here("Output", "t-test.csv"))

## save results

saveRDS(t.test.res, file = here("Output", "t-test-bmi.rds"))


