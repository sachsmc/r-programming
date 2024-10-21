library(here)
library(data.table)

hosp <- data.table(readRDS(here("Rawdata/lpr-ex.rds")))

## iterate over the drug file names

drugnames <- sprintf("Rawdata/med-%s-ex.rds", 2005:2010)
drugs <- data.table(do.call(rbind, lapply(drugnames, FUN = \(x) readRDS(here(x)))))

## merge drugs into hosp

alldat <- merge(hosp, drugs, by = "pid", allow.cartesian = TRUE)

## only keep the nearest drug dispensation after the hospitalization date

alldat[, days_to_drug := as.numeric(date - indat)]
alldat <- alldat[days_to_drug >= 0]

alldat[, keep := days_to_drug == min(days_to_drug), by = .(pid, indat)]

alldat <- alldat[keep== TRUE]

## alternative using roll

drugs[, drug_date := date]
alldat2 <- drugs[hosp, on = .(pid, date == indat), roll = -Inf] ## -Inf says to look backwards
alldat2 <- alldat2[!is.na(atc)]
alldat2[, days_to_drug := as.numeric(drug_date - date)]

saveRDS(alldat2, here("Data/analysis-data.rds"))

