library(here)


icd10codes <- expand.grid(LETTERS[1:11], 
            sprintf("%.02i", 0:99), 
            c(0, 1, 2, 9))
icd10codes <- do.call(paste0, as.list(icd10codes))

opcodes <- expand.grid(paste0("OP", LETTERS[20:24]), 
                       sprintf("%.03i", 80:220))

atccodes <- readLines(here("data", "atclist.txt"))

dates <- seq(as.Date("2005-01-01"), as.Date("2015-12-31"), by = 1)


generate_lpr <- function(pid) {
  
  ntimes <- max(1, rpois(1, lambda = 5))
  indat <- sort(sample(dates, ntimes))
  diag <- do.call(rbind, lapply(indat, function(dt) {
    
    hdia <- sample(icd10codes, 1)
    res <- unique(c(hdia, sample(icd10codes, max(1, rpois(1, 3)))))
    res[1:7]
    
  }))
  colnames(diag) <- c("hdia", paste0("diag", 1:6))
  
  sex <- ifelse(ntimes > 6, "m", "f")
  age <- max(100 - 4.5 * ntimes - 5, 1)
  
  data.frame(pid= pid, age = age, sex = sex, 
             indat = indat, visit = 1:length(indat),
             diag)
  
}

generate_lakmedel <- function(pid, year) {
  
  ntimes <- max(1, rpois(1, lambda = 10))
  
  idates <- dates[format(dates, "%Y") == as.character(year)]
  indat <- sort(sample(idates, ntimes))
  atc <- sample(atccodes, length(indat))
  ddd <- round(rexp(length(atc), .0025))
  data.frame(pid = pid, date = indat, atc = atc, dose = ddd)
  
  
}


pids <- sprintf("A%.03i", 1:400)
saveRDS(do.call(rbind, lapply(pids, generate_lpr)), 
  here("data", "lpr-ex.rds"))


for(year in 2005:2010) {
  saveRDS(do.call(rbind, lapply(pids, generate_lakmedel, year = year)), 
          here("data", sprintf("med-%s-ex.rds", year)))
  
  
}
