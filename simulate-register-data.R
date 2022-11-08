
icd10codes <- expand.grid(LETTERS[1:11], 
            sprintf("%.02i", 0:99), 
            c(0, 1, 2, 9))
icd10codes <- do.call(paste0, as.list(icd10codes))

opcodes <- expand.grid(paste0("OP", LETTERS[20:24]), 
                       sprintf("%.03i", 80:220))

atccodes <- readLines("atclist.txt")

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
  
  data.frame(pid= pid, indat = indat, visit = 1:length(indat),
             diag)
  
}




pids <- sprintf("A%.03i", 1:400)
saveRDS(do.call(rbind, lapply(pids, generate_lpr)), 
  "data/lpr-ex.rds")



