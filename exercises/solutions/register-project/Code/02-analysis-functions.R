
mean_sd <- function(x, na.rm = TRUE) {
  
  xname <- deparse1(substitute(x))  # gets the name of the variable
  
  res <- c(mean = mean(x, na.rm = na.rm), 
           sd = sd(x, na.rm = na.rm))
  
  class(res) <- "meansd"
  attr(res, "variable") <- xname
  attr(res, "sampsize") <- length(x)
  
  res
  
}

print.meansd <- function(x, digits = 2) {
  
  paste0(
        round(x["mean"], digits = digits), 
        " (", 
        round(x["sd"], digits = digits), 
        ")")
  
}

tidy.meansd <- function(x) {
  
  data.frame(variable = attr(x, "variable"), 
             mean = x["mean"], 
             sd = x["sd"], 
             sample.size = attr(x, "sampsize")
  )
  
}
