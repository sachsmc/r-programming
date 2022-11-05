even10 <- c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, 
            FALSE, TRUE, FALSE, TRUE)

for(i in 1:10){
  if(i %in% c(2, 4, 6, 8, 10)) {
  print(i)
  }
}

library(palmerpenguins)


for(colnum in 1:8){
  
  thiscol <- penguins[[colnum]]
  if(is.double(thiscol)) {
    
    thismean <- mean(thiscol, na.rm = TRUE)
    penguins[is.na(thiscol), colnum] <- thismean
    
  } else if(is.integer(thiscol)) {
    
    tabbedvals <- sort(table(thiscol), decreasing = TRUE)
    penguins[is.na(thiscol), colnum] <- as.integer(names(tabbedvals)[1])
    
  } else if(is.factor(thiscol)){
    
    tabbedvals <- sort(table(thiscol), decreasing = TRUE)
    penguins[is.na(thiscol), colnum] <- as.factor(names(tabbedvals)[1])
    
  } 
    
}

summary(penguins)
