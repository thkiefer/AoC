# einlesen ----
library(dplyr)
x <- readLines("Aufgabe9.txt")
head(x)

y <- lapply(x, function(xx) as.numeric(unlist(strsplit(xx, " "))))


# a
lapply(seq_along(y), function(l){# l <- 1
  
  d <- y[[l]]
  val <- tail(d, 1)
  
  while(any(d != 0)) {
    d <- diff(d)
    val <- c(tail(d, 1), val)
  }  
  
  Reduce("+", val)
  
}) %>% unlist() %>% sum()


# b
lapply(seq_along(y), function(l){# l <- 3
  
  d <- y[[l]]
  val <- head(d, 1)
  
  while(any(d != 0)) {
    d <- diff(d)
    val <- c(head(d, 1), val)
  }  
  
  diffneu <- function(e1, e2) e2-e1
  Reduce("diffneu", val)
  
}) %>% unlist() %>% sum()
