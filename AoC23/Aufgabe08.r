# einlesen ----
library(dplyr)
x <- readLines("Aufgabe8.txt")
head(x)

dir <- unlist(strsplit(x[1], ""))
or <- substr(x[-(1:2)], 1, 3)
ln <- substr(x[-(1:2)], 8, 10)           
rn <- substr(x[-(1:2)], 13, 15)
dimn <- sort(unique(c(or, ln, rn)))

zl <- matrix(FALSE, nrow = length(dimn), ncol = length(dimn), dimnames = list(dimn, dimn))
zl[cbind(or, ln)] <- TRUE

zr <- matrix(FALSE, nrow = length(dimn), ncol = length(dimn), dimnames = list(dimn, dimn))
zr[cbind(or, rn)] <- TRUE

zs <- list("L" = zl, "R" = zr)

# a
zl %*% zl %*% zr %*% zl %*% zl %*% zr

step <- 1
d <- dir[(step - 1) %% length(dir) + 1]
z <- zs[[d]]
cur <- "AAA"
new <- dimn[z[cur,]]

while(new != "ZZZ") {
  step <- step + 1
  cur <- new

  d <- dir[(step - 1) %% length(dir) + 1]
  z <- zs[[d]]

  new <- dimn[z[cur,]]
}
step

# b
step <- 1
d <- dir[(step - 1) %% length(dir) + 1]
z <- zs[[d]]
curs <- grep("A$", dimn, value = TRUE)
steps <- NULL

for(cc in seq_along(curs)) {# cc <- 1
  stepc <- step
  cur <- curs[cc]
  new <- dimn[z[cur,]]
  
  while(substr(new, 3, 3) != "Z") {
    stepc <- stepc + 1
    cur <- new
    
    d <- dir[(stepc - 1) %% length(dir) + 1]
    z <- zs[[d]]
    
    new <- dimn[z[cur,]]
  }
  
  steps <- c(steps, stepc)
}

library(primes)
Reduce("scm", steps)
library(numbers)
https://www.calculatorsoup.com/calculators/math/lcm.php -> steps
10668805667831
