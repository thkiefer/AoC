library(dplyr)
x <- readLines("Aufgabe6.txt")
head(x)

y <- data.frame("Time" = as.numeric(setdiff(unlist(strsplit(gsub("\\w+:\\s+", "", x[1]), " ")), "")),
                "Distance" = as.numeric(setdiff(unlist(strsplit(gsub("\\w+:\\s+", "", x[2]), " ")), "")))
y <- as.matrix(y)

# start-speed = 0
# incr = 1

#a
fn <- function(xx) {# t <- 7; d <- 9
  t <- xx[1]
  d <- xx[2]
  # hold time
  ht <- seq(from = 0, to = t)
  # travel time
  tt <- (t - ht) * ht
  
  return(sum(tt > d))
}
apply(y, 1, fn) %>% unlist %>% prod()

#b
t <- as.numeric(gsub("\\w+:\\s+|\\s", "", x[1]))
d <- as.numeric(gsub("\\w+:\\s+|\\s", "", x[2]))

# hold time
# ht <- seq(from = 0, to = t)

# distance is
# tt <- (t - ht) * ht
# d < -ht^2 + ht * t
# Schnittstellen sind bei 
# ht^2 - t*ht + d
# =>
diff(c(ceiling(t/2 - sqrt((t/2)^2 - d)), floor(t/2 + sqrt((t/2)^2 - d)))) + 1

# ht <- seq(from = 0, to = t)
# tt <- (t - ht) * ht
# ht[tt > d][1]
# ht[tt > d][sum(tt > d)]
# sum(tt > d)
