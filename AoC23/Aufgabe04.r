library(dplyr)
x <- readLines("Aufgabe4.txt")
head(x)

x <- gsub("Card\\s+\\d+:[ ]", "", x)

y <- lapply(lapply(x, strsplit, split = " \\| "), function(y) {
  list(winning = setdiff(as.numeric(unlist(strsplit(y[[1]][1], " "))), NA),
       having = setdiff(as.numeric(unlist(strsplit(y[[1]][2], " "))), NA))
})

# a
lapply(y, function(yy) {
  if(any(s <- yy$having %in% yy$winning)) 2^(sum(s) - 1) else 0
}) %>% unlist %>% sum

# b
l <- sapply(y, function(yy) sum(yy$having %in% yy$winning))
n <- rep(1, length(l))
for(ll in seq_along(l)) {
  ind <- seq(ll + 1, length.out = l[ll])
  n[ind] <- n[ind] + n[ll]
}
sum(n)
