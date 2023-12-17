# einlesen ----
library(dplyr)
x <- readLines("AoC23/Aufgabe17.txt")
head(x)
y <- lapply(x, strsplit, "") %>% lapply("unlist") %>% Reduce("rbind",.)
storage.mode(y) <- "numeric"
rownames(y) <- NULL
directions <- data.frame("right" = c(0, 1), "left" = c(0, -1), "up" = c(-1, 0), "down" = c(1, 0))
nd <- names(directions)

next_i <- function(pos, direction){
  pos <- pos + directions[[direction]]
}
next_d <- function(y, pos, last3){
  surr.pos <- t(pos[1,] + directions)
  probs <- ifelse(rowSums(surr.pos < c(1,1) | surr.pos > dim(y)) > 0, 0, y[surr.pos])
  if(all.equal(last3)
  sample(nd, 1, prob = probs)  
}

pos <- matrix(c(1, 1), ncol = 2, nrow = 1, dimnames = list(NULL, c("row", "col")))
direction <- "right"
next_d(y, pos, direction)