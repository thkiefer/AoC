# einlesen ----
library(dplyr)
x <- readLines("AoC23/Aufgabe17.txt")
head(x)
y <- lapply(x, strsplit, "") %>% lapply("unlist") %>% Reduce("rbind",.)
storage.mode(y) <- "numeric"
rownames(y) <- NULL
directions <- data.frame("right" = c(0, 1), "left" = c(0, -1), "up" = c(-1, 0), "down" = c(1, 0))
nd <- names(directions)
neig.losses <- setNames(rep(Inf, 4), nd)

next_i <- function(pos, direction){
  pos + directions[[direction]]
}

neigh <- function(y, pos2, dir2){
  surr.pos <- t(pos2[1,] + directions)
  neig.los <- neig.losses
  ind <- rowSums(surr.pos < c(1,1) | surr.pos > dim(y)) > 0
  if(sum(!ind) > 0) neig.los[!ind] <- y[surr.pos[!ind,,drop = FALSE]]
  if(length(dir2) >= 3) if(all(tail(dir2, 3) == tail(dir2, 1))) neig.los[tail(dir2, 1)] <- Inf
  if(tail(dir2, 1) == "right") neig.los["left"] <- Inf
  if(tail(dir2, 1) == "left") neig.los["right"] <- Inf
  if(tail(dir2, 1) == "up") neig.los["down"] <- Inf
  if(tail(dir2, 1) == "down") neig.los["up"] <- Inf
  neig.los
}


pos0 <- matrix(c(1, 1), ncol = 2, nrow = 1, dimnames = list(NULL, c("row", "col")))
dir0 <- NULL

rand_neig_path <- function(y, pos1, dir1) {
  # pos1 <- pos0; dir1 <- dir0
  poss <- matrix(NA, ncol = 2, nrow = 10, dimnames = list(NULL, c("row", "col")))
  dirs <- character(10);
 
  i <- nrow(pos1)
  while(any(pos1[i,] < dim(y))) {
    i <- i + 1
    if(i > nrow(pos1)) {
      pos1 <- rbind(pos1, poss)
      dir1 <- c(dir1, dirs)
    }
    dir1[i - 1] <- sample(names(which(neigh(y, pos1[i-1,,drop = FALSE], dir1[1:(i-2)]) < Inf)), 1)
    pos1[i,] <- next_i(pos1[i-1,,drop = FALSE], dir1[i - 1])
  }
  list(pos1[1:i,,drop = FALSE], dir1[1:(i-1)])  
}

inc <- rand_neig_path(y, pos0, dir0)
sumdiag2 <- 2 * sum(diag(y))
fit <- function(des) sum(y[des[[1]]]) / sumdiag2
fitinc <- fit(inc)
best <- list(inc, fitinc)

t <- 1; desc <- .8; l <- 1e4; tmin <- 1e-3; tt <- 0
while(t > tmin){
  tt <- tt + 1
  
  for(ll in 1:l) {
    quant <- (ll - 1) %% 20 + 1
    nr <- ceiling(nrow(inc[[1]]) / 20)
    this <- sample((quant-1)*nr+1:nr, 1)
    child <- rand_neig_path(y, inc[[1]][1:this,], inc[[2]][1:(this-1)])
    fitchild <- fit(child)
    
    if(fitchild < fitinc) { 
      ersetze <- TRUE
    } else {
      p <- exp(-(fitchild - fitinc) / t)
      if(p > runif(1)) ersetze <- TRUE
    }
    
    if(ersetze) {
      inc <- child
      fitinc <- fitchild
    }
    
    if(fitchild < best[[2]]) best <- list(child, fitchild)
    if(ll %% 50 == 0) cat("\r", tt, ", ", ll, "; ", round(fitchild, 2), "; ", round(best[[2]], 2))
  }
  
  t <- t * desc
}


