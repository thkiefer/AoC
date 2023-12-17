# einlesen ----
library(dplyr)
x <- readLines("AoC23/Aufgabe13.txt")
head(x)

ind <- c(0, which(x == ""))
yy <- lapply(seq_along(ind), function(ii) {# ii <- 1
  if(ii == 1) this.i <- seq(from = 1, to = ind[ii + 1] - 1, by = 1)
  if(ii > 1 & ii < length(ind))  this.i <- seq(from = ind[ii] + 1, to = ind[ii + 1] - 1, by = 1)
  if(ii == length(ind))  this.i <- seq(from = ind[ii] + 1, to = length(x), by = 1)
  mx <- as.matrix(1 * (strsplit(x[this.i], split = "") %>% Reduce("rbind", .) == "#"))
  dimnames(mx) <- list(NULL, NULL)
  mx
})

# a
oo <- data.frame()
for(ii in seq_along(yy)) {

  get_ind <- function(mx) {
    rr <- which(colMeans(mx[, -ncol(mx)] == mx[, -1]) == 1)
    out <- 0
    if(length(rr) > 0) {
      for(r in seq_along(rr)) {
        r <- rr[r]
        nc <- ncol(mx) - r    
        if(r >= nc) {
          if(all(mx[, (r-nc+1):r] == mx[, ncol(mx):(r+1)])) out <- r 
        } else {
          if(all(mx[, 1:r] == mx[, (r+1+r-1):(r+1)])) out <- r
        }
      }
    } 
    out
  }
  
  #
  oo <- rbind(oo, data.frame("row" = get_ind(t(yy[[ii]])), "col" = get_ind(yy[[ii]])))
  
}

sum(colSums(oo) * c(100, 1))

# b
oo2 <- data.frame()
check_this <-  function(this.mx, r, nc) {
  if(r >= nc) {
    check <- this.mx[, (r-nc+1):r] != this.mx[, ncol(this.mx):(r+1)]
  } else {
    check <- this.mx[, 1:r] != this.mx[, (r+1+r-1):(r+1)]
  }
  return(check)
}     
for(ii in seq_along(yy)) {# ii <- 1
  
  mx <- yy[[ii]] #col
  tmx <- t(mx) #row
  
  cc <- colSums(mx[, -ncol(mx)] == mx[, -1]) - nrow(mx)
  rr <- colSums(tmx[, -ncol(tmx)] == tmx[, -1]) - nrow(tmx)
  
  rc <- list("row" = rr, "col" = cc)
  mxs <- list("row" = tmx, "col" = mx)
  
  # Fall 1: es gibt eine 0 in der anderen Orientierung 
  #         (die vorher invalide war, sonst wäre oo[ii,THIS] != 0)
  was0 <- colnames(oo)[which(oo[ii,] == 0)]
  was1 <- colnames(oo)[which(oo[ii,] != 0)]
  found <- 0
  thiswas <- was0
  if(any(rc[[thiswas]] == 0)) {
    whichwas <- which(rc[[thiswas]] == 0)
    for(r in seq_along(whichwas)) {# r <- 1
      r <- whichwas[r]
      this <- mxs[[thiswas]]
      nc <- ncol(this) - r
      
      if(sum(check_this(this, r, nc)) == 1) {
        out <- r; was <- thiswas; found <- 1
      }
    }
  }
  
  # Fall 2: es gibt eine 0 in der gleichen Orientierung 
  thiswas <- was1
  if(sum(rc[[thiswas]] == 0) > 1 & found == 0) {
    whichwas <- setdiff(which(rc[[thiswas]] == 0), oo[ii, thiswas])
    for(r in seq_along(whichwas)) {# r <- 1
      r <- whichwas[r]
      this <- mxs[[thiswas]]
      nc <- ncol(this) - r
      
      if(sum(check_this(this, r, nc)) == 1) {
        out <- r; was <- thiswas; found <- 1
      }
    }
  }
     
  # Fall 3: es gibt eine -1 in der gleichen Orientierung
  thiswas <- was1
  if(any(rc[[thiswas]] == -1) & found == 0) {
    whichwas <- which(rc[[thiswas]] == -1)
    for(r in seq_along(whichwas)) {# r <- 1
      r <- whichwas[r]
      this <- mxs[[thiswas]]
      nc <- ncol(this) - r
      
      if(sum(check_this(this, r, nc)) == 1) {
        out <- r; was <- thiswas; found <- 1
      }
    }
  }
  
  # Fall 4: es gibt eine -1 in der anderen Orientierung
  thiswas <- was0
  if(any(rc[[thiswas]] == -1) & found == 0) {
    whichwas <- which(rc[[thiswas]] == -1)
    for(r in seq_along(whichwas)) {# r <- 1
      r <- whichwas[r]
      this <- mxs[[thiswas]]
      nc <- ncol(this) - r
      
      if(sum(check_this(this, r, nc)) == 1) {
        out <- r; was <- thiswas; found <- 1
      }
    }
  }
  
  if(found == 0) stop(5)
  
  thisoo <- data.frame("row" = 0, "col" = 0)
  thisoo[1, was] <- out
  oo2 <- rbind(oo2, thisoo)
}

sum(colSums(oo2) * c(100, 1))
