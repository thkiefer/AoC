# einlesen ----
library(dplyr)
x <- readLines("AoC23/Aufgabe12.txt")
head(x)

# a ----
check <- function(yy, n){
  yy <- unlist(strsplit(paste0(yy, collapse = ""), "[.]"))
  yy <- yy[yy != ""]
  paste0(nchar(yy), collapse = ",") == n
} 

if(FALSE){
  treffer <- numeric(length(x))
  for(ii in seq_along(x)){#ii <- 2
    
    tmp <- strsplit(x[[ii]], " ") %>% unlist()
    y <- strsplit(tmp[1], "") %>% unlist
    n <- as.numeric(unlist(strsplit(tmp[2], ",")))
    
    n_op <- sum(nchar(y)) - sum(n)
    n_op_miss <- n_op - length(grep("[.]", y))
    ind <- which(y == "?")
    
    # replace schlau
    
    # replace full search
    cb <- combn(ind, n_op_miss)
    for(cc in 1:ncol(cb)) {# cc <- 2
      
      thisi <- cb[,cc]
      y.tmp <- y
      y.tmp[thisi] <- "."
      y.tmp[setdiff(ind, thisi)] <- "#"
      check(y.tmp, tmp[2])
      
      treffer[ii] <- treffer[ii] + check(y.tmp, tmp[2])
    }
  }
}

# b ----
treffer <- numeric(length(x))

for(ii in seq_along(x)){#ii <- 1
  
  tmp <- strsplit(x[[ii]], " ") %>% unlist()
  y <- strsplit(tmp[1], "") %>% unlist
  n <- as.numeric(unlist(strsplit(tmp[2], ",")))
  ind <- which(y == "?")
  
  replace_schlau <- function(yy, n){# yy <- k0; n <- tmp[2]
    
    yy2 <- yy
    nn2 <- strsplit(n, ",") %>% unlist %>% as.numeric
    yy2 <- strsplit(paste0(yy2, collapse = ""), "[.]") %>% unlist()
    if(tail(yy, 1) == ".") yy2 <- c(yy2, "")
    
    # first
    ind0 <- which(yy2 != "")
    this.y <- yy2[ind0[1]]
    nc.y <- nchar(this.y)
    if(length(grep("[?]", this.y)) > 0) {
      # 
      if(nc.y < nn2[1]) {
        if(length(grep("#", this.y)) > 0) {
          out <- 0
        } else {
          yy2[ind0[1]] <- paste0(rep(".", nc.y), collpase = "")
        }
      }
      
      if(nc.y == nn2[1]) {
        yy2[ind0[1]] <- paste0(rep("#", nc.y), collpase = "")
      }
      
      if(nc.y == nn2[1] + 1) {
        this.y.splt <- strsplit(this.y, "") %>% unlist
        if(this.y.splt[1] == "#" & this.y.splt[nc.y] == "#") {
          out <- 0
        } else if(this.y.splt[1] == "#") {
          yy2[ind0[1]] <- paste0(c(rep("#", nn2[1]), "."), collpase = "")
        } else if(this.y.splt[nc.y] == "#") {
          yy2[ind0[1]] <- paste0(c(".", rep("#", nn2[1])), collpase = "")
        } 
      }
      
      if(nc.y == nn2[1] + 2) {
        this.y.splt <- strsplit(this.y, "") %>% unlist
        if((this.y.splt[1] == "#" & this.y.splt[nc.y-1] == "#") | 
           (this.y.splt[2] == "#" & this.y.splt[nc.y  ] == "#") | 
           (all(this.y.splt == "?"))) {
          out <- 0
        } else {
          if(this.y.splt[1] == "#"){
            yy2[ind0[1]] <- paste0(c(rep("#", nn2[1]), ".", this.y.splt[nc.y]), collpase = "")
          } else if(this.y.splt[nc.y] == "#") {
            yy2[ind0[1]] <- paste0(c(this.y.splt[1], ".", rep("#", nn2[1])), collpase = "")
          } else {
            yy2[ind0[1]] <- paste0(c(".", rep("#", nn2[1]), "."), collpase = "")
        }
      }
    }
    }
    
    # recursion 2 only tail
    # head vollständig aufgelöst
    ind0 <- which(yy2 != "")
    this.y <- yy2[ind0[1]]
    if(length(grep("?", yy2[ind0[1]])) == 0 & length(ind0) > 1) {
      yy2t <- paste0(yy2[-(1:ind0[1])], collapse = ".")
      y.tmp <- replace_schlau(unlist(strsplit(yy2t, "")), paste0(nn2[-1], collapse = ","))
      yy2 <- c(yy2[1:ind0[1]],
               y.tmp %>% paste0(., collapse = "") %>% strsplit("[.]") %>% unlist())
      
      yy2 <- strsplit(paste0(yy2, collapse = "."), "[.]") %>% unlist()
    }
    
    # last
    ind0 <- which(yy2 != "")
    this.y <- yy2[ind0[length(ind0)]]
    nc.y <- nchar(this.y)
    if(length(grep("[?]", this.y)) > 0) {
      # 
      if(nc.y < nn2[1]) {
        if(length(grep("#", this.y)) > 0) {
          out <- 0
        } else {
          yy2[ind0[1]] <- paste0(rep(".", nc.y), collpase = "")
        }
      }
      
      if(nc.y == nn2[1]) {
        yy2[ind0[1]] <- paste0(rep("#", nc.y), collpase = "")
      }
      
      if(nc.y == nn2[1] + 1) {
        this.y.splt <- strsplit(this.y, "") %>% unlist
        if(this.y.splt[1] == "#" & this.y.splt[nc.y] == "#") {
          out <- 0
        } else if(this.y.splt[1] == "#") {
          yy2[ind0[1]] <- paste0(c(rep("#", nn2[1]), "."), collpase = "")
        } else if(this.y.splt[nc.y] == "#") {
          yy2[ind0[1]] <- paste0(c(".", rep("#", nn2[1])), collpase = "")
        } 
      }
      
      if(nc.y == nn2[1] + 2) {
        this.y.splt <- strsplit(this.y, "") %>% unlist
        if((this.y.splt[1] == "#" & this.y.splt[nc.y-1] == "#") | 
           (this.y.splt[2] == "#" & this.y.splt[nc.y  ] == "#") | 
           (all(this.y.splt == "?"))) {
          out <- 0
        } else {
          if(this.y.splt[1] == "#"){
            yy2[ind0[1]] <- paste0(c(rep("#", nn2[1]), ".", this.y.splt[nc.y]), collpase = "")
          } else if(this.y.splt[nc.y] == "#") {
            yy2[ind0[1]] <- paste0(c(this.y.splt[1], ".", rep("#", nn2[1])), collpase = "")
          } else {
            yy2[ind0[1]] <- paste0(c(".", rep("#", nn2[1]), "."), collpase = "")
          }
        }
      }
    }
    
    # Ausgabe (ggf. recursion)    
    yy2 <- unlist(strsplit(paste0(yy2, collapse = "."), ""))
    if(tail(yy, 1) == "." & length(yy2) == length(yy) - 1) yy2 <- c(yy2, ".")
    if(length(yy2) != length(yy)) stop(4)
    if(paste0(yy, collapse = "") == paste0(yy2, collapse = "") | 
       all(yy2 != "?")){
      return(yy2)
    } else {
      # recursion 3
      return(replace_schlau(yy2, n))
    }
  }
  
    
    
  check <- function(yy, n){
    #yy <- y; n <- tmp[2]
    
    n1 <- n2 <- n0 <- 0
    if(any(yy == "?")) {
      
      i <- which(yy=="?")
      i <- i[ceiling(length(i) / 2)] # split in der Mitte
      k0 <- replace_schlau(yy, n)
      k1 <- k2 <- k0
      k1[i] <- "."
      k2[i] <- "#"
      
      #
      k1 <- replace_schlau(k1, n)
      k2 <- replace_schlau(k2, n)
      
      #
      if(all(k1[yy == "#"] == "#")) n1 <- check(k1, n)
      if(all(k2[yy == "#"] == "#")) n2 <- check(k2, n)
      
      n1 + n2
      
    } else {
      
      yy <- unlist(strsplit(paste0(yy, collapse = ""), "[.]"))
      yy <- yy[yy != ""]
      n0 <- 1 * (paste0(nchar(yy), collapse = ",") == n)
      
    }
    n1 + n2 + n0
    
  }
  
  check(y, tmp[2]) 
  
}
