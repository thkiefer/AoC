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

for(ii in seq_along(x)){#ii <- 6
  
  tmp <- strsplit(x[[ii]], " ") %>% unlist()
  y <- strsplit(tmp[1], "") %>% unlist
  n <- as.numeric(unlist(strsplit(tmp[2], ",")))
  ind <- which(y == "?")
  
  replace_schlau_block <- function(this.y, nn3) {
    out <- this.y
    nc.y <- nchar(this.y)
    
    if(nc.y < nn3) {
      if(length(grep("#", this.y)) > 0) {
        out <- 0
      } else {
        out <- paste0(rep(".", nc.y), collapse = "")
      }
    }
    
    # if(nc.y == nn3) {
    #   out <- paste0(rep("#", nc.y), collapse = "")
    # }
    
    if(nc.y == nn3 + 1) {
      this.y.splt <- strsplit(this.y, "") %>% unlist
      if(this.y.splt[1] == "#" & this.y.splt[nc.y] == "#") {
        out <- 0
      } else if(this.y.splt[1] == "#") {
        out <- paste0(c(rep("#", nn3), "."), collapse = "")
      } else if(this.y.splt[nc.y] == "#") {
        out <- paste0(c(".", rep("#", nn3)), collapse = "")
      } 
    }
    
    if(nc.y == nn3 + 2) {
      this.y.splt <- strsplit(this.y, "") %>% unlist
      if(this.y.splt[1] == "#") {
        if(this.y.splt[nc.y-1] == "#") out <- 0 else {
          out <- paste0(c(rep("#", nn3), ".", this.y.splt[nc.y]), collapse = "")
        }
      } 
      if(this.y.splt[1] == "?" & this.y.splt[2] == "#"){
        if(this.y.splt[nc.y-1] == "#") {
          if(this.y.splt[nc.y] == "#") out <- 0 else {
            out <- paste0(c(".", rep("#", nn3), "."), collapse = "")
          }
        }
        if(this.y.splt[nc.y-1] == "?") {
          if(this.y.splt[nc.y] == "#") {
            out <- paste0(c(rep("#", nn3), ".", this.y.splt[nc.y]), collapse = "")
          }
        }
      }
      if(this.y.splt[1] == "?" & this.y.splt[2] == "?"){
        if(this.y.splt[nc.y-1] == "#" & this.y.splt[nc.y] == "#") {
          out <- paste0(c("..", rep("#", nn3)), collapse = "")
        }
      }
    }
    out
  }
  
  replace_schlau <- function(yy, n){# yy <- k2; n <- tmp[2]
    
    yy2 <- yy
    nn2 <- strsplit(n, ",") %>% unlist %>% as.numeric
    yy2 <- strsplit(paste0(yy2, collapse = ""), "[.]") %>% unlist()
    if(tail(yy, 1) == ".") yy2 <- c(yy2, "")
    
    #
    if(length(nn2) < sum(yy2 != "")) {
      repl <- grep("^[?]+$", yy2)[1]
      if(!is.na(repl)) {
        yy2[repl] <- ""
        if(length(repl) == 1 & repl == length(yy2)) yy2 <- c(yy2, "")
      } else return(0)
    } 
    if(length(nn2) < sum(yy2 != "")) {
      stop(1)
    } 
    if(length(grep("#", yy2)) == length(nn2)) yy2[-grep("#", yy2)] <- ""
    if(sum(nn2) > sum(yy != ".")) return(0)
    
    
    # first
    ind0 <- which(yy2 != "")
    this.i <- ind0[1]
    if(length(grep("[?]", yy2[this.i])) > 0) {
      out <- replace_schlau_block(yy2[this.i], nn2[1])
      if(out != 0) yy2[this.i] <- out else return(out)
    }
    
    # last
    ind0 <- which(yy2 != "")
    this.i <- ind0[length(ind0)]
    if(length(grep("[?]", yy2[this.i])) > 0 & length(ind0) > 1) {
      rev_string <- function(ss) strsplit(ss, "") %>% unlist %>% rev %>% paste0(collapse = "")
      out <- replace_schlau_block(yy2[this.i] %>% rev_string, 
                                  tail(nn2, 1))
      if(out != 0) yy2[this.i] <- rev_string(out) else return(out)
    }
    
    # recursion 1 only tail
    # head vollständig aufgelöst
    ind0 <- which(yy2 != "")
    this.y <- yy2[ind0[1]]
    if(length(grep("[?]", this.y)) == 0 & length(ind0) > 1 & length(grep("[?]", yy2[-ind0[1]])) > 0) {
      yy2t <- paste0(yy2[-(1:ind0[1])], collapse = ".")
      y.tmp <- replace_schlau(unlist(strsplit(yy2t, "")), paste0(nn2[-1], collapse = ","))
      if(identical(y.tmp, 0)) return(0)
      yy2 <- c(yy2[1:ind0[1]],
               y.tmp %>% paste0(., collapse = "") %>% strsplit("[.]") %>% unlist(),
               if(tail(y.tmp, 1) == ".") "")
      
      trail. <- (strsplit(paste0(yy2, collapse = "."), "") %>% unlist() %>% tail(1)) == "."
      yy2 <- c(strsplit(paste0(yy2, collapse = "."), "[.]") %>% unlist(),
               if(trail.) "")
    }
    
    # recursion 2 only head
    # tail vollständig aufgelöst
    ind0 <- which(yy2 != "")
    this.i <- ind0[length(ind0)]
    this.y <- yy2[this.i]
    if(length(grep("[?]", this.y)) == 0 & length(ind0) > 1 & length(grep("[?]", yy2[-this.i])) > 0) {
      yy2h <- paste0(yy2[-(this.i:length(yy2))], collapse = ".")
      y.tmp <- replace_schlau(unlist(strsplit(yy2h, "")), paste0(nn2[-length(nn2)], collapse = ","))
      if(identical(y.tmp, 0)) return(0)
      yy2 <- c(y.tmp %>% paste0(., collapse = "") %>% strsplit("[.]") %>% unlist(),
               if(tail(y.tmp, 1) == ".") "",
               yy2[(this.i:length(yy2))])
      
      trail. <- (strsplit(paste0(yy2, collapse = "."), "") %>% unlist() %>% tail(1)) == "."
      yy2 <- c(strsplit(paste0(yy2, collapse = "."), "[.]") %>% unlist(),
               if(trail.) "")
    }
    
    # Ausgabe (ggf. recursion)    
    yy2 <- unlist(strsplit(paste0(yy2, collapse = "."), ""))
    if(length(yy2) == length(yy) - 1) yy2 <- c(yy2, ".")
    if(length(yy2) != length(yy)) stop(4)
    if(paste0(yy, collapse = "") == paste0(yy2, collapse = "") | 
       all(yy2 != "?")){
      return(yy2)
    } else {
      # recursion 3
      return(replace_schlau(yy2, n))
    }
  }
  
  check_n <- function(yy, n) {
    yy <- unlist(strsplit(paste0(yy, collapse = ""), "[.]"))
    yy <- yy[yy != ""]
    n0 <- 1 * (paste0(nchar(yy), collapse = ",") == n)
  }
  
  check <- function(yy, n){
    #yy <- y; n <- tmp[2]
    
    ny <- n0 <- n1 <- n2 <- 0
    if(all(yy != "?")) {
      ny <- check_n(yy, n)
    } else {
      k0 <- replace_schlau(yy, n)
      
      if(all(k0 != "?")) {
        n0 <- check(k0, n)
      } else {
        
        i <- which(k0 == "?")
        i <- i[ceiling(length(i) / 2)] # split in der Mitte
        
        k1 <- k2 <- k0
        k1[i] <- "."
        k2[i] <- "#"
        
        #
        k1 <- replace_schlau(k1, n)
        k2 <- replace_schlau(k2, n)
        
        #
        if(!identical(k1, 0)) n1 <- check(k1, n)
        if(!identical(k2, 0)) n2 <- check(k2, n)
        
        n1 + n2
      }
    }
    
    ny + n0 + n1 + n2
    
  }
  
  treffer[ii] <- check(y, tmp[2]) 
  
}
