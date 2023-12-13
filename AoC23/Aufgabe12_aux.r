rev_string <- function(ss) strsplit(ss, "") %>% unlist %>% rev %>% paste0(collapse = "")

replace_schlau_block <- function(this.y, nn3) {
  
  out <- this.y
  nc.y <- nchar(this.y)

  if(is.na(nn3) | nc.y < nn3) {
    if(length(grep("#", this.y)) > 0) return(0) else {
      return(paste0(nc.y, collapse = ""))
    }
  }
  
  if(nc.y == nn3) {
    if(length(grep("#", this.y)) > 0) return(paste0(rep("#", nc.y), collapse = ""))
  } 
  
  if(substr(this.y, 1, 1) == "#") {
    if(substr(this.y, nn3+1, nn3+1) == "#") return(0) else {
      return(paste0(paste0(rep("#", nn3), collapse = ""), ".", substr(this.y, nn3+2, nc.y)))
    }
  }
  
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

replace_schlau <- function(yy, n){# yy <- k2; n <- n
  
  yy2 <- yy
  nn2 <- strsplit(n, ",") %>% unlist %>% as.numeric
  yy2 <- strsplit(paste0(yy2, collapse = ""), "[.]") %>% unlist()
  
  # first
  ind0 <- which(yy2 != "")
  if(length(grep("[?]", yy2[ind0[1]])) > 0) {
    out <- replace_schlau_block(yy2[ind0[1]], nn2[1])
    if(identical(out, 0)) return(out) else yy2[ind0[1]] <- out
    if(length(grep("^\\d+$", yy2[ind0[1]])) > 0) {
      i <- ind0[1]
      d <- as.numeric(yy2[i])
      yy2[i] <- ""
      yy2 <- yy2[rep(seq_along(yy2), 1 + d * (seq_along(yy2) == i))]
    }
  }
  
  # recursion 1 only tail
  # head vollständig aufgelöst
  ind0 <- which(yy2 != "")
  this.y <- yy2[ind0[1]]
  if(length(grep("[?]", this.y)) == 0 & length(grep("[.]", this.y)) == 0 & length(ind0) > 1 & length(grep("[?]", yy2[-ind0[1]])) > 0) {
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
  
  # Ausgabe (ggf. recursion)    
  yy2 <- unlist(strsplit(paste0(yy2, collapse = "."), ""))
  if(length(yy2) == length(yy) - 1) yy2 <- c(yy2, ".")
  if(length(yy2) != length(yy)) stop(4)
  if(paste0(yy, collapse = "") == paste0(yy2, collapse = "") | 
     all(yy2 != "?")){
    return(yy2)
  } else {
    # recursion 2
    return(replace_schlau(yy2, n))
  }
}

check <- function(yy, n){
  #yy <- k1; n <- tmp[2]
  
  ny <- n0 <- n1 <- n2 <- 0
  if(all(yy != "?")) {
    ny <- check_n(yy, n)
  } else {
    k0 <- replace_schlau(yy, n)
    
    if(all(k0 != "?") & !identical(k1, 0)) {
      n0 <- check(k0, n)
    } else {
      
      i <- which(k0 == "?")[1]

      k1 <- k2 <- k0
      k1[i] <- "."
      k2[i] <- "#"
      
      #
      n1 <- check(k1, n)
      n2 <- check(k2, n)
    }
  }
  
  ny + n0 + n1 + n2
  
}

check_a <- function(yy, n){
  #yy <- y; n <- tmp[2]
  
  ny <- n0 <- n1 <- n2 <- 0
  if(all(yy != "?")) {
    ny <- check_n(yy, n)
    if(ny) assign("arrangement", rbind(get("arrangement", .GlobalEnv), yy), envir = .GlobalEnv)
  } else {
    k0 <- replace_schlau(yy, n)
    
    if(all(k0 != "?") & !identical(k1, 0)) {
      n0 <- check(k0, n)
      if(n0) assign("arrangement", rbind(get("arrangement", .GlobalEnv), k0), envir = .GlobalEnv)
    } else {
      
      i <- which(k0 == "?")[1]
      
      k1 <- k2 <- k0
      k1[i] <- "."
      k2[i] <- "#"
      
      #
      n1 <- check_a(k1, n)
      n2 <- check_a(k2, n)
    }
  }
  
  ny + n0 + n1 + n2
  
}
