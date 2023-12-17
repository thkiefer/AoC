# einlesen ----
library(dplyr)
x <- readLines("AoC23/Aufgabe12.txt")
head(x)

# a ----
check_n <- function(yy, n){
  yy <- unlist(strsplit(paste0(yy, collapse = ""), "[.]"))
  yy <- yy[yy != ""]
  paste0(nchar(yy), collapse = ",") == n
} 

if(TRUE) {
system.time({
treffer_a <- numeric(length(x))
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
      check_n(y.tmp, tmp[2])
      
      treffer_a[ii] <- treffer_a[ii] + check_n(y.tmp, tmp[2])
    }
  }
})
}

# b ----
if(FALSE){
system.time({
  treffer_b <- numeric(length(x))
  source("AoC23/Aufgabe12_aux.r")
  for(ii in seq_along(x)){#ii <- 1
    
    tmp <- strsplit(x[[ii]], " ") %>% unlist()
    y <- strsplit(tmp[1], "") %>% unlist
    n <- as.numeric(unlist(strsplit(tmp[2], ",")))
    ind <- which(y == "?")
    
    treffer_b[ii] <- check(y, tmp[2]) 
    
  }
})
}

if(FALSE){
system.time({
  treffer_c <- numeric(length(x))
  source("AoC23/Aufgabe12_aux.r")
  for(ii in seq_along(x)){#ii <- 1
    
    tmp <- strsplit(x[[ii]], " ") %>% unlist()
    tmp[1] <- paste0(rep(tmp[1], 5), collapse = "?")
    tmp[2] <- paste0(rep(tmp[2], 5), collapse = ",")
    y <- strsplit(tmp[1], "") %>% unlist
    n <- as.numeric(unlist(strsplit(tmp[2], ",")))
    ind <- which(y == "?")
    
    treffer_c[ii] <- check(y, tmp[2]) 
    
  }
})
}

# b V2 ----
treffer_b2 <- numeric(length(x))
arrangements <- arrangement <- NULL
source("AoC23/Aufgabe12_aux.r")
for(ii in seq_along(x)){#ii <- 1
  
  tmp <- strsplit(x[[ii]], " ") %>% unlist()
  y <- strsplit(tmp[1], "") %>% unlist
  ind <- which(y == "?")
  
  treffer_b2[ii] <- check_a(y, tmp[2]) 
  if(nrow(arrangement) != treffer_b2[ii]) stop()
  arrangements[[ii]] <- arrangement
  arrangement <- NULL
}

treffer_c2 <- numeric(length(x))
arrangements <- arrangement <- NULL
source("AoC23/Aufgabe12_aux.r")
for(ii in seq_along(x)){#ii <- 2
  
  tmp <- strsplit(x[[ii]], " ") %>% unlist()
  ys <- arrangements[[ii]]
  ys <- apply(ys, 1, function(yyss) {
    paste0(rep(paste0(yyss, collapse = ""), 5), collapse = "?") %>% strsplit("") %>% unlist
  }) %>% t() %>% unique()
  tmp[2] <- paste0(rep(tmp[2], 5), collapse = ",")

  lapply(1:nrow(ys), function(jj) {
    check(ys[jj,], tmp[2]) 
  })
 
  
}
