# einlesen ----
library(dplyr)
x <- readLines("AoC23/Aufgabe16.txt")
head(x)

y <- strsplit(x, split = "") %>% Reduce("rbind",.) 
rownames(y) <- NULL
z <- y
z[] <- 0
storage.mode(z) <- "numeric"
visited <- setNames(rep(list(z), 4), c("up", "down", "left", "right"))

# a ----
next_pos <- function(direction, action) {
  if(direction == "right") return(switch(action,
                                  "/"  = "up",
                                  "\\" ="down",
                                  "|"  = c("up", "down"),
                                  direction))
  if(direction == "left") return(switch(action,
                                 "/"  = "down",
                                 "\\" ="up",
                                 "|"  = c("up", "down"),
                                 direction))
  if(direction == "down") return(switch(action,
                                 "/"  = "left",
                                 "\\" ="right",
                                 "-"  = c("left", "right"),
                                 direction))
  if(direction == "up") return(switch(action,
                                 "/"  = "right",
                                 "\\" ="left",
                                 "-"  = c("left", "right"),
                                 direction))
}
next_ind <- function(pos, direction) {
  return(pos + switch(direction, "right" = c(0, 1), "left" = c(0, -1), "up" = c(-1, 0), "down" = c(1, 0)))
}

beam <- function(y, visited, pos, direction) {
  
  pos0 <- matrix(NA, ncol = 2, nrow = 10, dimnames = list(NULL, c("row", "col")))
  direction0 <- character(10);
  
  ii <- length(direction)
  thispos <- pos
  thisdir <- direction
  lauf <- TRUE
  
  while(lauf) {
    
    # Stop
    if(any(thispos[ii,] < c(1,1) | thispos[ii,] > dim(y))) {
      lauf <- FALSE
    } else if(visited[[thisdir[ii]]][thispos[ii,, drop = FALSE]] == 1) {
      lauf <- FALSE
    } else {
      
      # next i
      visited[[thisdir[ii]]][thispos[ii,, drop = FALSE]] <- 1
      
      # Sys.sleep(.5)
      cat("\r", ii, ", ", paste0(thispos[ii,], collapse = ","), ", ", thisdir[ii])
      ii <- ii + 1
      
      # strech
      if(ii > length(thisdir)) {
        thisdir <- c(thisdir, direction0)
        thispos <- rbind(thispos, pos0)
      }
      
      #
      np <- next_pos(thisdir[ii-1], y[thispos[ii-1,,drop = FALSE]])
      if(length(np) == 1) {
        
        # set next values
        thisdir[ii] <- np
        thispos[ii,] <- next_ind(thispos[ii - 1,], thisdir[ii])
        
      } else {
        
        # split 1
        thisdir[ii] <- np[1]
        merkn_pos <- thispos[ii - 1,]
        thispos[ii,] <- next_ind(merkn_pos, thisdir[ii])
        bm <- beam(y, visited, thispos[1:ii,], thisdir[1:ii])
        
        visited <- bm[[1]]
        thispos <- bm[[2]]
        thisdir <- bm[[3]]
        
        # split 2
        thispos <- rbind(thispos, pos0[1,])
        thisdir <- c(thisdir, direction0[1])
        ii <- length(thisdir)
        
        thisdir[ii] <- np[2]
        thispos[ii,] <- next_ind(merkn_pos, thisdir[ii])
        bm <- beam(y, visited, thispos[1:ii,], thisdir[1:ii])
        
        visited <- bm[[1]]
        thispos <- bm[[2]]
        thisdir <- bm[[3]]
      }
    }
    
  }
  
  return(list(visited, thispos[1:ii,], thisdir[1:ii]))
}

pos <- matrix(c(1, 1), ncol = 2, nrow = 1, dimnames = list(NULL, c("row", "col")))
direction <- c("right")
bm <- beam(y, visited, pos, direction)

sum(Reduce(`+`, bm[[1]]) > 0)

# b
out <- setNames(rep(list(numeric(100)), 4), names(visited))
dim(y)
for(kk in 50:110) {
  
  pos[] <- c(1, kk)
  out[[1]][kk] <- sum(Reduce(`+`, beam(y, visited, pos, "down")[[1]]) > 0)
  pos[] <- c(kk, 1)
  out[[2]][kk] <- sum(Reduce(`+`, beam(y, visited, pos, "right")[[1]]) > 0)
  pos[] <- c(kk, 110)
  out[[3]][kk] <- sum(Reduce(`+`, beam(y, visited, pos, "left")[[1]]) > 0)
  pos[] <- c(110, kk)
  out[[4]][kk] <- sum(Reduce(`+`, beam(y, visited, pos, "up")[[1]]) > 0)
  
  cat("\r\n", kk, ", ", max(unlist(out)), "\n")
}




