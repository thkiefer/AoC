# einlesen ----
library(dplyr)
x <- readLines("AoC23/Aufgabe15.txt")
head(x)

y <- strsplit(x, split = ",") %>% unlist

# a ----
my_ASCII <- function(s) sapply(s, function(ss) strtoi(charToRaw(ss), 16L))
my_fn <- function(a, b) (a * 17) %% 256 + b
hash_tbl <- function(s) s %>% strsplit("") %>% unlist %>% my_ASCII() %>% `c`(., 0) %>% Reduce("my_fn", .)
sum(sapply(y, hash_tbl))

# b ----
z <- data.frame("code" = character(0), "lens" = character(0), "focal.length" = numeric(0), "hash" = numeric(0))
ind <- grep("=", y)
z[ind, 1] <- "="
z[ind, 2] <- strsplit(y[ind], "=") %>% lapply("[", 1) %>% unlist
z[ind, 3] <- strsplit(y[ind], "=") %>% lapply("[", 2) %>% unlist %>% as.numeric
ind <- grep("-", y)
z[ind, 1] <- "-"
z[ind, 2] <- strsplit(y[ind], "-") %>% Reduce("rbind",.)
z[, 4] <- as.numeric(sapply(z[,2], hash_tbl))

box <- rep(list(z[0,c("lens", "focal.length")]), 256)
for(ii in 1:nrow(z)) {# ii <- 1
  thisz <- z[ii,]
  idbox <- thisz$hash + 1
  mt <- match(thisz$lens, box[[idbox]]$lens)
  if(z$code[ii] == "-") {
    if(!is.na(mt) > 0) box[[idbox]] <- box[[idbox]][-mt,]
  } else {
    if(!is.na(mt) > 0) box[[idbox]]$focal.length[mt] <- thisz$focal.length else box[[idbox]] <- rbind(box[[idbox]], thisz[, c("lens", "focal.length")])
  }
}

lapply(seq_along(box), function(ii) {
  if(nrow(box[[ii]]) > 0) ii * (1:nrow(box[[ii]])) * box[[ii]]$focal.length
}) %>% unlist %>% sum
