# einlesen ----
library(dplyr)
x <- readLines("Aufgabe7.txt")
head(x)

y0 <- sapply(x, strsplit, " ") %>% Reduce("rbind", .) %>% 
  as.data.frame(row.names = NULL) %>% setNames(., c("Hand", "bid"))
y0$bid <- as.numeric(y0$bid)
head(y0)

# a
y <- cbind(y0, 
           lapply(strsplit(y0[, 1], ""), function(yy) {
             factor(yy, levels = c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2")) %>%
               as.list() %>% setNames(., letters[1:5]) %>% as.data.frame()
           }) %>% Reduce("rbind", .),
           type = factor(NA, levels = c("5", "4", "fh", "3", "tp", "2", "1")),
           rank = NA
)

getType <- function(yy) {
  yy <- unlist(yy)
  oo <- rowSums(outer(yy, yy, "=="))
  if(all(oo == 5)) tp <- "5"
  if(any(oo == 4)) tp <- "4"
  if(any(oo == 3)){
    if(any(oo == 2)) tp <- "fh" else tp <- "3"
  } 
  if(!any(oo == 3) & any(oo == 2)){
    if(sum(oo == 2) == 4) tp <- "tp" else tp <- "2"
  } 
  if(all(oo == 1)) tp <- "1"
  tp
}

y$type[] <- apply(y[,letters[1:5]], 1, getType)
y$rank[] <- match(y$Hand, y$Hand[order(y$type, y$a, y$b, y$c, y$d, y$e, decreasing = TRUE)])

sum(y$bid * y$rank)

# b
y <- cbind(y0, 
           lapply(strsplit(y0[, 1], ""), function(yy) {
             factor(yy, levels = c("A", "K", "Q", "T", "9", "8", "7", "6", "5", "4", "3", "2", "J")) %>%
               as.list() %>% setNames(., letters[1:5]) %>% as.data.frame()
           }) %>% Reduce("rbind", .),
           type = factor(NA, levels = c("5", "4", "fh", "3", "tp", "2", "1")),
           rank = NA
)


getType <- function(yy) {
  nj <- sum(yy == "J")
  yy <- unlist(yy)
  yy <- yy[yy != "J"]
  oo <- rowSums(outer(yy, yy, "=="))
  
  if(all(oo == 5 - nj)) {
    tp <- "5"
  } else if(any(oo == 4 - nj)) {
    tp <- "4"
  } else if(any(oo == 3 - nj)){
    if(nj == 0){ if(any(oo == 2)) tp <- "fh" else tp <- "3" }
    if(nj == 1){ if(sum(oo == 2) == 4) tp <- "fh" else tp <- "3" }
    if(nj == 2) tp <- "3"
  } else if(any(oo == 2 - nj)){
    if(nj == 0){ if(sum(oo == 2) == 4) tp <- "tp" else tp <- "2" }
    if(nj == 1) tp <- "2"
  } else {
    tp <- "1"
  }
  tp
}  

y$type[] <- apply(y[,letters[1:5]], 1, getType)
y$rank[] <- match(y$Hand, y$Hand[order(y$type, y$a, y$b, y$c, y$d, y$e, decreasing = TRUE)])

sum(y$bid * y$rank)
