# einlesen ----
library(dplyr)
library(crayon)
x <- readLines("A0C23/Aufgabe10.txt")
head(x)

y <- lapply(x, function(xx) unlist(strsplit(xx, ""))) %>% Reduce("rbind", .)
dimnames(y) <- list(rep("", nrow(y)), rep("", ncol(y)))
z <- y
z[] <- "0"
allind <- which(y == y, arr.ind = TRUE)

oben <- c(-1, 0); rechts <- c(0, 1); unten <- c(1, 0); links <- c(0, -1)
dir <- list("oben" = oben, "rechts" = rechts, "unten" = unten, "links" = links)
ind0 <- which(y == "S", arr.ind = TRUE)

start <- c("oben", "rechts", "unten", "links")[c(
  any(y[ind0+oben] %in% c("|", "F", "7")),
  any(y[ind0+rechts] %in% c("-", "J", "7")),
  any(y[ind0+unten] %in% c("|", "J", "L")),
  any(y[ind0+links] %in% c("-", "F", "L")))]

next_i <- list(
  "oben" = c("|" = "oben", "F" = "rechts", "7" = "links"),
  "rechts" = c("-" = "rechts", "J" = "oben", "7" = "unten"),
  "unten" = c("|" = "unten", "J" = "links", "L" = "rechts"),
  "links" = c("-" = "links", "F" = "unten", "L" = "oben")
)

# a
z[ind0] <- y[ind0]

d <- 0
n <- start[1]
i <- ind0 + dir[[n]]
z[i] <- y[i]

while(any(i != ind0)) {
  # a
  d <- d + 1
  p <- y[i]
  n <- next_i[[n]][p]
  i <- i + dir[[n]]
  
  z[i] <- y[i]
}

(maxd <- (d + 1) / 2)

# b ----
#
getInd <- function() {
  nachoben <- nachunten <- nachrechts <- nachlinks <- NULL
  if(p %in% c("-", "7", "F")){
    mauer <- indeq[indeq[, "col"] == i[, "col"],,drop = FALSE]
    if(i[, "row"] > 1) nachoben <- cbind(seq(max(1, mauer[,"row"][mauer[,"row"] < i[, "row"]]+1), i[, "row"]-1), i[, "col"])
    if(nrow(mauer) > 0) if(any(mauer[,"row"] == i[, "row"] - 1)) nachoben <- NULL
  }
  if(p %in% c("-", "L", "J")){
    mauer <- indeq[indeq[, "col"] == i[, "col"],,drop = FALSE]
    if(i[, "row"] < nrow(y)) nachunten <- cbind(seq(i[, "row"]+1, min(nrow(y), mauer[,"row"][mauer[,"row"] > i[, "row"]]-1)), i[, "col"])
    if(nrow(mauer) > 0) if(any(mauer[,"row"] == i[, "row"] + 1)) nachunten <- NULL
  }
  if(p %in% c("|", "7", "J")){
    mauer <- indeq[indeq[, "row"] == i[, "row"],,drop = FALSE]
    if(i[, "col"] < ncol(y)) nachrechts <- cbind(i[, "row"], seq(i[, "col"]+1, min(ncol(y), mauer[,"col"][mauer[,"col"] > i[, "col"]]-1)))
    if(nrow(mauer) > 0) if(any(mauer[,"col"] == i[, "col"] + 1)) nachrechts <- NULL
  }
  if(p %in% c("|", "F", "L")){
    mauer <- indeq[indeq[, "row"] == i[, "row"],,drop = FALSE]
    if(i[, "col"] > 1) nachlinks <- cbind(i[, "row"], seq(max(1, mauer[,"col"][mauer[,"col"] < i[, "col"]]+1), i[, "col"]-1))
    if(nrow(mauer) > 0) if(any(mauer[,"col"] == i[, "col"] - 1)) nachlinks <- NULL
  }
  list("oben" = nachoben, "unten" = nachunten, "rechts" = nachrechts, "links" = nachlinks)
}

changeOrr <- function() {
  rl <- c("rechts", "links")
  uo <- c("unten", "oben")
  check <- 
    (all(orr %in% c("unten", "links")) | all(orr %in% c("oben", "rechts"))) & p == "7" |
    (all(orr %in% c("unten", "rechts")) | all(orr %in% c("oben", "links"))) & p == "J" | 
    (all(orr %in% c("unten", "links")) | all(orr %in% c("oben", "rechts"))) & p == "L" |
    (all(orr %in% c("oben", "links")) | all(orr %in% c("unten", "rechts"))) & p == "F" | 
    p %in% c("-", "|")
  
  if(!check) {
    if(n %in% uo) {
      orr <- c(intersect(orr, uo), setdiff(rl, orr))
    } else {
      orr <- c(intersect(orr, rl), setdiff(uo, orr))
    } 
  }
  orr
}

#
i <- ind0
indeq <- which(z == y, arr.ind = TRUE)

if(all(start == c("oben", "rechts"))) z[i] <- "L"
if(all(start == c("oben", "unten"))) z[i] <- "|"
if(all(start == c("oben", "links"))) z[i] <- "J"
if(all(start == c("rechts", "unten"))) z[i] <- "F"
if(all(start == c("rechts", "links"))) z[i] <- "-"
if(all(start == c("links", "unten"))) z[i] <- "7"

print(z, quote = FALSE)

# 0
p <- z[i]
n <- start[1]
orr <- start
# orr <- c("links", "oben")
norr <- setdiff(names(ind), orr)
ind <- getInd()

z[Reduce("rbind", ind[orr])] <- as.character(as.numeric(z[Reduce("rbind", ind[orr])]) + 1)
z[Reduce("rbind", ind[norr])] <- as.character(as.numeric(z[Reduce("rbind", ind[norr])]) - 1)

print(z, quote = FALSE)

# 1
i <- i + dir[[n]]
while(any(i != ind0)) {
  
  p <- z[i]
  n0 <- n
  n <- next_i[[n]][p]
  
  #
  orr <- changeOrr()
  norr <- setdiff(names(ind), orr)
  ind <- getInd()
  
  z[Reduce("rbind", ind[orr])] <- as.character(as.numeric(z[Reduce("rbind", ind[orr])]) + 1)
  z[Reduce("rbind", ind[norr])] <- as.character(as.numeric(z[Reduce("rbind", ind[norr])]) - 1)
  
  # cat("\f")
  # xx <- z
  # xx[i] <- paste0("*")
  # if(runif(1, 0, 1) > .9) print(xx, quote = FALSE)
  # print(orr)
  # Sys.sleep(.05)

  #
  i <- i + dir[[n]]
}

print(z, quote = FALSE)
print(y, quote = FALSE)

