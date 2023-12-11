# einlesen ----
library(dplyr)
x <- readLines("Aufgabe10.txt")
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
d <- 0
n <- start[1]
i <- ind0 + dir[[n]]


while(any(i != ind0)) {
  # a
  d <- d + 1
  p <- y[i]
  n <- next_i[[n]][p]
  i <- i + dir[[n]]
  
  z[i] <- y[i]
}

(maxd <- (d + 1) / 2)

# b
i <- ind0
z[i] <- y[i]

n <- start[1]
i <- ind0 + dir[[n]]

while(any(i != ind0)) {
  p <- y[i]

  #
  z[i] <- p
  
  indeq <- which(z == y, arr.ind = TRUE)
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

  z[rbind(nachoben, nachrechts)] <- as.character(as.numeric(z[rbind(nachoben, nachrechts)]) - ifelse(n %in% c("rechts", "unten"), 1, -1))
  z[rbind(nachunten, nachlinks)] <- as.character(as.numeric(z[rbind(nachunten, nachlinks)]) + ifelse(n %in% c("rechts", "unten"), 1, -1))
  
  cat("\f")
  print(z, quote = FALSE)
  Sys.sleep(.3)
  #
  n <- next_i[[n]][p]
  i <- i + dir[[n]]
  
}

print(z, quote = FALSE)
print(y, quote = FALSE)

