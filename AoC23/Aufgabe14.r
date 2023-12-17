# einlesen ----
library(dplyr)
x <- readLines("AoC23/Aufgabe14.txt")
head(x)

y <- as.matrix((strsplit(x, split = "") %>% Reduce("rbind", .)))
dimnames(y) <- list(NULL, NULL)
y[y == "."] <- NA
y[] <- car::Recode(y[], "'O'='1';'#'='0'")
storage.mode(y) <- "numeric"

# a ----
calc_support <- function(mx) {
  cs <- which(mx == 1, arr.ind = TRUE)
  p0 <- which(mx == "0", arr.ind = TRUE)
  out <- 0
  for(ii in 1:ncol(mx)) {# ii <- 1
    thiscs <- cs[cs[,"col"] == ii,"row"]
    thisp0 <- c(0, p0[p0[,"col"] == ii, "row"])
    if(any(thiscs)) {
      gr <- outer(thiscs, thisp0, `>`)
      gr <- c(crossprod(t(gr), rep(1, ncol(gr))))
      ind <- lapply(unique(gr), function(gg) seq(from = thisp0[gg] + 1, by = 1, length.out = sum(gr == gg)))
      out <- out + sum(nrow(mx) - unlist(ind) + 1)
    }
  }
  out
}

calc_support(y)

# b ----
# if(FALSE) {
suppsyy <- numeric(100)
suppszz <- NULL
  tilt <- function(mx) {# mx <- y
    cs <- which(mx == 1, arr.ind = TRUE)
    p0 <- which(mx == "0", arr.ind = TRUE)
    mx[cs] <- NA
    for(ii in 1:ncol(mx)) {# ii <- 1
      thiscs <- cs[cs[,"col"] == ii,"row"]
      thisp0 <- c(0, p0[p0[,"col"] == ii, "row"])
      if(any(thiscs)) {
        gr <- outer(thiscs, thisp0, `>`)
        gr <- c(crossprod(t(gr), rep(1, ncol(gr))))
        ind <- lapply(unique(gr), function(gg) seq(from = thisp0[gg] + 1, by = 1, length.out = sum(gr == gg)))
        
        mx[unlist(ind), ii] <- 1
      }
    }
    mx
  }
  
  rotate_inv <- function(mx) {
    nc <- ncol(mx)
    nr <- nrow(mx)
    mx <- mx[, nc:1]
    t(mx)
  } 
  rotate <- function(mx) {
    mx <- t(mx)
    nc <- ncol(mx)
    mx <- mx[, nc:1]
    mx
  } 
  calc_supp_neu <- function(mx) sum(which(tilt(mx) == 1, arr.ind = TRUE)[,"row"])
  yy <- y
  for(kk in seq_along(suppsyy)) {
    suppsyy[kk] <- calc_supp_neu(yy)
    
    n <- tilt(yy)
    w <- tilt(rotate(n))
    s <- tilt(rotate(w))  
    e <- tilt(rotate(s))
    yy <- rotate(e)
    suppszz <- rbind(suppszz, data.frame(calc_supp_neu(yy), calc_supp_neu(n), calc_supp_neu(w), calc_supp_neu(s), calc_supp_neu(e)))
  }
# }

# b Versuch 2 ---- 
mx <- y
cs <- which(mx == 1, arr.ind = TRUE)
p0 <- which(mx == "0", arr.ind = TRUE)

leercs <- cs; leercs[] <- NA
leerp0 <- p0; leerp0[] <- NA

p0s <- vector(mode = "list", length = 4)
p0s[[1]] <- p0
for(rr in 2:4) {
  # rotate p0
  thisdd <- dd[(rr-1) %% 2 + 1]
  thisp0 <- leerp0
  thisp0[, "row"] <- p0[, "col"]
  thisp0[, "col"] <- thisdd - p0[, "row"] + 1
  p0 <- thisp0
  p0 <- p0[order(p0[,"row"], p0[,"col"]),]
  p0s[[rr]] <- p0
}
ind0s <- vector(mode = "list", length = 4)
for(rr in 1:4) {
  cls <- p0s[[rr]][,"col"]
  this <- vector(mode = "list", length = length(unique(cls)))
  thisp0 <- p0s[[rr]]
  for(cc in 1:max(dim(y))) {
    this[[cc]] <- sort(c(0, thisp0[thisp0[,"col"] == cc, "row"]), decreasing = TRUE)
  }
  ind0s[[rr]] <- this
}

mx[] <- NA

dd <- dim(mx)
supps <- numeric(10000)
for(cc in seq_along(supps)) {
  # rotate
  for(rr in 1:4) {# rr <- 1
    
    thisdd <- dd[(rr-1) %% 2 + 1]
    tiltcs <- leercs
    
    # tilt
    indneu <- 1
    for(ii in unique(cs[, "col"])) {# ii <- 1
      ind1 <- cs[cs[,"col"] == ii,"row"]
      ind0 <- ind0s[[rr]][[ii]]
      
      ind <- lapply(seq_along(ind0), function(gg) {
        if(gg == 1) thisi <- ind1 > ind0[gg]
        if(gg > 1 & length(ind0) > 1) thisi <- ind1 > ind0[gg] & ind1 < ind0[gg-1]
        if(any(thisi)) (ind0[gg] + 1):(ind0[gg] + sum(thisi))
      }) %>% unlist()
      indtmp <- indneu:(indneu + length(ind) - 1)
      indneu <- indneu + length(ind)
      
      tiltcs[indtmp, "row"] <- ind
      tiltcs[indtmp, "col"] <- ii
      
    }
    
    # rotate cs
    cs[,"row"] <- tiltcs[, "col"]
    cs[,"col"] <- thisdd - tiltcs[, "row"] + 1
    
    if(rr == 3) supps[cc] <- sum(cs[, "row"])
    # if(rr == 3) supps[cc] <- sum(tiltcs[, "row"])
  }
}
table(supps)

## 
supps1 <- supps[1:(length(supps)/2)]

# burnin
names(which(table(supps1) == 1))
names(which(table(supps) == 1))
names(which(table(supps1) > 2))
names(which(table(supps) > 2))

# cycle
table(supps)[names(which(table(supps) > 2))]
lng <- length(names(which(table(supps) > 2)))
brn <- max(which(!supps %in% names(which(table(supps) > 2))))
supps[brn + 1:lng]
supps[(brn + 1:lng) + lng]

(1000000000 - brn - 1) %% lng + 1
supps[(100 - brn - 1) %% lng + brn + 1] == supps[100]
supps[(1000000000 - brn - 1) %% lng + brn + 1]


