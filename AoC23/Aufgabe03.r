x <- readLines("Aufgabe3.txt")
head(x)
library(dplyr)
xx <- lapply(x, function(i) unlist(strsplit(i, ""))) %>% Reduce("rbind", .) %>% as.matrix
xx[xx == "."] <- NA


ind <- which(!xx %in% as.character(0:9) & !is.na(xx), arr.ind = TRUE)
ind <- ind[order(ind[, "row"], ind[, "col"]),]
head(ind)
head(xx[ind])

und <- rbind(t(t(ind) + c(-1, -1)),
             t(t(ind) + c(-1,  0)),
             t(t(ind) + c(-1,  1)),
             t(t(ind) + c( 0, -1)),
             t(t(ind) + c( 0,  0)),
             t(t(ind) + c( 0,  1)),
             t(t(ind) + c( 1, -1)),
             t(t(ind) + c( 1,  0)),
             t(t(ind) + c( 1,  1)))
und <- data.frame("chr" = xx[ind], setNames(as.data.frame(ind), c("rx", "cx")), und, row.names = NULL)
und$nu <- xx[as.matrix(und[, c("row", "col")])]
und$cols <- und$val <- NA
und <- und[und$nu %in% as.character(0:9), ]
und <- und[order(und[, "row"], und[, "col"]),]
head(und)

for(uu in 1:nrow(und)){# uu <- 1
  lookup <- which(!xx[und[uu, "row"],] %in% as.character(0:9))
  thisc <- und[uu, "col"]
  sq <- thisc - lookup
  sq <- c(if(any(sq > 0)) lookup[which(sq == min(sq[sq > 0]))] + 1 else 1, 
          if(any(sq < 0)) lookup[which(sq == max(sq[sq < 0]))] - 1 else ncol(xx) 
          )
  sq <- seq(from = sq[1], to = sq[2], by = 1)
  und$val[uu] <- as.numeric(paste0(xx[und[uu, "row"], sq], collapse = ""))
  und$cols[uu] <- paste0(sq, collapse = ", ")
}

head(which(duplicated(und[, c("row", "cols")])))
und[und$row == und$row[5] & und$cols == und$cols[6], ]
und <- und[!duplicated(und[, c("row", "cols")]),]

# a
sum(und$val)

# b
and <- merge(und, und[duplicated(und[, c("chr", "rx", "cx")]),c("chr", "rx", "cx")])
sum(aggregate(and["val"], and[,c("rx", "cx")], prod)[, "val"])


