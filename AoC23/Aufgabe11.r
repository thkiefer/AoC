# einlesen ----
library(dplyr)
x <- readLines("AoC23/Aufgabe11.txt")
head(x)
y <- lapply(x, function(xx) gsub("[.]", "", unlist(strsplit(xx, "")))) %>% Reduce("rbind",.)
dimnames(y) <- list(rep("", nrow(y)), rep("", ncol(y)))

indi <- which(rowSums(y == "") == ncol(y))
indj <- which(colSums(y == "") == nrow(y))

# a
i <- which(y == "#", arr.ind = TRUE)
i[, "row"] <- i[, "row"] + colSums(outer(indi, i[, "row"], `<`))
i[, "col"] <- i[, "col"] + colSums(outer(indj, i[, "col"], `<`))

sum(dist(i, method = "manhattan"))

# b
i <- which(y == "#", arr.ind = TRUE)
i[, "row"] <- i[, "row"] + colSums(outer(indi, i[, "row"], `<`)) * (10-1)
i[, "col"] <- i[, "col"] + colSums(outer(indj, i[, "col"], `<`)) * (10-1)

sum(dist(i, method = "manhattan"))

i <- which(y == "#", arr.ind = TRUE)
i[, "row"] <- i[, "row"] + colSums(outer(indi, i[, "row"], `<`)) * (100-1)
i[, "col"] <- i[, "col"] + colSums(outer(indj, i[, "col"], `<`)) * (100-1)

sum(dist(i, method = "manhattan"))

i <- which(y == "#", arr.ind = TRUE)
i[, "row"] <- i[, "row"] + colSums(outer(indi, i[, "row"], `<`)) * (1e6-1)
i[, "col"] <- i[, "col"] + colSums(outer(indj, i[, "col"], `<`)) * (1e6-1)

sum(dist(i, method = "manhattan"))
