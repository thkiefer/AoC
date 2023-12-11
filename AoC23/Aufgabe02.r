x <- readLines("Aufgabe2.txt")
head(x)
library(plyr)
library(dplyr)

df <- lapply(seq_along(x), function(ii) {
  xx <- gsub("Game \\d+:", "", x[ii])
  xx <- strsplit(xx, ";")
  lapply(seq_along(xx[[1]]), function(jj) {
    yy <- unlist(strsplit(xx[[1]][jj], ","))
    data.frame(id = ii, subset = jj,
               as.list(setNames(as.numeric(gsub("\\D", "", yy)), 
                                gsub("[^[:alpha:]]", "", yy)))
    )
  }) %>% Reduce("rbind.fill", .)
}) %>% Reduce("rbind.fill", .)
df[is.na(df)] <- 0

# a
df$a <- df[, "red"] <= 12 & df[, "green"] <= 13 & df[, "blue"] <= 14
tmp <- aggregate(df$a, df["id"], mean)
sum(tmp$id[tmp$x == 1])

# b
tmp <- aggregate(df[, c("red", "green", "blue")], df["id"], max)
tmp$pow <- tmp$red * tmp$green * tmp$blue
sum(tmp$pow)
