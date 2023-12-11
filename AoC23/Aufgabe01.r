# a
x <- read.table("Aufgabe1.txt")
head(x)

zahl <- 0
for(i in 1:1000) {#i <- 1
  splt <- strsplit(x[i,], "")[[1]]
  
  ind <- which(splt %in% 1:9)
  zehnerstelle <- splt[min(ind)]
  einerstelle <- splt[max(ind)]
  zahl_i <- as.numeric(zehnerstelle) * 10 + as.numeric(einerstelle)
  zahl <- zahl + zahl_i
}


# b
x <- read.table("Aufgabe1.txt")
head(x)
zahl <- 0
for(i in 1:1000) {#i <- 15
  
  splt <- x[i, ]
  
  zehnerstelle <- list("1" = gsub("one", "1", splt),
                       "2" = gsub("two", "2", splt),
                       "3" = gsub("three", "3", splt),
                       "4" = gsub("four", "4", splt),
                       "5" = gsub("five", "5", splt),
                       "6" = gsub("six", "6", splt),
                       "7" = gsub("seven", "7", splt),
                       "8" = gsub("eight", "8", splt),
                       "9" = gsub("nine", "9", splt))
  
  
  splt_r <- paste0(rev(strsplit(splt, "")[[1]]), collapse = "")
  einerstelle <- list("1" = gsub("eno", "1", splt_r),
                       "2" = gsub("owt", "2", splt_r),
                       "3" = gsub("eerht", "3", splt_r),
                       "4" = gsub("ruof", "4", splt_r),
                       "5" = gsub("evif", "5", splt_r),
                       "6" = gsub("xis", "6", splt_r),
                       "7" = gsub("neves", "7", splt_r),
                       "8" = gsub("thgie", "8", splt_r),
                       "9" = gsub("enin", "9", splt_r))

  zehnerstelle_zahl <- zehnerstelle
  einerstelle_zahl <- einerstelle
  
  for(j in 1:9) {# j <- 1
    zehnerstelle_zahl[[j]] <- min(which(strsplit(zehnerstelle[[j]], "")[[1]] == j))
    einerstelle_zahl[[j]] <- min(which(strsplit(einerstelle[[j]], "")[[1]] == j))
  }
  
  
  zahl_i <- as.numeric(names(which.min(unlist(zehnerstelle_zahl)))) * 10 + 
    as.numeric(names(which.min(unlist(einerstelle_zahl))))
  zahl <- zahl + zahl_i
}

zahl
