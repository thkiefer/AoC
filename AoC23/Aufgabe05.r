library(dplyr)
x <- readLines("Aufgabe5.txt")
head(x)

ind <- grep("map", x)
map <- lapply(seq_along(ind), function(ii) {
  j <- (ind[ii]+1):(ifelse(ii < length(ind), ind[ii+1]-2, length(x)))
  j <- sapply(x[j], strsplit, split = " ") %>% lapply(., as.numeric) %>% 
    Reduce("rbind", .) %>% as.data.frame(.,row.names = NA) %>% setNames(., c("destination", "source", "range"))
  j$source.max <- j$source + j$range - 1
  j <- j[order(j$source),]
  j
}) %>% setNames(., nm = gsub(" map:", "", x[ind]))

# a
y <- vector(mode = "list", length = length(map)+1) %>% setNames(., c("seed", names(map)))
y[[1]] <- gsub("seeds: ", "", x[1]) %>% strsplit(split = " ") %>% unlist %>% as.numeric

get_corresp <- function(mx, ii) {# mx <- map[[5]]; ii <- y[[5]]
  ind <- outer(mx$source.max, ii, ">=") & outer(mx$source, ii, "<=")
  ind <- c(crossprod(ind, 1:nrow(mx)))
  out <- ii
  out[ind > 0] <- mx$destination[ind] + ii[ind > 0] - mx$source[ind]
  if(any(ind == 0)) out[ind == 0] <- ii[ind == 0]
  out
}

for(mm in seq_along(map)){# mm <- 1
  y[[mm + 1]] <- get_corresp(map[[mm]], y[[mm]])
} 

min(y[[length(y)]])

# b
y <- vector(mode = "list", length = length(map)+1) %>% setNames(., c("seed", names(map)))
y[[1]] <- gsub("seeds: ", "", x[1]) %>% strsplit(split = " ") %>% unlist %>% 
  as.numeric %>% matrix(ncol = 2, byrow = TRUE) %>% as.data.frame() %>% setNames(., c("source", "range"))
y[[1]]$source.max <- y[[1]]$source + y[[1]]$range - 1
y[[1]] <- y[[1]][order(y[[1]]$source),]
y[[1]]$index <- 1:nrow(y[[1]])

# clean maps
for(mm in seq_along(map)){# mm <- 1
  mx <- map[[mm]]
  mx <- mx[order(mx$source), ]
  if(!0 %in% mx$source){
    mx <- rbind(data.frame(destination = 0, source = 0, range = min(mx$source), source.max = min(mx$source) - 1),
                mx)
  }
  
  ind <- which(!(mx$source[-1] - 1) %in% mx$source.max)
  if(length(ind) > 0) {
    for(i in ind) {
      this <- data.frame(destination = mx$source.max[i] + 1, 
                         source = mx$source.max[i] + 1, 
                         range = (mx$source[i + 1] - 1) - (mx$source.max[i] + 1) + 1, 
                         source.max = mx$source[i + 1] - 1)
      mx <- rbind(this, mx)
      mx <- mx[order(mx$source), ]
    }
  }
  
  if(!all((mx$source[-1] - 1) %in% mx$source.max) | 
     !all((mx$source.max[-nrow(mx)] + 1) %in% mx$source)){ 
    print(mm)
  } else {
    map[[mm]] <- mx
  }
}

#
get_corresp <- function(mx, ii) {# mx <- map[[5]]; ii <- y[[5]]
  
  mxi <- max(ii$source.max)
  if(mxi > max(mx$source.max)){
    mx <- rbind(mx, 
                data.frame(destination = max(mx$source.max) + 1,
                           source = max(mx$source.max) + 1, 
                           range = mxi - max(mx$source.max), 
                           source.max = mxi))
  }
  
  out <- ii[0,]
  for(j in 1:nrow(ii)) {# j <- 1

    inda <- max(which(mx$source <= ii$source[j]))
    indb <- min(which(mx$source.max >= ii$source.max[j]))
    
    if(indb - inda == 0){
      this <- data.frame(source = mx$destination[inda] + ii$source[j] - mx$source[inda],
                         range =  ii$range[j],
                         source.max = NA,
                         index = paste0(ii$index[j], ".", inda))
    } else {
      this <- data.frame(source = mx$destination[inda] + ii$source[j] - mx$source[inda],
                         range =  mx$source.max[inda] - ii$source[j],
                         source.max = NA,
                         index = paste0(ii$index[j], ".", inda))
      those <- NULL
      if(indb > (inda + 1)){
        those <- lapply(seq(from = inda + 1, to = indb - 1, by = 1), function(k) {
          data.frame(source = mx$destination[k],
                     range = mx$range[k],
                     source.max = NA,
                     index = paste0(ii$index[j], ".", k))
        }) %>% Reduce("rbind",.)
      }
      
      that <- data.frame(source = mx$destination[indb],
                         range =  ii$source.max[j] - mx$source[indb],
                         source.max = NA,
                         index = paste0(ii$index[j], ".", indb))
      
      this <- rbind(this, those, that)
    }

    this$source.max <- this$source + this$range - 1
    out <- rbind(out, this)
  }
  
  out <- setNames(as.data.frame(out), names(ii))
  out
}

for(mm in seq_along(map)){# mm <- 1
  y[[mm + 1]] <- get_corresp(map[[mm]], y[[mm]])
} 

min(y[[length(y)]]$source)
