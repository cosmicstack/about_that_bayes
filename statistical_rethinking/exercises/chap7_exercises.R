library(tidyverse)
library(rethinking)

# 7E2
entropy <- function(p) {
  if (sum(p) != 1) {
    stop("Sum of probability vector must be 1")
  }
  valid.idx = which(p!=0)
  if (length(p) == length(valid.idx)) {
    -sum(p*log(p))  
  } else {
    -sum(p[valid.idx]*log(p[valid.idx]))
  }
}

entropy(c(0.7, 0.3))

# 7E3
entropy(c(0.2, 0.25, 0.25, 0.3))

# 7E4
entropy(c(1/3, 1/3, 1/3, 0))
