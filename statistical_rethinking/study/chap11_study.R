library(rethinking)
library(tidyverse)

data("chimpanzees")
d <- chimpanzees
head(d)

d$treatment <- 1 + d$prosoc_left + 2*d$condition

xtabs(~ treatment + prosoc_left + condition, d)
