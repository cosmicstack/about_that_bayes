library(tidyverse)
library(rethinking)
library(dagitty)

# Quick recap on spurious association and masked relationships
# Let me use the same symbols for two different DAGs
dag.spurious <- dagitty("dag{Z -> X; Z -> Y}")
coordinates(dag.spurious) <- list(x = c(X = 0, Y = 1, Z = 2), y = c(X = 0, Y = 1, Z = 0))
drawdag(dag.spurious)

dag.masked.1 <- dagitty("dag{Z <-> X; X -> Y; Z -> Y}")
coordinates(dag.masked.1) <- list(x = c(X = 0, Y = 1, Z = 2), y = c(X = 0, Y = 1, Z = 0))
drawdag(dag.masked.1)

dag.masked.2 <- dagitty("dag{X -> Z; X -> Y; Z -> Y}")
coordinates(dag.masked.2) <- list(x = c(X = 0, Y = 1, Z = 2), y = c(X = 0, Y = 1, Z = 0))
drawdag(dag.masked.2)

# while dag.spurious is a spurious association, for a masked relationship,
# dag.masked.1 or dag.masked.2 could work form the data alone (markov equivalency)

dag.masked.3 <- dagitty("dag{U -> Z; U -> X; X -> Y; Z -> Y}")
coordinates(dag.masked.3) <- list(x = c(X = 0, U = 1, Y = 1, Z = 2), y = c(X = 0, U = 0, Y = 1, Z = 0))
drawdag(dag.masked.3)
