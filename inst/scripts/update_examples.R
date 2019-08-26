library(dplyr)

fictiveteams <- read.csv("inst/fictiveteams.csv", stringsAsFactors = FALSE) %>%
  mutate(
    female = as.factor(female),
    foreign = as.factor(foreign),
    operation = as.factor(operation)
  )
save(fictiveteams, file = "data/fictiveteams.RData")

fictivetmpnet <- read.csv("inst/fictivetmpnet.csv", stringsAsFactors = FALSE)
save(fictivetmpnet, file = "data/fictivetmpnet.RData")

