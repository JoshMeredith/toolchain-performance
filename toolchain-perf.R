library(ggplot2)
library(umx)
library(fs)


dir_create("./results/graph")

hello_world <- function() {
  data <- read.csv("./results/csv/GHC build Hello World.csv", header = FALSE)

  print(data)

  ggplot(data, aes(x=V2, y=V3, fill=V1)) +
    geom_boxplot() +
    ylab("Execution time (milliseconds)") +
    xlab("Test variables") +
    labs(fill = "Tool versions") +
    ggtitle("GHC build Hello World") +
    coord_flip() +
    theme(plot.title = element_text(hjust = 0.5))

  ggsave("./results/graph/Build Hello World.svg")
}

build_cabal <- function() {
  data <- read.csv("./results/csv/Cabal build Cabal.csv", header = FALSE)

  data[,3] = data[,3] / 1000

  ggplot(data, aes(x=V2, y=V3, fill=V1)) +
    geom_boxplot() +
    ylab("Execution time (seconds)") +
    xlab("Test variables") +
    labs(fill = "Tool versions") +
    ggtitle("Cabal build Cabal") +
    coord_flip() +
    theme(plot.title = element_text(hjust = 0.5))

  ggsave("./results/graph/Cabal build Cabal.svg")
}

simple_lenses <- function() {
  data_baseline <- read.csv("./results/csv/Simple lenses (TH),baseline.csv", header = FALSE)
  data_lenses   <- read.csv("./results/csv/Simple lenses (TH),lenses.csv",   header = FALSE)

  baseline <- data.frame(data_baseline, label = "Baseline")
  lenses   <- data.frame(data_lenses,   label = "With lenses")

  print(rbind(baseline, lenses))

  ggplot(data = rbind(lenses, baseline), mapping = aes(x = V1, y = V3, fill = label)) +
    facet_wrap(vars(V2), ncol = 1) +
    geom_bar(data = lenses, stat = "identity", position = "dodge") +
    geom_bar(data = baseline, stat = "identity", position = "dodge") +
    ylab("Execution time (milliseconds)") +
    xlab("Test variables") +
    labs(fill = "Lenses") +
    ggtitle("Simple lenses (TH)") +
    coord_flip() +
    theme(plot.title = element_text(hjust = 0.5))

  ggsave("./results/graph/Simple lenses (TH).svg")
}

hello_world()
build_cabal()
simple_lenses()