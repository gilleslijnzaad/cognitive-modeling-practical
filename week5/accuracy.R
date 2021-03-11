library(stringr)
library(data.table)

accuracies <- data.table(
  type = c( rep("Low", 25), rep("High", 25)),
  acc = c( rep(1, 25))
)

for(participant in 1:25) {
  file <- paste("~/Uni/year4/cmp/cognitive-modelling-practical/week5/data/low-load/dat", participant, ".csv", sep = "")
  data <- read.csv(file, sep = ",", strip.white=TRUE, stringsAsFactors = FALSE)
  points <- 0
  for(i in 1:200) {
    if (data$stimulus[i] == "O") {
      if (data$response[i] == "f") {
        points <- points + 1
      }
    } else {
      if (data$response[i] == "NIL") {
        points <- points + 1
      }
    }
  }
  a = points/200*100
  accuracies$acc[participant] <- a
}

for(participant in 1:25) {
  file <- paste("~/Uni/year4/cmp/cognitive-modelling-practical/week5/data/high-load/dat", participant, ".csv", sep = "")
  data <- read.csv(file, sep = ",", strip.white=TRUE, stringsAsFactors = FALSE)
  points <- 0
  for(i in 2:200) {
    if (data$stimulus[i-1] == "O") {
      if (data$response[i] == "f") {
        points <- points + 1
      }
    } else {
      if (data$response[i-1] == "NIL") {
        points <- points + 1
      }
    }
  }
  a = points/200*100
  accuracies$acc[25+participant] <- a
}

ggplot(accuracies, aes(type,acc)) + 
  geom_boxplot(aes(fill=type)) +
  scale_fill_manual(values = c("darkgreen", "darkgoldenrod1")) +
  theme_bw() + 
  ggtitle("SART accuracy") +
  labs(x = "Task load", y = "Accuracy (%)") +
  theme(plot.title=element_text(size=20, face="bold")) +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"))
