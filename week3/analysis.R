## ACCURACY CALCULATION
## no mind-wandering and mind-wandering in the same table
accuracies <- data.table(
  type = c( rep("No mind-wandering", 25), rep("Mind-wandering", 25)),
  acc = c( rep(1, 25))
)

for(participant in 1:25) {
  file <- paste("~/Uni/year4/cmp/cognitive-modelling-practical/week3/results-MW/dat", participant, ".csv", sep = "")
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
  a = points/200
  accuracies$acc[25+participant] <- a
}

ggplot(accuracies, aes(x=acc)) + 
  geom_histogram(aes(fill=type), binwidth=0.005) +
  scale_x_continuous(breaks=seq(0.92,1.00,0.02)) +
  scale_fill_manual(values = c("darkgreen", "cornflowerblue")) +
  labs(title = "Response accuracy for 25 participants", x = "Accuracy", y = "Frequency", fill="Model") +
  theme_bw() + 
  theme(plot.title=element_text(size=20, face="bold")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"))
