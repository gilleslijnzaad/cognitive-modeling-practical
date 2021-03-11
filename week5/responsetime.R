library(data.table)

responsetimes <- data.table(
  type = c( rep("Low", 5000), rep("High", 5000)),
  rt = rep(0, 10000)
)

# low load
for(participant in 1:25) {
  file <- paste("~/Uni/year4/cmp/cognitive-modelling-practical/week5/data/low-load/dat", participant, ".csv", sep = "")
  data <- read.csv(file, sep = ",", strip.white=TRUE, stringsAsFactors = FALSE)
  for (i in 1:200) {
    if (data$rt[i] != "NIL") {
      responsetime <- as.numeric(data$rt[i])
      index <- (participant - 1)*200 + i
      responsetimes$rt[index] <- responsetime
    }
  }
}

# high load
for(participant in 1:25) {
  file <- paste("~/Uni/year4/cmp/cognitive-modelling-practical/week5/data/high-load/dat", participant, ".csv", sep = "")
  data <- read.csv(file, sep = ",", strip.white=TRUE, stringsAsFactors = FALSE)
  for (i in 1:200) {
    if (data$rt[i] != "NIL") {
      responsetime <- as.numeric(data$rt[i])
      index <- 5000 + (participant - 1)*200 + i
      responsetimes$rt[index] <- responsetime
    }
  }
}

responsetimes <- filter(responsetimes, rt > 0)

ggplot(responsetimes, aes(type,rt)) + 
  geom_boxplot(aes(fill=type)) +
  scale_fill_manual(values = c("darkgreen", "darkgoldenrod1")) +
  theme_bw() + 
  ggtitle("SART response time") +
  labs(x = "Task load", y = "Response time (s)") +
  theme(plot.title=element_text(size=20, face="bold")) +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"))
