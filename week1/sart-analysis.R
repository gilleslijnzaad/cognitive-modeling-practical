##
## SART model analysis
## Cognitive Science Practical 2018
##


##-----------------------------------##
## Question 2: Running an experiment ##
##-----------------------------------##


# Read in the data
data_path <- "~/output/" # Change to the location of your output folder

# List of the data files
behfiles <- list.files(path = data_path, pattern=".csv", full.names = TRUE)

# Combine all data files into a single data frame (behdat)
behdat <- data.frame()
for (i in 1:length(behfiles)) {
  behdat <- rbind(behdat, read.csv(behfiles[i], sep = ",", strip.white = TRUE))
}


## Analysis

# What is the mean response accuracy, and what is the standard error of the mean? Aggregate within and then across participants.


# Plot a histogram of response time for participant 1
library(tidyverse)
file <- "~/Uni/year4/cmp/cognitive-modelling-practical/week1/results/dat1_noNIL.csv"
data <- read.csv(file, sep = ",", strip.white=TRUE, colClasses = c('factor', 'numeric', 'factor', 'factor', 'numeric'))
ggplot(data, aes(x=rt)) + geom_histogram(fill='darkgreen') +
  scale_x_continuous(breaks=seq(0.38,0.56,0.04)) +
  labs(title = "Response times of participant 1", x = "Response time (s)", y = "Frequency") +
  theme_bw() + 
  theme(plot.title=element_text(size=20, face="bold")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"))

# What is the mean response time and standard error? Aggregate within and then across participants.




##---------------------------------------##
## Question 3: Analysing the model trace ##
##---------------------------------------##

## The easiest way to do this is to read the trace file line by line while keeping track of the state of the model

library(stringr)
library(data.table)

# Read the trace file into memory
trace <- file(paste0("~/Uni/year4/cmp/cognitive-modelling-practical/week1/", "sart-trace.txt"), "r")
lines <- readLines(trace)

time <- 1
activations <- data.table(time = rep(0, 500), activation = rep(0, 500)) # Preallocate a data.table where we can store ATTEND activation values

actpersec <- 1
activationtotal <- 0
time <- 0

for(i in 1:length(lines)) {

  # Read a single line
  line <- lines[i]
  
  if(str_detect(line, "Chunk ATTEND has an activation of:")) {
    actpersec <- actpersec+1
    if(str_extract(lines[i-1], "\\d+") == time + 1) {
      meanact <- activationtotal/actpersec
      activations$time[time] <- time
      activations$activation[time] <- meanact
      time <- time + 1
      activationtotal <- 0
      actpersec <- 0
    }
    y <- str_extract(line, "[:digit:][:punct:]\\d+")
    activationtotal <- activationtotal + as.numeric(y)
  }
  
}

plot <- ggplot(activations, aes(x=time, y=activation))
plot + geom_line(color='darkgreen') + 
  xlim(1,500) + 
  ylim(4.5,6) + 
  ggtitle("Activation of chunk ATTEND over time") +
  labs(x = "Time (s)", y = "Mean activation") +
  theme_bw() + 
  theme(plot.title=element_text(size=20, face="bold")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18,face="bold"))
  
  # Check whether the line contains the activation of the ATTEND chunk, and then store the value
  # Hints:
  #   - you can use str_detect() and str_extract(). See http://stringr.tidyverse.org/
  #   - use regular expressions to describe string patterns. A good resource is https://regexr.com/ 
  #   - you will also need to keep track of the time, which is given at the start of many (but not all!) lines in the trace file. 
  #   - you can add a line to the activations data.table using set(activations, idx, j = 1:3, value = list(participant, time, activation)). See ?set for more information.




# You should now have a data.table containing each observation of the activation of ATTEND for all 25 model runs, along with the time of the observation.
# Since the observations were not all made at exactly the same time for each participant, and the number of observations differs between participants,
# we have to bin the measurements (e.g. in 1-second bins) before we can calculate the average activation over time across participants.

# Create a plot of mean activation of the ATTEND chunk over time, averaged across participants.

# Create another plot in which you apply smoothing, so that it is easier to see if there is a trend in the data.