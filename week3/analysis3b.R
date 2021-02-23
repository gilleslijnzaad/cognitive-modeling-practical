
# Read in the data
data_path <- "~/Uni/year4/cmp/cognitive-modelling-practical/week3/" # Change to the location of your output folder

library(stringr)
library(data.table)

# Read the trace file into memory
trace <- file(paste0(data_path, "sart-trace.txt"), "r")
lines <- readLines(trace)

activations_at <- data.table(time = rep(0, 500), activation = rep(0, 500)) # Preallocate a data.table where we can store ATTEND activation values
activations_wa <- data.table(time = rep(0, 500), activation = rep(0, 500)) # Preallocate a data.table where we can store WANDER activation values

actpersec_at <- 0
actpersec_wa <- 0
activationtotal_at <- 0
activationtotal_wa <- 0
time <- 0


for(i in 4:length(lines)){

  # Read a single line
  line <- lines[i]
  
  if(!str_detect(line, "Chunk")){
    if(str_detect(line, "\\d+")){
      current_time <- as.numeric(str_extract(line, "\\d+"))
      while(current_time != time){
        if(actpersec_at == 0){
          meanact_at <- 0
        }else {
          meanact_at <- activationtotal_at/actpersec_at
        }
        if(actpersec_wa == 0){
          meanact_wa <- 0
        }else {
          meanact_wa <- activationtotal_wa/actpersec_wa
        }
        activations_at$time[time] <- time
        activations_at$activation[time] <- meanact_at
        activations_wa$time[time] <- time
        activations_wa$activation[time] <- meanact_wa
        time <- time + 1
        activationtotal_at <- 0
        actpersec_at <- 0
        activationtotal_wa <- 0
        actpersec_wa <- 0
      }
    } 
  }
  
  else if(str_detect(line, "Chunk ATTEND has an activation of:")) {
    actpersec_at <- actpersec_at+1
    y <- str_extract(line, "[:digit:][:punct:]\\d+")
    activationtotal_at <- activationtotal_at + as.numeric(y)
  }
  else if(str_detect(line, "Chunk WANDER has an activation of:")) {
    actpersec_wa <- actpersec_wa+1
    y <- str_extract(line, "[:digit:][:punct:]\\d+")
    activationtotal_wa <- activationtotal_wa + as.numeric(y)
  }
}

activations <- data.table(
  type = c( rep("ATTEND", 500), rep("WANDER", 500)),
  time = c( seq(1, 500, 1), seq(1, 500, 1)),
  a = rep(0, 1000)
)

for (i in 1:500) {
  activations$a[i] <- activations_at$activation[i]
}
for (i in 1:500) {
  activations$a[500+i] <- activations_wa$activation[i]
}

ggplot(activations, aes(x=time, y=a)) +
  geom_smooth(aes(color=type)) +
  xlim(1,500) + 
  ylim(4.5,6) + 
  scale_color_manual(values = c("cornflowerblue", "darkgreen")) +
  ggtitle("Activation of chunk ATTEND and chunk WANDER over time") +
  labs(x = "Time (s)", y = "Mean activation", color="Chunk") +
  theme_bw() + 
  theme(plot.title=element_text(size=18, face="bold")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))

  
