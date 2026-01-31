library(tidyverse)
library(Hmisc)

set.seed(1)

#Event setup
T <- 360 #time period of simulation, minutes
#lambda <- 0.8 #average arrival rate = 1/time between arrivals
mu <- 1 #average service rate = 1/service time
#meanlog <- log(2.2)
#sdlog <- 0.35

###########################################################
# ---- arrival process with Beta-shaped rate ----

dt <- 1                     # minute resolution
time <- seq(0, T, by = dt)

# Beta shape parameters (fiddle with these)
alpha <- 3
beta  <- 4

u <- time / T
shape <- dbeta(u, alpha, beta)
shape <- shape / max(shape)   # peak = 1

# Scale to realistic arrival rates
lambda_peak <- 0.8            # peak customers / minute
lambda_low  <- 0.2            # quiet baseline

lambda_t <- lambda_low + lambda_peak * shape

# arrivals per minute
arrivals_per_min <- rpois(length(time), lambda_t * dt)

# convert to arrival times (random within minute)
arrival <- unlist(
  lapply(seq_along(arrivals_per_min), function(i) {
    if (arrivals_per_min[i] == 0) return(NULL)
    time[i] + runif(arrivals_per_min[i], 0, dt)
  })
)

arrival <- sort(arrival)
N <- length(arrival)

# service times
service <- rexp(N, rate=mu)
#service <- rlnorm(N, meanlog = meanlog, sdlog = sdlog)
##################################

start <- numeric(N) #service start time
finish <- numeric(N) #service end time
wait <- numeric(N) #queue time before service

start[1] <- arrival[1] #service starts when the first customer arrives
finish[1] <- start[1] + service[1] # service ends at service start time + time taken for service
wait[1] <- 0 #queueing time of first customer is 0

for (i in 2:N) { #from the 2nd customer onwards...
  start[i] <- max(arrival[i], finish[i-1]) #the next customer's service starts the later of when they arrive or when the previous customer's service ends
  finish[i] <- start[i] + service[i] #the customer's service ends at service start time + time taken for service
  wait[i] <- start[i] - arrival[i] #their queueing time is the time between their arrival and the start of their service
}

#Queue times
#count rows where finish > arrival[n] for all rows < n
queue <- sapply(seq_len(N), function(n) {
  if (n == 1) 0 else sum(finish[1:(n-1)] > arrival[n])
})

#Dataframe
data <- data.frame(
  id = seq_len(N),
  arrival = arrival,
  service = service,
  start = start,
  finish = finish,
  wait = wait,
  queue = queue
)

View(data)

label(data$id) <- "Customer number"
label(data$arrival) <- "Arrival time"
label(data$service) <- "Service duration"
label(data$start) <- "Service start time"
label(data$finish) <- "Service end time"
label(data$wait) <- "Queue time before service"
label(data$queue) <- "Queue length"

View(data)

# Infographics
ggplot(data = data, aes(x = wait)) + geom_histogram(binwidth = 1) + labs(title = "Distribution of Wait Times in Minutes")

plot(data$arrival, data$wait, type = "l", main = "Wait vs Arrival Time After Opening \nTime in Minutes", xlab = "Minutes since opening", ylab = "Wait (minutes)")

plot(data$arrival, data$service, type = "l", main = "Service Time vs Arrival Time After Opening \nTime in Minutes", xlab = "Minutes since opening", ylab = "Wait (minutes)")
