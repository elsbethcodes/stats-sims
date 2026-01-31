library(tidyverse)
library(Hmisc)

set.seed(1)

#Event setup
T <- 360 #time period of simulation, minutes
lambda <- 0.8 #average arrival rate, customers/minute
mu <- 1.0 #average service rate, customers/min

t <- 0 #counter
interarrival <- c()
service <- c()
while (t <= T){
  interarrival <- c(interarrival, rexp(1, rate = lambda)) #randomly generates time between each arrival from expononetial distribution of arrival rate
  service <- c(service, rexp(1, rate = mu)) #randomly generates time taken for each service from expononetial distribution of service rate
  t <- sum(interarrival)
}
#remove last row as t > T
interarrival <- interarrival[-length(interarrival)]
service <- service[-length(service)]

N <- length(interarrival) #number of ordering customers

arrival <- cumsum(interarrival) #arrival times of customers

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

# Data Frame Setup
data <- data.frame(
  id = seq_along(interarrival),
  interarrival = interarrival,
  arrival = arrival,
  service = service,
  start = start,
  finish = finish,
  wait = wait,
  queue = queue
)

label(data$id) <- "Customer number"
label(data$interarrival) <- "Time from last arrival"
label(data$arrival) <- "Arrival time"
label(data$service) <- "Service duration"
label(data$start) <- "Service start time"
label(data$finish) <- "Service end time"
label(data$wait) <- "Queue time before service"
label(data$queue) <- "Queue length"

View(data)

#Infographics
ggplot(data=data, aes(x=wait)) + geom_histogram(binwidth=1) + labs(title="Distribution of Wait Times in Minutes")
plot(arrival, wait, type="l", main="Wait vs Arrival Time After Opening \nTime in Minutes")

plot(arrival, queue, type="l", main="Queue Length vs Arrival Time \nAfter Opening Time in Minutes")

#plot(1:N, dexp(1:N), type="l") #exponential dist.
#plot(1:N, dlnorm(1:N), type="l") #lognormal dist.
