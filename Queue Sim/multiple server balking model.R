library(tidyverse)
library(Hmisc)

set.seed(1) 

# --------------------
# ---- Parameters ----
# --------------------
# Time period for simulation
T  <- 360   # minutes 
# represents 10-4pm opening hours

# Parameters of Beta distribution
alpha <- 3
beta  <- 4

# Min and max rate of customer arrivals in customers/min
lambda_low  <- 0.2 # quiet baseline
# e.g. 0.2 means a customer every 5 mins on average
lambda_peak <- 1.2 # desired peak 
# e.g. 0.8 means 4 customers every 5 mins on average

# Multi-server
n_servers <- 6 # number of staff working the cafe
#Balking
max_queue <- 10 # max number of customers waiting to be served; if >= this, customer leaves

# -----------------------------------------------------------------------------------
# ---- Construction of non-homogeneous Poisson distribution of customer arrivals ----
# -----------------------------------------------------------------------------------
time <- seq(0, T-1)
u <- time / T #reparameterise time so that it is on a [0,1] interval 

# Shape for rate of customer arrivals, lambda(t)
# 3,4 gives right-skew beta dist. with mean approx slightly earlier than halfway through service
shape <- dbeta(u, alpha, beta)
shape <- shape / max(shape) #scale range of beta shape to [0,1]

# Lambda as a function of time
lambda_t <- lambda_low + (lambda_peak - lambda_low) * shape

arrivals_per_min <- rpois(length(time), lambda_t) 
# arrivals per min 
# e.g. 2 in 1st min, 4 in 2nd min, 3 in 3rd min, etc.

arrival <- unlist(lapply(seq_along(arrivals_per_min), function(i) {
  k <- arrivals_per_min[i]
  if (k == 0) return(NULL) #if no arrivals in that minute then don't return anything
  time[i] + runif(k, 0, 1) #otherwise return k random intervals between time[i] and time[i+1]
}))

arrival <- sort(arrival) #order chronologically
N <- length(arrival)

# --------------------------------------------------------------
# ---- Order mix varies by time: hot food up to 60% at lunch ---
# --------------------------------------------------------------

lunch_center <- T * 0.5 #lunch peak should be at 1pm and symmetric in a 10-4pm open
lunch_sd     <- 40 #95% of customers will come for lunch within 80 mins either side of 1pm

lunch_bump <- exp(-0.5 * ((arrival - lunch_center) / lunch_sd)^2) # Gaussian distribution
lunch_bump <- lunch_bump / max(lunch_bump) # scale the range of the distribution down to 1

p_hot_total <- 0.15 + (0.60 - 0.15) * lunch_bump # probability share of hot food

# 40/60 split of hot orders into hot_food and coffee_hot_food
p_combo_given_hot <- 0.60 
p_combo   <- p_hot_total * p_combo_given_hot
p_hot     <- p_hot_total * (1 - p_combo_given_hot)

# 80/20 split of other orders into coffee and till only
p_coffee_given_nonhot <- 0.80
p_nonhot <- 1 - p_hot_total
p_coffee <- p_nonhot * p_coffee_given_nonhot
p_till   <- p_nonhot * (1 - p_coffee_given_nonhot)

#sample order types based on vector of probabilities
order_type <- map_chr(seq_len(N), function(i) {
  sample(
    x = c("till_only", "coffee", "hot_food", "coffee_hot_food"),
    size = 1,
    prob = c(p_till[i], p_coffee[i], p_hot[i], p_combo[i])
  )
})

# make order_type a factor
order_type <- factor(order_type,
                     levels = c("till_only", "coffee", "hot_food", "coffee_hot_food"))

# --------------------------------------------------------------------------------
# ---- Service times: order category based, min service time 1 minute ------------
# --------------------------------------------------------------------------------

service <- numeric(N)

service[order_type == "till_only"] <- 1 + runif(sum(order_type == "till_only"), 0, 1.0) # uniformly bounded between [0,1] minutes
service[order_type == "coffee"] <- 2 + runif(sum(order_type == "coffee"), 0, 3.0) # uniformly bounded between [2,5] minutes
service[order_type == "hot_food"] <- 5 + runif(sum(order_type == "hot_food"), 0, 3.0) # uniformly bounded between [5,8] minutes
service[order_type == "coffee_hot_food"] <- 6 + runif(sum(order_type == "coffee_hot_food"), 0, 4.0) # uniformly bounded between [6,10 minutes]

# useful to uncomment to make the data more readable
# service <- ceiling(service)

# ------------------------------------------------------------
# ---- FCFS multi-server dynamics + balking at max_queue -------
# ------------------------------------------------------------

# use NAs as some customers are never served
start <- rep(NA_real_, N)
finish <- rep(NA_real_, N)
queue_time <- rep(NA_real_, N)
server_id <- rep(NA_integer_, N) # servers assigned to customers

number_in_service <- integer(N) # number ahead still being served
number_in_queue <- integer(N) # number ahead in queue not yet being served
number_in_system_ahead <- integer(N) # total ahead

balked <- rep(FALSE, N) # TRUE if customer leaves
entered_system <- rep(FALSE, N) # TRUE if customer enters order system (opposite of balked but kept for simplicity)
server_free <- rep(0, n_servers)  # next time each server [1-n] becomes free

for (i in seq_len(N)) { # for each customer
  
  prev_served <- which(entered_system) # who has entered the system?
  
  if (length(prev_served) == 0) { # if no one has entered the system yet then there is no one being served or queuing
    number_in_service[i] <- 0 
    number_in_queue[i]   <- 0
  } else {
    number_in_service[i] <- sum(start[prev_served] <= arrival[i] & finish[prev_served] > arrival[i], na.rm = TRUE)
    number_in_queue[i] <- sum(start[prev_served] > arrival[i], na.rm = TRUE)
  }
  
  number_in_system_ahead[i] <- number_in_service[i] + number_in_queue[i]
  
  # Balking rule: if queue already at/above max, customer leaves
  if (number_in_queue[i] >= max_queue) {
    balked[i] <- TRUE
    next # skips to next customer if this customer isn't being served
  }
  
  # otherwise assign next available server
  j <- which.min(server_free) # index of first server available
  
  start[i] <- max(arrival[i], server_free[j]) # customer served the later of when they arrive or a server is free
  finish[i] <- start[i] + service[i]
  queue_time[i] <- start[i] - arrival[i] # queue time is the time between arriving and service beginning
  
  server_free[j] <- finish[i] # server becomes free when this customer is finished being served
  server_id[i] <- j 
  entered_system[i] <- TRUE
}

# ---- Dataframe ----
data <- data.frame(
  id = seq_len(N),
  arrival = arrival,
  order_type = order_type,
  service = service,
  start = start,
  finish = finish,
  queue_time = queue_time,
  number_in_queue = number_in_queue,
  number_in_service = number_in_service,
  #number_in_system_ahead = number_in_system_ahead,
  #p_hot_total = p_hot_total,
  balked = balked,
  server_id = server_id
)

# ---- Variable labels (Hmisc) ----
Hmisc::label(data$id)                    <- "Customer number"
Hmisc::label(data$arrival)               <- "Arrival time"
Hmisc::label(data$order_type)            <- "Order type"
Hmisc::label(data$service)               <- "Service duration (minutes)"
Hmisc::label(data$start)                 <- "Service start time"
Hmisc::label(data$finish)                <- "Service end time"
Hmisc::label(data$queue_time)            <- "Queue time before service (minutes)"
Hmisc::label(data$number_in_queue)       <- "Number waiting (excludes in-service)"
Hmisc::label(data$number_in_service)     <- "Number currently in service"
#Hmisc::label(data$number_in_system_ahead)<- "Number ahead in system (queue + in service)"
Hmisc::label(data$balked)                <- "Customer left due to full queue"
Hmisc::label(data$server_id)             <- "Server assigned"
#Hmisc::label(data$p_hot_total)           <- "Hot-food share target (hot + combo)"

View(data)

# ---- Infographics ----
served_data <- dplyr::filter(data, !balked)

# time axis setup (more readable than minutes after arrival)
time_axis_10_16 <- scale_x_continuous(
  breaks = seq(0, 360, by = 60),
  labels = c("10:00","11:00","12:00","13:00","14:00","15:00","16:00")
)

# rate of arrivals
arrival_rate_df <- data.frame(
  time = time,
  lambda_t = lambda_t
)
ggplot(arrival_rate_df, aes(time, lambda_t)) + geom_line() + 
  labs(title="Average rate of customer arrivals", x="Time of Day", y="Rate of arrivals (customers/min)") +
  time_axis_10_16

# hot food share
ggplot(data, aes(x = arrival, y = p_hot_total)) +
  geom_line() +
  labs(title = "Hot-food share over time", x = "Time of Day", y = "Hot-food share (orders incl. hot food)") +
  scale_y_continuous(labels = scales::percent) +
  time_axis_10_16

# distribution of queue times
ggplot(served_data, aes(x = queue_time)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of Queue Times \n(served customers)", x = "Queue time (minutes)", y = "Count")

# queue length by arrival time
ggplot(data, aes(x = arrival, y = number_in_queue)) +
  geom_step() +
  labs(title = "Queue length vs arrival time", x = "Time of Day", y = "Number in queue") +
  time_axis_10_16

#workload vis
workload_df <- data %>%
  mutate(minute = pmin(floor(arrival), T - 1)) %>%
  group_by(minute) %>%
  summarise(
    mean_service = mean(service),
    .groups = "drop"
  ) %>%
  mutate(
    lambda = lambda_t[minute + 1], # lambda_t is on time grid 0..T-1
    workload = lambda * mean_service, # expected minutes of work arriving per minute
    capacity = n_servers # servers provide ~n_servers minutes of service per minute
  )

ggplot(workload_df, aes(x = minute)) +
  geom_line(aes(y = workload), linewidth = 1) +
  geom_hline(aes(yintercept = capacity), linetype = "dashed") +
  labs(
    title = "Workload vs capacity",
    x = "Time of Day",
    y = "Work arriving per minute (minutes of service)"
  ) +
  time_axis_10_16 +
  theme_minimal()

#balking vis
balk_df <- data %>%
  mutate(minute = pmin(floor(arrival), T - 1)) %>%
  group_by(minute) %>%
  summarise(
    arrivals = n(),
    balked = sum(balked),
    balk_rate = balked / arrivals,
    .groups = "drop"
  )

ggplot(balk_df, aes(x = minute, y = balk_rate)) +
  geom_col(alpha = 0.7) +
  labs(
    title = "Balking rate over time (customers \nwho leave)",
    x = "Time of Day",
    y = "Share who leave"
  ) +
  scale_y_continuous(labels = scales::percent) +
  time_axis_10_16 +
  theme_minimal()

ggplot(served_data, aes(x = order_type, y = queue_time)) +
  geom_boxplot(outlier.alpha = 0.3) +
  labs(
    title = "Queue time by order type (served customers)",
    x = "Order type",
    y = "Queue time (minutes)"
  ) +
  theme_minimal()

#heatmap of queues
heat_df <- data %>%
  mutate(minute = pmin(floor(arrival), T - 1)) %>%
  group_by(minute) %>%
  summarise(mean_queue = mean(number_in_queue), .groups = "drop")

ggplot(heat_df, aes(x = minute, y = 1, fill = mean_queue)) +
  geom_tile() +
  labs(
    title = "Average queue length through the day",
    x = "Time of Day",
    y = NULL,
    fill = "Mean queue"
  ) +
  time_axis_10_16 +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())


#service times currently randomly sampled so no insight to be gained
#ggplot(served_data, aes(x = arrival, y = service)) +
#  geom_point(alpha = 0.5) +
#  geom_smooth(se = FALSE) +
#  labs(title = "Service Time vs Arrival Time (served customers)", x = "Minutes since opening", y = "Service time (minutes)")

# ---- Quick summary ----
cat("N served:", sum(entered_system), "\n")
cat("N balked:", sum(balked), "\n")
cat("N arrivals:", N, "\n")

cat("Mean queue time (served):", mean(served_data$queue_time), "minutes\n")
cat("P(queue_time > 0) (served):", mean(served_data$queue_time > 0), "\n")
cat("Max queue time (served):", max(served_data$queue_time), "minutes\n")
cat("Max number_in_queue observed:", max(data$number_in_queue), "\n")
