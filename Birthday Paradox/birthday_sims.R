#Birthday Paradox
#Half of the time, the first birthday collision happens by the 23rd person

# Case n=23
set.seed(1)
B <- 2500 # number of simulations
n <- 23 # sample size
repeats <- numeric(B) # empty placeholder for whether a repeat has occured or not
cum_mean <- numeric(B) # empty placeholder for the cumulative proportion

for (b in 1:B) {
  bdays <- sample(1:365, n, replace = TRUE) #generates data randomly between 1 and 365
  repeats[b] <- length(unique(bdays)) < length(bdays) #records true if there are less unique birthdays than birthdays in total
  cum_mean[b] <- mean(repeats[1:b]) #calculates cumulative mean
}

#estimate of proportion of samples with bday repeats from B simulations
estimate <- mean(repeats)

# Calculation of actual probability
choices <- seq(from=364, to=365-23+1)
P_no_repeats <- 1
for (i in choices) {
  P_no_repeats <- P_no_repeats * i/365
}
actual_P <- 1 - P_no_repeats
# Figure showing proportion vs number of simulations
library(tidyverse)
theme_set(theme_minimal())

df <- data.frame(
  sim = seq(1,B),
  prop = cum_mean
)
ggplot(df, aes(x = sim, y = prop)) + 
  geom_line() + 
  geom_hline(yintercept = actual_P, col="red", lty=2, linewidth = 1) + 
  labs(title="Birthday Paradox for n=23", x="number of simulations", y="proportion of repeat birthdays")

# Case: when is a repeat most likely to first appear?
set.seed(1)
C <- 10000 # number of simulations
max <- 366 # max group size to test
firstrepeat <- numeric(C) 

for (c in 1:C) {
  bdays <- sample(1:365, max, replace = TRUE) 
  firstrepeat[c] <- which(duplicated(bdays))[1]
}

df2 <- data.frame(
  firstrepeat = firstrepeat
)

ggplot(data=df2, aes(x=firstrepeat)) + 
  geom_histogram(binwidth=1, alpha=0.3, fill="blue") + 
  labs(title="Birthday Paradox - How Many People in a Group Before a \nBirthday is Repeated?", x="Group Size") + 
  geom_vline(xintercept = median(firstrepeat), lty = 2, lwd=0.5, col="black") +
  annotate(
    "text",
    x = median(firstrepeat),
    y = Inf,
    label = paste("Median =", median(firstrepeat)),
    vjust = 2,
    hjust = -0.1,
    col="black"
  )

#When does probability of a repeat cross 0.5?
size <- 100
probabilities <- numeric(100)
for (s in 1:size) {
  choices <- seq(from=364, to=365-s+1)
  P_no_repeats <- 1
  for (i in choices) {
    P_no_repeats <- P_no_repeats * i/365
  }
  probabilities[s] <- 1 - P_no_repeats
}
df3 <- data.frame(
  probs = probabilities,
  gsize = seq(1,size)
)
ggplot(data=df3, aes(x=gsize, y=probs)) + 
  geom_line() + 
  geom_vline(xintercept = 23, col="red", lty=2, linewidth = 0.7) + 
  geom_hline(yintercept = 0.5, col="red", lty=2, linewidth = 0.7) + 
  labs(title="Birthday Paradox - Probability of a Repeat Birthday \nin the Group by Group Size", x="Group Size", y="Probability")

# Case: real birthday data 1995 to 2014

#Data Source:
#https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/articles/howpopularisyourbirthday/2015-12-18

setwd("C:/Users/elsbe/OneDrive/Documents2/Coding")
data <- read_csv("birthday paradox/data.csv")
View(data)

data$probability <- data$average
data$probability[data$date == "29-Feb"] <- data$probability[data$date == "29-Feb"] * (97 / 400)
data$probability <- data$probability / sum(data$probability)
data <- data %>% transmute(dayofyear = row_number(), probability)

# Case: when is a repeat most likely to first appear with real birth data?

set.seed(1)
C <- 10000 # number of simulations
max <- 366 # max group size to test
firstrepeatwtd <- numeric(C) # first repeat weighted by historic probabilities

for (c in 1:C) {
  bdays <- sample(data$dayofyear, size = max, prob = data$probability, replace=TRUE) 
  firstrepeatwtd[c] <- which(duplicated(bdays))[1]
}

#plot just the real data version
df4 <- data.frame(
  firstrepeatwtd = firstrepeatwtd
)

ggplot(data=df4, aes(x=firstrepeatwtd)) + 
  geom_histogram(binwidth=1, alpha=0.3, fill="red") + 
  labs(title="Birthday Paradox - How Many People in a Group Before a \nBirthday is Repeated?", x="Group Size") + 
  geom_vline(xintercept = median(firstrepeatwtd), lty = 2, lwd=0.5) +
  annotate(
    "text",
    x = median(firstrepeatwtd),
    y = Inf,
    label = paste("Median =", median(firstrepeatwtd)),
    vjust = 2,
    hjust = -0.1
  )

#median is now higher
#distribution is now less uniform so we should expect more collisions
#however there is 1 more possible birthday now 
#more outcomes decreases the number of collisions

#overlay
med_u <- median(firstrepeat, na.rm = TRUE)
med_w <- median(firstrepeatwtd, na.rm = TRUE)

df_overlay <- data.frame(
  firstrepeat = firstrepeat,
  firstrepeatwtd = firstrepeatwtd
)

ggplot() +
  # weighted histogram (red outline)
  geom_histogram(
    data = df_overlay,
    aes(x = firstrepeatwtd),
    binwidth = 1,
    fill = "red",
    alpha=0.3,
    linewidth = 0.7
  ) +
  # uniform histogram (blue outline)
  geom_histogram(
    data = df_overlay,
    aes(x = firstrepeat),
    binwidth = 1,
    fill = "blue",
    alpha = 0.3,
    linewidth = 0.7
  ) +
  # median lines
  geom_vline(xintercept = med_u, linetype = 2, linewidth = 0.8, colour = "blue") +
  geom_vline(xintercept = med_w, linetype = 2, linewidth = 0.8, colour = "red") +
  # labels (placed near top using max bin count)
  annotate("text", x = med_u, y = Inf, label = paste("Uniform Median =", med_u),
           vjust = 2, hjust = -0.1, colour = "blue") +
  annotate("text", x = med_w, y = Inf, label = paste("Median w/ real data =", med_w),
           vjust = 4, hjust = -0.1, colour = "red") +
  labs(
    title = "Birthday Paradox - How Many People in a Group Before a \nBirthday is Repeated?",
    x = "Group Size",
    y = "Count"
  )

#pmf of real data
plot(
  data$probability,
  type = "l",
  lwd=1.2,
  main = "PDF of Real Data",
  xlab = "Day of Year",
  ylab = "Probability"
)
abline(h = 1/365, col = "red")
legend(
  "bottomright",
  legend = c("Real data", "Uniform"),
  lty = c(1, 1),
  col = c("black", "red")
)
# new year
points(1, data$probability[1], pch = 19, col = "black")
text(
  x = 1,
  y = data$probability[1],
  labels = "New \nYear",
  pos = 1,
  col = "black",
  cex=0.7
)
# leap day
points(60, data$probability[60], pch = 19, col = "black")
text(
  x = 60,
  y = data$probability[60],
  labels = "Leap Day",
  pos = 2,
  col = "black",
  cex=0.7
)
# christmas
points(360, data$probability[360], pch = 19, col = "black")
text(
  x = 360,
  y = data$probability[360],
  labels = "Christmas",
  pos = 2,
  col = "black",
  cex=0.7
)
# boxing day
points(361, data$probability[361], pch = 19, col = "black")
text(
  x = 361,
  y = data$probability[361],
  labels = "Boxing \nDay",
  pos = 1,
  col = "black",
  cex=0.7
)
# The birthday paradox is driven by combinatorics, not by unrealistic assumptions about birthdays.
