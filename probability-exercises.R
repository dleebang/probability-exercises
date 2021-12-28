library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)

# Background
# An insurance company offers a one-year term life insurance policy that pays $150,000 in the event of death within one year. 
# The premium (annual cost) for this policy for a 50 year old female is $1,150. Suppose that in the event of a claim, the company forfeits the premium and loses a total of $150,000, 
# and if there is no claim the company gains the premium amount of $1,150. The company plans to sell 1,000 policies to this demographic.

# Insurance company
loss <- -150000
profit <- 1150
n <- 1000

#The death_prob data frame from the dslabs package contains information about the estimated probability of death within 1 year (prob) for different ages and sexes.
#Question 1
#a) Use death_prob to determine the death probability of a 50 year old female, p.
p <- death_prob %>% filter(sex == "Female" & age==50) %>% pull(prob)

#b) The loss in the event of the policy holder's death is -$150,000 and the gain if the policy holder remains alive is the premium $1,150.
# What is the expected value of the company's net profit on one policy for a 50 year old female?
E <- (p*loss) + (profit*(1-p))

#c) standard error of the profit on one policy for a 50 year old female.
S <- abs(loss - profit) * sqrt(p*(1-p))

#d) expected value of the company's profit over all 1,000 policies for 50 year old females
avg <- 1000 * E

#e) standard error of the sum of the expected value over all 1,000 policies for 50 year old females
se <- sqrt(1000) * S

#f) Use the Central Limit Theorem to calculate the probability that the insurance company loses money on this set of 1,000 policies.
pnorm(profit, avg, se)


#Question 2)
#a) Use death_prob to determine the probability of death within one year for a 50 year old male.
p2 <- death_prob %>% filter(sex == "Male" & age==50) %>% pull(prob)

#b) Suppose the company wants its expected profits from 1,000 50 year old males with $150,000 life insurance policies to be $700,000. Use the formula for expected value of the sum of draws with the following values and solve for the premium b:
mu_sum = 700000
a <- -150000
b <- (mu_sum/n-a*p2)/(1-p2)
b

#c) Using the new 50 year old male premium rate, calculate the standard error of the sum of 1,000 premiums.
sqrt(n)*abs(loss - b)*sqrt(p2*(1-p2))

#d) What is the probability of losing money on a series of 1,000 policies to 50 year old males?
pnorm(b, 700000, 338262.1)


#Question 3)
#a)
n <- 1000
p <- 0.015
loss <- -150000
premium <- 1150

avg <- n * ((p*loss) + (premium*(1-p)))

#b)
se <- sqrt(n) * abs(loss - premium) * sqrt(p*(1-p))

#c)
pnorm(premium, avg, se)

#d)
million <- -1000000
pnorm(million, avg, se)

#e)
p_seq <- seq(.01, .03, .001)

expec_values <- function(n, p, loss, premium){
  n * ( (loss * p) + (premium * (1-p) ) )
}

SE <- function(n, p, loss, premium){
  sqrt(n) * abs(loss - premium) * sqrt(p * (1-p))
}

E<-expec_values(n, p = p_seq, loss, premium)
SE<-SE(n, p = p_seq, loss, premium)
probabilities<-pnorm(premium, E, SE)

question3e <- cbind(E, SE, probabilities, p_seq)

#f)
p <- seq(.01, .03, .0025)
E2<-expec_values(n, p = p_seq2, loss, premium)
SE2<-SE(n, p = p_seq2, loss, premium)
probabilities2<-pnorm(million, E2, SE2)

question3f <- cbind(E2, SE2, probabilities2, p_seq2)


#Question 4
#a)
million<- -1000000
n<-1000
p_loss <- 0.015
loss <- -150000
premium <- 1150
set.seed(25, sample.kind = "Rounding")

s<-sample(c(loss, premium), n, replace=T, prob=c(p_loss, 1-p_loss))
sum(s/10^6)

set.seed(27, sample.kind = "Rounding")
B<-10000
S <- replicate(B, {
  simulation<-sample(c(loss, premium), n, replace=T, prob=c(p_loss, 1-p_loss))
  sum(simulation)
})
mean(S<million)


#Question 5)
#a)
l <- loss
p <- p_loss
z <- qnorm(0.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))

#b)
(l * p) + (x * (1-p))

#c)
n * ((l * p) + (x * (1-p)))

#d)
set.seed(28, sample.kind = "Rounding")
S <- replicate(B, {
  x <- sample(c(loss, x), n, replace=T, prob=c(p_loss, 1-p_loss))
  sum(x)
})
mean(S<0)


#Question 6)
#a)
set.seed(29, sample.kind = "Rounding")

B <- replicate(B, {
  new_p <- p + sample(seq(-0.01, 0.01, length = 100), 1)
  x <- sample(c(loss, x), n, replace=T, prob = c(new_p, 1-new_p))
  sum(x)
})

mean(B)   #expected value
mean(B<0)  #prob of losing money
mean(B < -1000000)  #prob of losing more than 1 million
