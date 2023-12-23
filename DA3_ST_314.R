x <- c(0, 300, 350, 400, 450, 500, 550, 600, 700, 800, 900, 5000)
Count <- c(2, 5, 1, 2, 1, 3, 1, 3, 1, 2, 2, 1)
total_count <- 0
for (i in Count){
  total_count <- total_count + i
}
wheel_of_fortune <- data.frame(x, Count)
wheel_of_fortune <- wheel_of_fortune %>% mutate(prob = Count/total_count)
sum <- 0
for(i in wheel_of_fortune$prob){
  sum <- sum + i
}

x <- c(0, 300, 350, 400, 450, 500, 550, 600, 700, 800, 900, 5000)
wheel_of_fortune <- data.frame(x, Count)
total_count <- 0
for (i in Count){
  total_count <- total_count + i
}
wheel_of_fortune <- wheel_of_fortune %>% mutate(prob = Count/total_count)
sum <- 0
index <- 1
for(i in 1:12){
  sum <- sum + (x[i]*wheel_of_fortune$prob[i])
}
sum

#index of 800 in probs is 10
#prob of x happening 3 times (disjoint) is x^3
#therefore
wheel_of_fortune$prob[10]^3

# prob of success 0 times in 3 trials
y <- (1 - wheel_of_fortune$prob[10])^3
#prob of success at least one of these is opposite
1-y

# Q4
wheel_outcomes = c(rep(0,2),  rep(300,5),
                   rep(350,1),rep(400,2), 
                   rep(450,1),rep(500,3), 
                   rep(550,1),rep(600,3), 
                   rep(700,1),rep(800,2), 
                   rep(900,2),rep(5000,1))
spins_thousand <- sample(wheel_outcomes, size = 1000, replace = TRUE)
spins_table <- table(spins_thousand)
spins_table_probs <- c()
for(i in as.numeric(spins_table)){
  spins_table_probs <- c(spins_table_probs, i/1000)
}
spins_table_probs


### Fast Food ###
mcdonalds <- fastfood %>% filter(restaurant == "Mcdonalds")
dairy_queen <- fastfood %>% filter(restaurant == "Dairy Queen")
ggplot(mcdonalds, aes(x=calories)) + geom_histogram(binwidth = 125, boundary = 0, color = "white") + 
  labs(title = "Number of Calories in McDonalds Menu Items by Increments of 125")
ggplot(dairy_queen, aes(x=calories)) + geom_histogram(boundary = 0, binwidth = 50, color = "white") + 
  labs(title = "Number of Calories in Dairy Queen Menu Items by Increments of 50")
ggplot(mcdonalds, aes(x=calories)) + geom_boxplot()

dqmean <- mean(dairy_queen$cal_fat)
dqsd <- sd(dairy_queen$cal_fat)

ggplot(data = dairy_queen, aes(x = cal_fat)) + 
  geom_blank() + 
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = c(mean = dqmean, sd = dqsd), col = "tomato")

pnorm(q=200, mean = dqmean, sd = dqsd) -
  pnorm(q=100, mean = dqmean, sd = dqsd)
dairy_queen %>% 
  filter(cal_fat >= 100 & cal_fat <= 200) %>% 
  summarise(percent = n() / nrow(dairy_queen))


sonic <- fastfood %>% filter(restaurant == "Sonic")
scmean <- mean(sonic$cal_fat)
scsd <- sd(sonic$cal_fat)

ggplot(data = sonic, aes(x = cal_fat)) + 
  geom_blank() + 
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, args = c(mean = scmean, sd = scsd), col = "tomato")

pnorm(q=300, mean = scmean, sd = scsd) -
  pnorm(q=200, mean = scmean, sd = scsd)
sonic %>% 
  filter(cal_fat >= 200 & cal_fat <= 300) %>% 
  summarise(percent = n() / nrow(sonic))

