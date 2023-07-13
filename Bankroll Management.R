# Load packages
library(tidyverse)

# Set the working directory
directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(directory)

### Parameters
# Number of players
n_players <- 6
# Buy in (dollars)
buy_in <- 20
# Total Pot
pot <- n_players * buy_in 
# Prizes distribution
prize_1 <- 90
prize_2 <- 30

# Win probability distribution
prob_1 <- 0.2
prob_2 <- 0.2
prob_profit <- prob_1 + prob_2
prob_deficit <- 1 - prob_profit

# Expected Value
ev_table <- tibble(
  profit_x = c(prize_1-buy_in, prize_2-buy_in, 0-buy_in),
  prob_x = c(prob_1, prob_2, prob_deficit)
) %>%
  mutate(
    x_times_p_x = profit_x * prob_x,
    x_squared = profit_x^2,
    x_squared_times_p_x = x_squared * prob_x
  )

expected_value <- sum(ev_table$x_times_p_x)
variance <- sum(ev_table$x_squared_times_p_x) - expected_value^2
standard_deviation <- sqrt(variance)

######################### SIMULATION
# parameters
n_sims <- 10000
n_nights <- 100
initial_bankroll <- 5 * buy_in

profit_vec <- c(prize_1-buy_in, prize_2-buy_in, 0-buy_in)
prob_vec <- c(prob_1, prob_2, prob_deficit)

nights <- tibble(
  night = 1:n_nights,
  profit = sample(
    x = profit_vec,
    size = n_nights,
    replace = TRUE,
    prob = prob_vec
  ),
  cumulative_winnings = cumsum(profit)
)

mean(nights$profit)
sd(nights$profit)

nights %>%
  ggplot(aes(x = profit)) +
  geom_bar()

nights %>%
  ggplot(aes(x = night, y = cumulative_winnings)) +
  geom_line()

### many sims. Prob broke
sim_vec <- vector(length = n_sims)
min_vec <- vector(length = n_sims)
max_vec <- vector(length = n_sims)
mean_vec <- vector(length = n_sims)
sd_vec <- vector(length = n_sims)
profit_matrix <- matrix(data = NA, nrow = n_sims, ncol = n_nights)

#bankroll = ifelse(is.na(lag(bankroll) + profit), initial_bankroll + profit, lag(bankroll) + profit)

set.seed(2023)
for(i in 1:n_sims){
  sim_vec[i] <- i
  profit <- sample(x = profit_vec, size = n_nights, replace = TRUE, prob = prob_vec)
  profit_matrix[i, ] <- profit
  # bankroll <- initial_bankroll + profit
  # bankroll <- ifelse(is.na(lag(bankroll) + profit), initial_bankroll + profit, lag(bankroll) + profit)
  cumulative_winnings = cumsum(profit)
  min_vec[i] <- min(cumulative_winnings)
  max_vec[i] <- max(cumulative_winnings)
  mean_vec[i] <- mean(profit)
  sd_vec[i] <- sd(profit)
}

sim_results <- tibble(
  sim_vec,
  min_vec,
  max_vec,mean_vec, sd_vec
)

long_profit <- cbind(1:n_sims, profit_matrix) %>%
  as.tibble() %>%
  pivot_longer(!V1, names_to = "night", values_to = "profit") %>%
  group_by(V1) %>%
  mutate(night = 1:100) %>%
  ungroup() %>%
  transmute(
    sim = V1,
    night,
    profit
  ) %>%
  group_by(sim) %>%
  mutate(cumulative_winnings = cumsum(profit))

long_profit$cumulative_winnings %>% mean()
sim_results$mean_vec %>% mean()

cumulative_winnings_10000 <- long_profit %>%
  ggplot(aes(x = night, y = cumulative_winnings, group = sim)) +
  geom_line(alpha = 0.05) +
  #geom_smooth(aes(group = 1), method = "lm", formula = 'y ~ x', color = "red", se = FALSE) +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(breaks = seq(-10000, 10000, 250)) +
  labs(
    title = "Distribution of Outcomes After 100 Poker Nights",
    subtitle = "It's very possible to lose money. Sometimes a ton of it!",
    x = "Nth Poker Night",
    y = "Cumulative Winnings After N Games ($)",
    caption = paste("*** Simulated Data with", n_sims, "replications")
    
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none",
    legend.title = element_blank()
  )

#cumulative_winnings_10000

# ggsave(
#   plot = cumulative_winnings_10000,
#   filename = "cumulative_winnings_10000.png",
#   width = 10,
#   height = 5.63,
#   units = "in"
# )

cumulative_winnings_10 <- long_profit %>% head(1000) %>%
  ggplot(aes(x = night, y = cumulative_winnings, group = sim)) +
  geom_hline(yintercept = seq(-450, 0, by =50), color = "red", linetype = "dashed", alpha = 0.4) +
  geom_line(alpha = 0.5) +
  #geom_smooth(aes(group = 1), method = "lm", formula = 'y ~ x', color = "red", se = FALSE) +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(breaks = seq(-10000, 10000, 50)) +
  labs(
    title = "Distribution of Outcomes After 100 Poker Nights",
    subtitle = "It's very possible to lose money. Sometimes a ton of it!",
    x = "Nth Poker Night",
    y = "Cumulative Winnings After N Games ($)",
    caption = paste("*** Simulated Data with", 10, "replications")
    
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none",
    legend.title = element_blank()
  )

cumulative_winnings_10

ggsave(
  plot = cumulative_winnings_10,
  filename = "cumulative_winnings_10.png",
  width = 10,
  height = 5.63,
  units = "in"
)

hist_10000 <- long_profit %>%
  group_by(sim) %>%
  summarise(
    s_100 = tail(cumulative_winnings, 1),
  ) %>%
  ggplot(aes(x = s_100)) +
  geom_density() +
  geom_vline(xintercept = 400, linetype = "dashed") +
  theme_classic() +
  scale_x_continuous(breaks = seq(-10000, 10000, 400)) +
  labs(
    title = "Distribution of Outcomes After 100 Poker Nights",
    subtitle = "It's very possible to lose money. Sometimes a ton of it!",
    x = "Cumulative Winnings After 100 Games ($)",
    y = "Probability Density",
    caption = paste("*** Simulated Data with", n_sims, "replications")
    
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none",
    legend.title = element_blank()
  )

hist_10000

ggsave(
  plot = hist_10000,
  filename = "hist_10000.png",
  width = 10,
  height = 5.63,
  units = "in"
)

##### Going Broke Given Bankroll

profit_summary <- long_profit %>%
  group_by(sim) %>%
  summarise(
    min = min(cumulative_winnings),
    max = max(cumulative_winnings),
    s_100 = tail(cumulative_winnings, 1),
    mean = mean(profit),
    sd = sd(profit),
    br_50 = ifelse(min <= -50, TRUE, FALSE),
    br_100 = ifelse(min <= -100, TRUE, FALSE),
    br_200 = ifelse(min <= -200, TRUE, FALSE),
    br_300 = ifelse(min <= -300, TRUE, FALSE),
    br_400 = ifelse(min <= -400, TRUE, FALSE),
    br_500 = ifelse(min <= -500, TRUE, FALSE),
    br_600 = ifelse(min <= -600, TRUE, FALSE),
    br_700 = ifelse(min <= -700, TRUE, FALSE),
    br_800 = ifelse(min <= -800, TRUE, FALSE),
    br_900 = ifelse(min <= -900, TRUE, FALSE),
    br_1000 = ifelse(min <= -1000, TRUE, FALSE),
  )

profit_summary

broke <- profit_summary %>%
  summarise(
    br_50 = mean(br_50),
    br_100 = mean(br_100),
    br_200 = mean(br_200),
    br_300 = mean(br_300),
    br_400 = mean(br_400),
    br_500 = mean(br_500),
    br_600 = mean(br_600),
    br_700 = mean(br_700),
    br_800 = mean(br_800),
    br_900 = mean(br_900),
    br_1000 = mean(br_1000),
  ) %>%
  pivot_longer(starts_with("br"), names_to = "bankroll", values_to = "broke_prob") %>%
  transmute(
    bankroll = as.numeric(str_replace(bankroll, "br_", "")),
    broke_count = broke_prob * 10000,
    broke_prob
  )

broke

write.csv(broke, file = "broke.csv")


