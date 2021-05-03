
# load packages -----------------------------------------------------------

library(tidyverse)


# read data ---------------------------------------------------------------

fish <- read_csv("chap12q19ElectricFish.csv")

fish_long <- 
  pivot_longer(fish, speciesUpstream:speciesDownstream,
               names_to = "location",
               values_to = "species") %>% 
  mutate(location = str_remove(location, c("species"))) %>% 
  print()

head(fish)

str(fish)

# perform tasks -----------------------------------------------------------

t.test(formula = species ~ location, data = fish_long)  #t-test

ttest_results <- t.test(formula = species ~ location, data = fish_long)

ttest_results

diff(ttest_results$estimate)

fish_long_data <-
  fish_long %>% 
  group_by(location) %>% 
  summarize(
    n = n(),
    mean = mean(species),
    sd = sd(species),
    sem = sd/sqrt(n),
    upper = mean + 1.96 * sem,
    lower = mean - 1.96 * sem
  ) %>% 
  print()

fish_long %>% 
  ggplot(aes(x = location, y = species)) + 
  geom_jitter(aes(color = location), 
              shape = 16, size = 3, 
              alpha = 0.3, width = 0.4) + 
  geom_errorbar(aes(y = mean, ymax = upper, ymin = lower), 
                data = fish_long_data, 
                width = 0.1, size = 0.8) + 
  geom_point(aes(y = mean), 
             data = fish_long_data, 
             size = 3) + 
  scale_color_manual(values = c("dark blue","red")) + 
  theme_minimal() + 
  guides(color = "none")


fish_long %>% 
  ggplot(aes(x = species)) +
  geom_histogram(
    aes(fill = location), 
    bins = 5, 
    alpha = 0.5, 
    position = "identity"
  ) +
  scale_fill_manual(values = c("darkorange","cyan4")) +
  theme_minimal() 

# read data ---------------------------------------------------------

Crab <- read_csv("chap15q27FiddlerCrabFans.csv") %>% 
  rename(type = crabType, temp = bodyTemperature) 
Crab

temp_means <- 
  Crab %>% 
  filter(!is.na(temp)) %>%     #remove missing values 
  group_by(type) %>% 
  summarize( 
    mean = mean(temp), 
    sd = sd(temp), 
    n = n(), 
    sem = sd / sqrt(n), 
    upper = mean + 1.96 * sem, 
    lower = mean - 1.96 * sem
  ) %>% 
  print() 



# Question D --------------------------------------------------------------
ggplot(data = Crab, aes(x = type, y = temp)) + 
  geom_jitter(aes(color = type), 
              width = 0.1, 
              alpha = 0.7, 
              show.legend = FALSE, 
              na.rm = TRUE) + 
  geom_errorbar(aes(y = mean, ymin = lower, ymax = upper), 
                data = temp_means,
                width = 0.1, position = position_nudge(0.3)) + 
  geom_point(aes(y = mean), data = temp_means, 
             position = position_nudge(0.3)) + 
  scale_color_manual(values = c("darkorange","darkorchid","cyan4","blue3"))

# question E -------------------------------------------------------------------
aov_crab_summary <- 
  aov(temp ~ type, data = Crab) 
aov_crab_summary 

summary(aov_crab_summary)











