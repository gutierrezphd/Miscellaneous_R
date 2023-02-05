# Code written by Ian A. Gutierrez, PhD
# License GNU 3.0 

# Feel free to use or modify for educational purposes! 
# Please acknowledge yours truly as the source of this code :)

#required packages
library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(scales)
library(beepr)
library(hms)

#stack exchange summary of the problem
browseURL("https://math.stackexchange.com/questions/2129646/bayess-theorem-from-tversky-and-kahneman-in-michael-lewiss-the-undoing-project")

#problem statement
cat("The mean IQ of the population of eighth graders in a city is known to be 100. 
You have selected a random sample of 50 children for a study of educational achievement. 
The first child tested has an IQ of 150. 
What do you expect the mean IQ to be for the whole sample?")

#create population 
{set.seed(1337)
  distro <- rnorm(100000000, 100, 15)
}

#transform into tibble
data <- tibble(value = distro) %>% 
  mutate(n = row_number()) 

#draw k samples of 50
{
  samples <- list()
  k <- 1000
  time.start <- Sys.time()
  for (sample in 1:k) {
    samples[[sample]] <-sample_n(data, 50) #randomly sample 5O  cases
    if (length(samples) %% 10 == 0) {
      cat("\14\n")
      cat("Percentage of simulations complete:", percent(length(samples)/k, .01))
      time.end <- Sys.time()
      time <- difftime(time.end, time.start, units = "secs")
      time <- round(time)
      time <- as_hms(time)
      time <- as.character(time)
      cat("\n\n")
      cat("Elapsed time:", time)
    }
    if (length(samples)==k) {
      beepr::beep(3)

    }
  }
  }

#select samples that contain an IQ of ~150
samples150 <- samples %>% 
  tibble(sample = .) %>% 
  mutate(values = map(sample, ~{pull(., value)})) %>% 
  mutate(has150 = map(values, ~{ifelse(any(. > 149 & . < 151), TRUE, FALSE)})) %>% 
  unnest(col = has150) %>% 
  filter(has150==TRUE)
  
  
means <- samples150 %>% 
  mutate(mean = map(values, ~(mean(.)))) %>% 
  select(mean) %>% 
  unnest(cols="mean")

#distribution of means
means %>% 
  mutate(mean = mean %>% round(0)) %>%  
  count(mean)

#bar chart
means %>% 
  mutate(mean = mean %>% round(0)) %>%  
  count(mean) %>% 
  ggplot(aes(x = mean, y = n, label = n)) +
  geom_col() +
  geom_text(vjust = -1.1, color="black") + 
  labs(x = "Sample Mean",
       y = "N Samples") +
  theme_bw() +
  scale_x_continuous(breaks=90:110, expand=c(0,0)) +
  scale_y_continuous(breaks=seq(0, 250, 50), limits=c(0, 250), expand=c(0,0))

#mean of means
means %>% 
  mutate(mean = mean %>% round(0)) %>% 
  summarize(bigmean = mean(mean)) %>% 
  pull(bigmean)



