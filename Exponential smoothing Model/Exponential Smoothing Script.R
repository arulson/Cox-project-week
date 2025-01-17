library(tidyverse)
library(doParallel)
library(foreach)
library(lubridate)

#read in Merlin outage invocation data for a specific site

data = read.csv("'site126_6.csv'")

#aggregate based on the hour in which the interaction took place as well as dtl_1_num
hourly_data = data %>% mutate(start_time = as.POSIXct(start_time)) %>%
  mutate(hour_interval = floor_date(start_time, "hour")) %>%
  group_by(site_id, hour_interval, dtl_1_num) %>%
  summarise(invo_count = n(), .groups = "drop")

#aggregate again into each hour interval, giving a count of how many 1s are in dtl_1_num, 
#the total number of interactions, and the proportion of the interactions that were 1s
results = hourly_data %>% arrange(hour_interval) %>% group_by(hour_interval) %>%
  summarise(count_1 = sum(invo_count[dtl_1_num == 1]),
            total_count = sum(invo_count),
            proportion_1 = count_1/total_count)


#calculate an exponential weighted moving average based on a selected lambda
#the closer the lambda to 1, the less smoothing, the closer to 0, the smoother the result
lambda = 0.8
ewma = rep(0, nrow(results))
ewma[1] = results$proportion_1[1]

for(n in 2:nrow(results)){
  ewma[n] = lambda*(results$proportion_1[n]) + (1-lambda)*(ewma[n-1])
}


#if one hour is more than k standard deviations greater than the previous hour,
#mark it as a spike
#additionally, make sure that the point is greater than some minimum threshold
threshold = 0.5
k = 2
spikes = vector()
for(n in 2:length(ewma)){
  if(ewma[n] > threshold & ewma[n] > k*sd(ewma) + ewma[n-1]){
    spikes = append(spikes, n)
  }
}

#graph the resulting data and located spikes
results %>% ggplot(aes(x = hour_interval, y = proportion_1)) +
  geom_line(color = 'black') +
  geom_point(data = results[spikes,], aes(x = hour_interval, y = proportion_1, color = 'potential outages'), size = 1.7) +
  scale_color_manual(values = c("potential outages" = 'red')) +
  labs(title = 'Hourly Merlin Outage Proportion',
       x = 'Hour Interval',
       y = 'Proportion of outages reported') +
  theme(legend.position = c(0.9, 0.9),
        legend.title = element_blank())



#check to see if the most recent hour is marked as a spike - if so, print a warning
latest_spike = results[max(spikes),]$hour_interval
if(max(results$hour_interval) == latest_spike){
  print("There may currently be an outage")
} else{
  print("There is currently not likely to be an outage happening")
}