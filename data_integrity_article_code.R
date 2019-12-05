library(ggplot2)
library(dplyr)

set.seed(111)

# create no error dates
date_seq <- seq.Date(as.Date('2019-07-01'), as.Date('2019-09-30'), 'day')
date_seq <- as.data.frame(rep(date_seq, 6000))
colnames(date_seq) <- 'Visit_Date'

# create no error wait times
wait_times_normal <- as.data.frame(rnorm(92 * 6000, mean=30, sd=5))
colnames(wait_times_normal) <- 'Wait_Time'

combo_df <- cbind(date_seq, wait_times_normal)

# create no error plot
g1 <- combo_df %>% 
  group_by(Visit_Date) %>% 
  summarize(AVG_Wait_Time = mean(Wait_Time)) %>% 
  ggplot(aes(x=Visit_Date, y=AVG_Wait_Time)) + 
  geom_line(size=0.75, color='steelblue') +
  geom_hline(aes(yintercept=mean(AVG_Wait_Time)), 
             color='red', linetype='dotted', size=1) +
  geom_hline(aes(yintercept=mean(AVG_Wait_Time) + sd(AVG_Wait_Time) * 3), 
             color='red', linetype='dotted', size=1) +
  geom_hline(aes(yintercept=mean(AVG_Wait_Time) - sd(AVG_Wait_Time) * 3), 
             color='red', linetype='dotted', size=1) +
  ylim(29.7, 30.3) +
  ggtitle('Average Wait Time per Day (No Error)') +
  theme(plot.title = element_text(size = 20, face = "bold"))

# add error value
new_combo_df <- rbind(combo_df, c('2019-09-30', 781))
new_combo_df$Wait_Time <- as.numeric(new_combo_df$Wait_Time)

# create error plot
g2 <- new_combo_df %>% 
  group_by(Visit_Date) %>% 
  summarize(AVG_Wait_Time = mean(Wait_Time)) %>% 
  ggplot(aes(x=Visit_Date, y=AVG_Wait_Time)) + 
  geom_line(size=0.75, color='steelblue') +
  geom_hline(aes(yintercept=mean(AVG_Wait_Time)), 
             color='red', linetype='dotted', size=1) +
  geom_hline(aes(yintercept=mean(AVG_Wait_Time) + sd(AVG_Wait_Time) * 3), 
             color='red', linetype='dotted', size=1) +
  geom_hline(aes(yintercept=mean(AVG_Wait_Time) - sd(AVG_Wait_Time) * 3), 
             color='red', linetype='dotted', size=1) +
  ylim(29.7, 30.3) +
  ggtitle('Average Wait Time per Day (With Error)') +
  theme(plot.title = element_text(size = 20, face = "bold"))

print(g1)
print(g2)