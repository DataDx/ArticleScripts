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
subs <- subset(combo_df, Visit_Date == as.Date('2019-09-30'))

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
  ggtitle('Figure 1: Average Wait Time per Day (No Error)') +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  geom_point(data=subs, aes(x=Visit_Date, y=mean(Wait_Time)), color='red', size=5) +
  annotate('text', x=as.Date('2019-09-30'), y=30.1, label='Normal Point') +
  ggsave('no_error_plot.png')

# add error value
new_combo_df <- rbind(combo_df, c('2019-09-30', 781))
new_combo_df$Wait_Time <- as.numeric(new_combo_df$Wait_Time)

# create error plot
subs2 <- subset(new_combo_df, Visit_Date == as.Date('2019-09-30'))

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
  ggtitle('Figure 2: Average Wait Time per Day (With Error)') +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  geom_point(data=subs2, aes(x=Visit_Date, y=mean(Wait_Time)), color='red', size=5) +
  annotate('text', x=as.Date('2019-09-30'), y=30.22, label='Error Point') +
  ggsave('error_plot.png')

print(g1)
print(g2)