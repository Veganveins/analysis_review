library(scales)
library(dplyr)
library(DAAG)

# https://vincentarelbundock.github.io/Rdatasets/doc/DAAG/cricketer.html
data("cricketer")

d <- as.data.table(cricketer)

# cricketers not killed in action
killed_in_action = d[kia == 0]

# cricketers not killed in action, died in bed
kia_inbed = killed_in_action[inbed == 1]

# HYPOTHESIS: left and right handed cricketers should live about the same
right = kia_inbed[left == 'right'] # 2615 observations
left = kia_inbed[left == 'left'] # 584 observations

right_s <- right[sample(nrow(right), 584), ]

#create new dataframe
df <- data.table(precinct = seq(1:584),
                                left = left$life,
                                right = right_s$life)

ggplot(df, aes(left, right))+
  geom_point()+
  ylim(c(0,100))+
  xlim(c(0,100))+
  geom_abline(intercept = 0, slope = 1, color = 'purple')+
  geom_text(x = 15, y = 90, label = 'right handers live longer', color = 'red')+
  geom_text(x = 80, y = 20, label = 'left handers live longer', color = 'limegreen')+
  ylab('Age of Right Handed Cricketers (yrs)')+
  xlab('Age of Left Handed Cricketers (yrs)')


# calculate difference in lifespan
df[, difference := right - left]


# plot lefty lifespans vs difference with righty's
ggplot(df, aes(left, difference))+
  geom_point()+
  ylim(c(-40,40))+
  xlim(c(0,100))+
  #geom_abline(intercept = 0, slope = 1, color = 'purple')+
  geom_hline(yintercept = 0, color = 'purple')+
  #geom_text(x = 15, y = 90, label = 'right handers live longer', color = 'red')+
  #geom_text(x = 80, y = 20, label = 'left handers live longer', color = 'limegreen')+
  ylab('Age Difference (yrs, righty minus lefty)')+
  xlab('Age of Left Handed Cricketers (yrs)')

# low correlation

# still weird flat tail in voter data


# assume identical voting patterns between the two groups
IC = rnorm(n = 1000, mean = .4, sd = .04)
SP = rnorm(n = 1000, mean = .5, sd = .1)

uncorrelated_syn_voter_df <- data.table(IC = IC,
                           SP = SP)

correlated_syn_voter_df <- data.table(IC = sort(IC),
                                        SP = sort(SP))

uncorrelated_syn_voter_df[, difference := IC - SP]
correlated_syn_voter_df[, difference := IC - SP]

# plot voter data
ggplot(correlated_syn_voter_df, aes(SP, IC))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, color = 'purple')+
  geom_text(x = .15, y = .90, label = 'IC Voters Redder than SP', color = 'red')+
  geom_text(x = .80, y = .20, label = 'IC Voters Bluer than SP', color = 'blue')+
  scale_y_continuous(labels = scales::percent, limits = c(0, 1))+
  scale_x_continuous(labels = scales::percent, limits = c(0, 1))+
  ylab('IC Trump Support')+
  xlab('SP Trump Support')+
  ggtitle('Prior 1 #1')


# plot difference data
ggplot(uncorrelated_syn_voter_df, aes(SP, difference))+
  geom_point(alpha = .15)+
  geom_hline(yintercept = 0, color = 'purple')+
  geom_text(x = .7, y = .25, label = 'IC Voters Redder than SP', color = 'red')+
  geom_text(x = .7, y = -.25, label = 'IC Voters Bluer than SP', color = 'blue')+
  scale_y_continuous(labels = scales::percent, limits = c(-.5, .5))+
  scale_x_continuous(labels = scales::percent, limits = c(0, 1))+
  ylab('Difference In Trump Support (IC - SP)')+
  xlab('SP Trump Support')+
  ggtitle('Possible Prior #6 : \nAssuming UNcorrelated Voting Patterns, \nDifferent Means, \nDifferent Variance')


# if the two groups have the same mean 
# if they have the same standard deviation, 
# and they are correlated
# then you expect Dr. Shiva's normal case



# if the two groups have the same mean
# if they have the same standard deviation
# and they are uncorrelated
# then you get a line, but you don't get a "flat tail"


# if the two groups have different means
# if they have the same standard deviation
# and they are correlated
# then you get a flat line showing IC voters consistently bluer than SP


# if the two groups have different means
# if they have the same standard deviation
# and they are uncorrelated
# then you get a line but you don't get a "flat tail"


# if the two groups have different means
# if they have they have different standard deviations
# and they are correlated
# then you get a line but you don't get a "flat tail"


df = data.table(SP = .6, IC = .65)

ggplot(df, aes(SP, IC))+
  geom_point()+
  ylab('IC Voter Trump Support')+
  xlab('SP Voter Trump Support')+
  scale_y_continuous(labels = scales::percent, limits = c(0, 1))+
  scale_x_continuous(labels = scales::percent, limits = c(0, 1))+
  geom_abline(intercept = 0, slope = 1, color = 'purple')+
  geom_text(x = .5, y = .15, label = 'Equal Trump support among IC and SC voters in a precinct(IC - SP = 0)', color = 'purple')+
  geom_text(x = .35, y = .85, label = 'Each point represents a precinct', color = 'black')+
  geom_text(x = .25, y = .65, label = 'Points above line indicate IC voters like')+
  geom_text(x = .25, y = .6, label = 'Trump MORE than SC voters', color = 'black')+
  geom_text(x = .7, y = .3, label = 'Points below line indicate IC voters like Trump')+
  geom_text(x = .7, y = .25, label = 'LESS than SC voters', color = 'black')+
  ggtitle('Precinct Plotting')



# HEIGHT EXAMPLE

men = rnorm(1000, 70, 3)
women = rnorm(1000, 64.5, 2.5)

heights = data.table(men = men, 
                     women = women)

heights[, diff := women - men]
heights[, color := ifelse(diff <=0, 'taller man', 'taller woman')]

heights[, color := ifelse( (diff < -15.97 & diff > -15.971), 
                           'example_point', ifelse( (diff < -4.74 & diff > -4.75), 'example_point', color))]


ggplot(heights, aes(men, women, colour = color))+
  geom_point()+
  scale_color_manual(values = c('blue', 'hotpink', 'darkorange'))+
  xlim(c(50, 85))+
  ylim(50, 85)+
  ylab('Womens Heights (inches)')+
  xlab('Mens Heights (inches)')+
  ggtitle('Heights of Each Partner, Heterosexual Couples')+
  geom_text(x = 71, y = 52, label = 'Avg M', color = 'blue')+
  geom_hline(yintercept = 64.5, color = 'hotpink', linetype = 'dotted')+
  geom_text(x = 55, y = 65, label = 'Avg F ', color = 'hotpink')+
  geom_vline(xintercept = 70, color = 'blue', linetype = 'dotted')+
  geom_text(x = 58, y= 80, label = 'Tall F, Short M', color = 'black')+
  geom_text(x = 75, y= 80, label = 'Tall F, Tall M', color = 'black')+
  geom_text(x = 58, y= 55, label = 'Short F, Short M', color = 'black')+
  geom_text(x = 80, y= 55, label = 'Short F, Tall M', color = 'black')+
  geom_text(x = 82, y = 63, label = '(79, 63)', color = 'darkorange')+
geom_text(x = 59, y = 58, label = '(62, 57)', color = 'darkorange')


heights[, color := ifelse( (diff < -15.97 & diff > -15.971), 
                           'example_point', ifelse( (diff < -4.74 & diff > -4.75), 'example_point', color))]

ggplot(heights, aes(men, diff, colour = color))+
  geom_point()+
  scale_color_manual(values = c('blue', 'hotpink'))+
  xlim(c(50, 85))+
  ylim(-20, 20)+
  ylab('Height Difference (inches, F minus M)')+
  xlab('Mens Heights (inches)')+
  geom_hline(yintercept = 0, color = 'black')+
  ggtitle('Couple Height Differences, Heterosexual Couples')+
  geom_text(x = 71, y = -20, label = 'Avg M', color = 'blue')+
  geom_hline(yintercept = -5.65, color = 'hotpink', linetype = 'dotted')+
  geom_text(x = 54, y = -1, label = 'Avg Diff = -5.5 ', color = 'hotpink')+
  geom_vline(xintercept = 70, color = 'blue', linetype = 'dotted')+
  geom_text(x = 58, y= 18, label = 'Tall F, Short M', color = 'black')+
  geom_text(x = 75, y= 18, label = 'Tall F, Tall M', color = 'black')+
  geom_text(x = 58, y= -18, label = 'Short F, Short M', color = 'black')+
  geom_text(x = 80, y= -20, label = 'Short F, Tall M', color = 'black')+
  geom_text(x = 83, y = -15, label = '(79, -16)', color = 'black')+
  geom_text(x = 60, y = -6, label = '(62, -5)', color = 'black')
  



  
  











