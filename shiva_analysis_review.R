library(scales)

mens_heights = rnorm(500, 50, 10)
womens_heights = rnorm(500, 50, 10)

height = data.table(men = mens_heights, women = womens_heights)
height[, height_difference_women_minus_men := womens_heights - mens_heights]

ggplot(height, aes(mens_heights, height_difference_women_minus_men))+
  geom_point()+
  geom_hline(yintercept = 0, color = 'red')+
  #geom_segment(x = 50, xend = 60, y = 6, yend = 6, color = 'blue')+
  geom_smooth(method='lm')+
  ggtitle('Weak Linear Fit')+
  xlim(c(0, 100))
  #ggtitle('Height Difference Between Men and Women\n The TRUE Normal Case By Voting Precinct')


weak_fit = lm(height_difference_women_minus_men ~ mens_heights, data = height)
summary(weak_fit)

strong_fit = lm(height_difference_women_minus_men ~ mens_heights, data = height)
summary(strong_fit)


IC_Trump_Support = rnorm(500, .35, .20)
SP_Trump_Support = rnorm(500, .50, .20)

support = data.table(IC_Trump_Support = IC_Trump_Support, 
                     SP_Trump_Support = SP_Trump_Support)

support[, relative_difference_IC_minus_SP := IC_Trump_Support - SP_Trump_Support]


ggplot(support, aes(SP_Trump_Support, IC_Trump_Support))+
  geom_point()+
  geom_abline(yintercept = 0,slope = 1, color = 'purple')+
  scale_y_continuous(label = scales::percent)+
  scale_x_continuous(label = scales::percent)+
  geom_text(x = .2, y = .8, label = 'IC Voters Are Redder Than SC Voters', color = 'red')+
  geom_text(x = .6, y = -.1, label = 'IC Voters Are Bluer Than SC Voters', color = 'blue')+
  
  ggtitle('Support For Trump\nIC Voters vs SP Voters')


ggplot(support, aes(SP_Trump_Support, relative_difference_IC_minus_SP))+
  geom_point()+
  geom_hline(yintercept = 0, color = 'purple')+
  scale_y_continuous(label = scales::percent)+
  scale_x_continuous(label = scales::percent)+
  geom_text(x = 1, y = .05, label = 'Equal Support for Trump', color = 'purple')+
  geom_text(x = 1, y = -.05, label = 'Among Trump and SC Voters', color = 'purple')+
  geom_text(x = .2, y = .3, label = 'IC Voters Are Redder Than SC Voters', color = 'red')+
  geom_text(x = .2, y = -.5, label = 'IC Voters Are Bluer Than SC Voters', color = 'blue')+
  ggtitle('Support For Trump\nIC Voters vs SP Voters\n Using The Same Data, But Dr. Shivas Axes')
  
  
  
  
  
  xlim(c(0, 100))