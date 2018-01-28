
dat = data.frame(xvar = c(6, 11.4, 16.8, 22, 26.2), yvar = c(5, 10, 15, 20, 24))
ggplot(dat, aes(x=xvar, y=yvar)) +
  xlab("cm") + ylab("Liters") +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line 
#  (by default includes 95% confidence region)