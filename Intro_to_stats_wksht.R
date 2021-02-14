# Null hypothesis:  There is no significant difference in height between seedlings fertilised with the same plant and ones fertilised with a different plant 
# pot = where the plant was
# pair = took one plant and either self or crossed the plant - on same plant they can cross of self the different flwoers  on the same plant 
library("tidyverse")
darwin <- read.csv("Data/darwin.csv") ### reading in the data
view(darwin) ### inspecting the data table 
str(darwin)
darwin <- darwin %>%
  pivot_longer(cols=c("Self":"Cross"),
          names_to = "type",
          values_to = "height") ### tidies data by making the cross and self data long 
darwin %>%
  ggplot(aes(x=type, y= height))+
  geom_jitter(width=0.1, 
               pch=21, 
               aes(fill=type))
### Making Models
model1<-lm(height~1, data = darwin)
model2 <- lm(height~1+type, data=darwin)
model2
anova_test(height~type, data=darwin)
pf(q=5.9395, df1=1, df2=28, lower.tail=FALSE)
pf(q=2, df1=1, df2=28, lower.tail=FALSE)