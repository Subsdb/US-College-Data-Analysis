library(tidyverse)
library(ISLR2)

# Q. Do colleges with larger full time enrollments have lower 
#grad rates ?

#Is it diff for public/private institutions?

#is it diff for more selective schools?
?College

summary(College)

glimpse(College)
view(College)

#Exploratory graphics

ggplot(College, aes(x=Grad.Rate)) + geom_histogram()
 
#we can see outlier

outlier <- filter(College, Grad.Rate >=100)
view(outlier)


ggplot(College, aes(x=log10(F.Undergrad), 
                    y=Grad.Rate)) +
  geom_point()

college_new <- College |>
  mutate(Log_Ftime = log10(F.Undergrad)) |>
  select(Grad.Rate,
    Log_Ftime,
    Private,
    Top25perc)

view(college_new)         


#modelling 

ggplot(College, aes(x=log10(F.Undergrad), 
                    y=Grad.Rate)) +
  geom_point() +
  geom_smooth(method='lm')


#the line is flat, indicating that full time undergrad alone
#isn't a great predictor of grad rates 

model_undergrad <- lm(Grad.Rate ~ Log_Ftime,
                      data = college_new)

summary(model_undergrad)  

# p value range is 0 - 1 . 

#smaller p values means the variables are highly statistical . 
#In this case p = 0.95 so full time undergrad isn't a 
#great predictor of grad rates 


#Q. what about private/public college?

ggplot(College, aes(x=log10(F.Undergrad), 
                    y=Grad.Rate,
                      color = Private)) +
  geom_point() +
  geom_smooth(method='lm', 
              se=FALSE) +
  scale_color_brewer(palette = "Dark2")


model_private <- lm(Grad.Rate ~ Private + Log_Ftime,
                    data = college_new)

summary(model_private)


#Both PrivateYes and log_fulltime have extremely low p-values (p < 2.2e-16), 
#indicated by the '***' next to their estimates. This suggests that 
#both variables are highly statistically significant in predicting the 
#Graduation Rate.

# interaction : Private and F_Undergrad
model_private_int <- lm(Grad.Rate ~ Private * Log_Ftime,
                        data = college_new)
summary(model_private_int)


ggplot(College, aes(x = log10(F.Undergrad),
                    y = Grad.Rate,
                    color = Private)) + 
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  labs(x = "Full-time undergraduates (log scale)",
       y = "Graduation rate",
       color = "Private?",
       title = "Bigger schools, more degrees") +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()


#PrivateYes:log_fulltime:  This is the combined effect of being a private college and the log_fulltime variable.

#In summary, the model suggests that the interaction between being a private college and the logarithm of the number of full-time undergraduates has a statistically significant effect on predicting the graduation rate. while the effect of PrivateYes on its own does not appear to be statistically significant.

#so 1) Do colleges with larger full time enrollments have lower grad rates ? - No 

#2)Is it diff for public/private institutions? private college and the logarithm of the number of full-time undergraduates has a statistically significant effect on predicting the graduation rate.

