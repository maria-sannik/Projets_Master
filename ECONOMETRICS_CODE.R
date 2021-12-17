save(data, file = "AssignmentData.RData")
open(SannikovMaria_Assignemnt.R)
#Question 1

model1 <- lm(fare ~ dist + passen + concen,data)
summary(model1)
plot(model1)

library(lmtest)
bptest(model1)

#Question 2

model2 <- lm(lfare ~ ldist + lpassen + concen,data)
summary(model2)

bptest(model2)
#Question 3

model3 <- lm(lfare ~ ldist + ldistsq + lpassen + concen,data)
summary(model3)
plot(model3)

full.model=function(x){model3$coefficients["ldist"]*x + model3$coefficients["ldistsq"]*x^2}
bptest(model3)



plot(full.model(1:15))
plot(fitted(model3), resid(model3))

#Graph in question 4
d1 <- data%>% select(lfare,lpassen,concen)
fit3 <- lm(lfare ~ ldist + ldistsq + lpassen + concen,data)
# Obtain predicted and residual values
d1$predicted <- predict(fit3)
d1$residuals <- residuals(fit3)

library(ggplot2)
library(dplyr)
library(tidyr)
d1 %>% 
  gather(key = "iv", value = "x", -lfare, -predicted, -residuals) %>%
  ggplot(aes(x = x, y = lfare)) +
  geom_segment(aes(xend = x, yend = predicted), alpha = .2) +
  geom_point(aes(color = residuals)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  guides(color = FALSE) +
  geom_point(aes(y = predicted), shape = 1) +
  facet_grid(~ iv, scales = "free_x") +
  theme_bw()  

#Question 4

model4 <- lm(lfare ~ lpassen + concen,data)
summary(model4)
plot(model4)

plot(fitted(model4), resid(model4))

cor(data$ldist, data$concen, method = "pearson", use = "complete.obs")
cor(data$ldistsq, data$concen, method = "pearson", use = "complete.obs")

bptest(model4)
#Graph2:
d2 <- data%>% select(lfare,lpassen,concen)
fit4 <- lm(lfare ~ lpassen + concen,data)
# Obtain predicted and residual values
d2$predicted <- predict(fit4)
d2$residuals <- residuals(fit4)

library(ggplot2)
library(dplyr)
library(tidyr)
d2 %>% 
  gather(key = "iv", value = "x", -lfare, -predicted, -residuals) %>%
  ggplot(aes(x = x, y = lfare)) +
  geom_segment(aes(xend = x, yend = predicted),
               
               alpha = .2) +
  geom_point(aes(color = residuals)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  guides(color = FALSE) +
  geom_point(aes(y = predicted), shape = 1) +
  facet_grid(~ iv, scales = "free_x") +
  theme_bw()  


#Question 5

model5 <- lm(lfare ~ ldist + ldistsq + lpassen + concen + factor(year) +factor(id),data)
summary(model5)
plot(model5)
plot(fitted(model5), resid(model5))
bptest(model5)

data$year <- factor(data$year)
levels(data$year)
#1997 is our reference level

data$id <- factor(data$id)
levels(data$id)
#id 1 is our reference level




