library(dplyr)
library(ggplot2)
library(scales)



load("~/file_path/UsedCars.RData")



str(UsedCars)
unique(UsedCars$Make)

## Get Chevrolet cars in a separate data.frame
#Apply a filter: I want only Chevrolets and am going to be only working with Make, Price, and Miles variables.

(UsedCars
  %>% select(Make,Price,Miles)
  %>% filter(Make == "Chevrolet")
) -> UsedCars_Chevi
str(UsedCars_Chevi)


### Fit a model

UsedCars_SLR <- lm(formula = Price ~ Miles, data=UsedCars_Chevi)
summary(UsedCars_SLR)



#UsedCars_SLR$residuals


### Create Residual and Predicted variables

(UsedCars_Chevi %>% 
    mutate(SLR.Residuals = UsedCars_SLR$residuals,
           SLR.Predicted = UsedCars_SLR$fitted.values)) -> Used_Cars_Chevi_Resid.Predict


### Create a plot of actual vs. predicted values
#Set x-axis limit to start from 0 since price cannot be negative. This will display a warning message: "Removed 18 rows containing missing values (geom_point)."

ggplot(Used_Cars_Chevi_Resid.Predict, aes(x = SLR.Predicted, y = Price)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lwd = 1, col = "blue") +
  xlim(0,25000) +
  labs(x = "Predicted Values", y = "Actual Values", title = "Mikolaj Wieczorek")


### Create a plot of residuals vs. predicted
#Outliers estimation at > +/- 2*RMSE

#Get RMSE from the summary of fitted model UsedCars_SLR
summary = summary(UsedCars_SLR)
summary$sigma

#RMSE = 3274
#2*3274

#Thus, outliers will be the points whose residual values are greater than 6548 or smaller than - 6548.

ggplot(Used_Cars_Chevi_Resid.Predict, aes(x = SLR.Predicted, y = SLR.Residuals)) +
  geom_point() +
  geom_hline(yintercept = c(0)) +
  geom_hline(yintercept = c(-6548, 6548), lty = "dashed", lwd = 0.3) +
  coord_cartesian(ylim = c(-12000,12000)) +
  xlim(0,20000) +
  scale_y_continuous(labels = comma, breaks = pretty_breaks()) +
  labs(x = "Predicted Values", y = "Residuals", title = "Residuals vs. Predicted")


### Create a plot of Price vs. Miles

ggplot(Used_Cars_Chevi_Resid.Predict, aes(x = Miles, y = Price)) +
  geom_point() +
  geom_smooth(method = "loess" , se = FALSE) +
  scale_x_continuous(labels = comma, breaks = pretty_breaks()) +
  scale_y_continuous(labels = dollar_format(prefix="$"), breaks = pretty_breaks(n=10)) +
  labs(title = "Price vs. Miles")



### Loess fit object

Chevi_loess <- loess(formula = Price ~ Miles, data = UsedCars_Chevi)
(UsedCars_Chevi
  %>% mutate(Residuals_Loess = Chevi_loess$residuals,
             Predicted_Loess = Chevi_loess$fitted)
) -> Used_Cars_Chevi_Loess_Resid.Predict



### Create a plot of residuals vs. predicted with Loess

ggplot(Used_Cars_Chevi_Loess_Resid.Predict, aes(x = Predicted_Loess, y = Residuals_Loess)) +
  geom_point() +
  geom_hline(yintercept = c(0)) +
  geom_hline(yintercept = c(-6548, 6548), lty = "dashed", lwd = 0.3) +
  coord_cartesian(ylim = c(-12000,12000)) +
  labs(x = "Predicted Values", y = "Residuals", title = "Residuals vs. Predicted with Loess") +
  scale_x_continuous(labels = comma, breaks = pretty_breaks(n=10)) +
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n=10))


#Now, I am interested only in Impala and Malibu models.
## Get Impala and Malibu cars in separate data.frames

( UsedCars
  %>% select(Model,Price,Miles)
  %>% filter(Model == "Impala")
  %>% filter(Miles > 20000 & Miles < 120000)
) -> UsedCars_Impala



( UsedCars
  %>% select(Model,Price,Miles)
  %>% filter(Model == "Malibu")
  %>% filter(Miles > 20000 & Miles < 120000)
) -> UsedCars_Malibu



### Fitting a model for Impala and Malibu

UsedCars_Impala_SLR <- lm(formula = Price ~ Miles, data=UsedCars_Impala)
UsedCars_Malibu_SLR <- lm(formula = Price ~ Miles, data=UsedCars_Malibu)



### Set up data.frame in order to make predictions for every 10000 miles

UsedCars_NewPrediction <- data.frame(Miles = c(20000,30000,40000,50000,60000,70000,80000,90000,100000,110000,120000))



### Getting predictions for Impala’s and Malibu‘s vehicles

( UsedCars_NewPrediction
  %>% mutate(Impala_Predictions = predict(UsedCars_Impala_SLR,newdata=UsedCars_NewPrediction) )
  %>% mutate(Malibu_Predictions = predict(UsedCars_Malibu_SLR,newdata=UsedCars_NewPrediction) )
) -> UsedCars_NewPrediction



### Create a plot of both regression lines

ggplot(UsedCars_NewPrediction, mapping=aes(x=Miles,y=Impala_Predictions)) +
  geom_line(mapping=aes(x=Miles,y=Impala_Predictions)) + 
  geom_line(mapping=aes(x=Miles,y=Malibu_Predictions),linetype="dashed")+ 
  labs(x = "Miles", y = "Predictions by Model", title="Predictions by Model vs. Miles") +
  scale_x_continuous(labels = comma, breaks = pretty_breaks(n=10)) +
  scale_y_continuous(labels = dollar_format(prefix="$"), breaks = pretty_breaks(n=10))


### Regression lines intersection point coordinates
#Find at how many miles Malibu becomes more expensive than Impala. I.g. find the coordinates of a point where the 2 regression lines intersect.

#Form a coefficient matrix 'cm' using coefficients from the output of lm() function: UsedCars_Impala_SLR and UsedCars_Malibu_SLR.

cm <- rbind(coef(UsedCars_Impala_SLR),coef(UsedCars_Malibu_SLR)) 
c(-solve(cbind(cm[,2],-1)) %*% cm[,1])

#Plug in the intersection point to check.

ggplot(UsedCars_NewPrediction, mapping=aes(x=Miles,y=Impala_Predictions)) +
  geom_line(mapping=aes(x=Miles,y=Impala_Predictions)) + 
  geom_line(mapping=aes(x=Miles,y=Malibu_Predictions),linetype="dashed")+ 
  labs(x = "Miles", y = "Predictions by Model", title="Predictions by Model vs. Miles") +
  scale_x_continuous(labels = comma, breaks = pretty_breaks(n=10)) +
  scale_y_continuous(labels = dollar_format(prefix="$"), breaks = pretty_breaks(n=10)) +
  geom_point(aes(x=89508.494, y=9238.901), colour="blue", size =3) 

#Malibu is cheaper that Impala when it has less than 89,508 miles. When it has more than that, it becomes more expensive than Impala.


