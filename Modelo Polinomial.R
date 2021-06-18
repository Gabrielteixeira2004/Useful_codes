#Useful packages for regression 
library(readr) 
library(readxl) 
library(ggplot2) 
library(dplyr) 
library(tidyverse) 
library(lubridate) 
library(modelr) 
library(cowplot)

url <-"https://raw.githubusercontent.com/charlescoverdale/predicttemperature/master/MEL_weather_2019.csv"

MEL_weather_2019 <- readr::read_csv(url)

#Add a proper date column 
MEL_weather_2019 <- MEL_weather_2019 %>% 
  mutate(Date = make_date(Year, Month, Day))

names(MEL_weather_2019)[4]<- "Solar_exposure" 
names(MEL_weather_2019)[6]<- "Max_temp" 
names(MEL_weather_2019)[7]<- "Rainfall"

head(MEL_weather_2019)




ggplot(MEL_weather_2019)+ 
  geom_point(aes(y=Max_temp, x=Solar_exposure),col="grey")+ 
  labs(title = "Does solar exposure drive temperature in Melbourne?", 
       caption = "Data: Bureau of Meteorology 2020") + 
  xlab("Solar exposure")+ 
  ylab("Maximum temperature °C") +
  theme_bw()



ggplot(MEL_weather_2019, aes(x=Max_temp)) +  
  geom_histogram(aes(y=..density..), colour="black", fill="lightblue")+ 
  geom_density(alpha=.8, fill="grey",colour="darkblue")+
  scale_x_continuous(breaks=c(5,10,15,20,25,30,35,40,45), 
                     expand=c(0,0))+ 
  xlab("Temperature")+ 
  ylab("Density")+
  
  theme_bw()+ 
  theme(axis.text=element_text(size=12))+ 
  theme(panel.grid.minor = element_blank())


#Create a straight line estimate to fit the data

temp_model <- lm(Max_temp~Solar_exposure, data=MEL_weather_2019)

summary(temp_model)


#Calculate the prediction interval

prediction_interval <- predict(temp_model,  
newdata=MEL_weather_2019,  
interval = "prediction")


#Plot a chart with data and model on it

 ggplot(MEL_weather_2019)+ 
geom_point(aes(y=Max_temp, x=Solar_exposure), 
           col="grey")+ 
 
  geom_smooth(aes(y=Max_temp, x= Solar_exposure),  
              method=lm)+ 
  labs(title =  
         "Does solar exposure drive temperature in Melbourne?", 
       subtitle = 'Investigation using linear regression', 
       caption = "Data: Bureau of Meteorology 2020") +  
  xlab("Solar exposure")+ 
  ylab("Maximum temperature °C")+
  scale_x_continuous(expand=c(0,0), 
                     breaks=c(0,5,10,15,20,25,30,35,40))+
  
  theme_bw()+ 
  theme(axis.text=element_text(size=10))+ 
  theme(panel.grid.minor = element_blank())

 
 
 
 residuals_temp_predict <- MEL_weather_2019 %>% 
   add_residuals(temp_model)

 
 ggplot(data=residuals_temp_predict, 
        aes(x=Solar_exposure, y=resid) )+ 
   geom_ref_line(h=0,colour="blue", size=1)+ 
   geom_point(col="grey")+ 
   xlab("")+ 
   ylab("°C")+ 
   theme_bw()+ 
   theme(axis.text=element_text(size=12))+ 
   theme(axis.ticks.x=element_blank(), 
         axis.text.x=element_blank())+ 
   scale_x_continuous(expand=c(0,0))+ 
   labs(title = "Residual values from the linear model")

 
 
 temp_model_2 <-  
   lm(Max_temp ~ Solar_exposure + Rainfall, data=MEL_weather_2019)
 
 summary(temp_model_2)
 
 
 
 MEL_weather_2019 <- MEL_weather_2019 %>% 
   mutate(Day_number=row_number())

 
 
 ggplot(MEL_weather_2019)+ 
   geom_line(aes(x = Day_number, y = Max_temp)) + 
   labs(title = 'Melbourne temperature profile', 
        subtitle = 'Daily maximum temperature recorded in Melbourne in 2019', 
        caption = "Data: Bureau of Meteorology 2020") + 
   xlab("Day of the year")+ 
   ylab("Temperature")+ 
   theme_bw()   

 
 
 #Create a straight line estimate to fit the data 
 poly1 <- lm(Max_temp ~ poly(Day_number,1,raw=TRUE), 
             data=MEL_weather_2019)
 
 summary(poly1)
 
 #Create a polynominal of order 2 to fit this data 
 poly2 <- lm(Max_temp ~ poly(Day_number,2,raw=TRUE), 
             data=MEL_weather_2019)
 
 summary(poly2)
 
 #Create a polynominal of order 3 to fit this data 
 poly3 <- lm(Max_temp ~ poly(Day_number,3,raw=TRUE), 
             data=MEL_weather_2019)
 
 summary(poly3)
 
 MEL_weather_2019 <- MEL_weather_2019 %>% 
   mutate(poly1values=predict(poly1,newdata=MEL_weather_2019),
          poly2values=predict(poly2,newdata=MEL_weather_2019),
          poly3values=predict(poly3,newdata=MEL_weather_2019))
 
 head(MEL_weather_2019) 

 
 
 
 #Plot a chart with all models on it
 ggplot(MEL_weather_2019)+ 
 geom_line(aes(x=Day_number, y= Max_temp),col="black")+ 
   geom_line(aes(x=Day_number, y= poly1values),col="red") + 
   geom_line(aes(x=Day_number, y= poly2values),col="green")+ 
   geom_line(aes(x=Day_number, y= poly3values),col="blue")+
   #Add text annotations 
   geom_text(x=10,y=18,label="data series",col="black",hjust=0)+ 
   geom_text(x=10,y=16,label="linear",col="red",hjust=0)+ 
   geom_text(x=10,y=13,label=parse(text="x^2"),col="green",hjust=0)+ 
   geom_text(x=10,y=10,label=parse(text="x^3"),col="blue",hjust=0)+
   
   labs(title = "Estimating Melbourne's temperature", 
        subtitle = 'Daily maximum temperature recorded in Melbourne in 2019', 
        caption = "Data: Bureau of Meteorology 2020") +
   
   xlim(0,366)+ 
   ylim(10,45)+
   
   scale_x_continuous(breaks= 
                        c(15,45,75,105,135,165,195,225,255,285,315,345), 
                      labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),  expand=c(0,0), 
                      limits=c(0,366)) + 
   scale_y_continuous(breaks=c(10,15,20,25,30,35,40,45)) + 
   xlab("")+ 
   ylab("°C")+ 
   theme_bw()+ 
   theme(axis.text=element_text(size=12))+ 
   theme(panel.grid.minor = element_blank())

 
 
 
 #Get the residuals for poly1
 residuals_poly1 <- MEL_weather_2019 %>% 
 add_residuals(poly1)
 residuals_poly1_chart <-  
   ggplot(data=residuals_poly1,aes(x=Day_number, y=resid))+ 
   geom_ref_line(h=0,colour="red", size=1)+ 
   geom_line()+ 
   xlab("")+ 
   ylab("°C")+ 
   theme_bw()+ 
   theme(axis.text=element_text(size=12))+ 
   theme(axis.ticks.x=element_blank(), 
         axis.text.x=element_blank())
 
 residuals_poly1_chart
 
 
 #Get the residuals for poly2 
 residuals_poly2 <- MEL_weather_2019%>% 
   add_residuals(poly2)
 residuals_poly2_chart <- ggplot(data=residuals_poly2,aes(x=Day_number, y=resid))+ 
   geom_ref_line(h=0,colour="green", size=1)+ 
   geom_line()+ 
   xlab("")+ 
   ylab("°C")+ 
   theme_bw()+ 
   theme(axis.text=element_text(size=12))+ 
   theme(axis.ticks.x=element_blank(), 
         axis.text.x=element_blank())
 residuals_poly2_chart
 
 #Get the residuals for poly3 
 residuals_poly3 <- MEL_weather_2019 %>% 
   add_residuals(poly3)
 residuals_poly3_chart <- ggplot(data=residuals_poly3,aes(x=Day_number, y=resid))+ 
   geom_ref_line(h=0,colour="blue", size=1)+ 
   geom_line()+ 
   theme_bw()+ 
   theme(axis.text=element_text(size=12))+ 
   scale_x_continuous(breaks= 
                        c(15,45,75,105,135,165,195,225,255,285,315,345), 
                      labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                      expand=c(0,0), 
                      limits=c(0,366))+ 
   xlab("")+ 
   ylab("°C")
 
 residuals_poly3_chart
 
plot_grid( 
   residuals_poly1_chart,  
   residuals_poly2_chart,  
   residuals_poly3_chart, 
   ncol=1,nrow=3,label_size=16) 


library(patchwork)

residuals_poly1_chart / residuals_poly2_chart / residuals_poly3_chart
 