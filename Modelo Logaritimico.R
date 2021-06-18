#generating the data
n=500
x1 <- 1:n
set.seed(10)
y1 <- 1*log(x1)-6+rnorm(n)
#plot the data
plot(y1~x1)

   
#fit log model
fit <- lm(y1~log(x1))
#Results of the model
summary(fit)   


#plot 

x1=seq(from=1,to=n,length.out=1000)

y2=predict(fit,newdata=list(x1),       interval="confidence")

teste <- data.frame(x1,y1,y2)

matlines(x1,y1,lwd=2)


ggplot(teste) +
  aes(x1,y1) +
  geom_point() +
  geom_line(aes(x1,fit), color = 'blue', size = 0.9, alpha = 0.5) +
  theme_light()

teste1 <- teste %>% add_residuals(fit)


  
  ggplot(teste1)+ 
    aes(x1,resid)+
  geom_ref_line(h=0,colour="red", size=1)+ 
  geom_line()+ 
  xlab("")+ 
  ylab("°C")+ 
  theme_bw()
  