
#MA615 homework

Date = read.csv("MR_DATE.csv")

#Comparing daily water temperature
Date_1 = filter(Date, Date$WTMP <800, Date$ATMP<800)

x1 = grep("12:00:00", Date_1$DATETIME)
y1 = Date_1$WTMP[x1]
x2 = c(1:length(x1))
data1 = data.frame(x2,y1)

ggplot(data=data1, mapping=aes(x = x2, y = y1))+geom_point()+geom_smooth()+scale_x_continuous(breaks=seq(0,10146,365),labels=c(1988:2015))+
  xlab("Daily time at 12pm")+ylab("Water temperature")

fit_1 = stan_glm(y1~x2,data = Date_1, refresh=0)

summary(fit_1, digits=5)





#comparing average water temperature of December every year
x1 = grep("-12-",Date_1$DATETIME)

y1 = Date_1$WTMP[x3]

y2 =c()
for (i in 1987:2016){
  y2= append(y2, mean(y1[grep(i, Date_1$DATETIME[x1])]))
}


x = c(1987:2016)
plot(x , y2 , xlab = "year", ylab = "average WTMP in December")

fit_2 = stan_glm(y2 ~ x, data = data.frame(x,y2), refresh=0)
print(fit_2,digits=5)
abline(fit_2)







#comparing maximum temperature every year
y1 = c()
for(i in 1988:2016){
  y1 = append(y1, max(Date_1$WTMP[grep(i, Date_1$DATETIME)]))  

}

x = c(1988:2016)

x1 = grep(3898, Date_1$DATETIME)
y = max(Date_1$WTMP[x1])
y1[11]=32.1

plot(x , y1 , xlab = "year", ylab = "Maximum temperature each year")

fit_3 = stan_glm(y1 ~ x, refresh=0)
print(fit_3,digits=5)
abline(fit_3)
















