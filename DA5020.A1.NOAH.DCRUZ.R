### Question 1
### A dataframe is a table like structure that has columns(variables) and rows(data points or observations)

s <- data.frame(ID = 1:3, Name = c("N1","N2","N3"),height=c(60,70,80))

s


### Question 2
cars <- c("Truck","Car", "SUV")
mpg <- c(11, 30, 24)
cost <- c(45000,25000,35000)

df <- data.frame(cars, mpg, cost)

print(df)

### Question 2a

df[1,3]
df[1:3,]
df[,ncol(df)]

### Question 3

head(mtcars,3)
tail(mtcars, 5)

### Question 4
## Firdt one is categorical and second is continuous
str(mtcars)
cat <- c("vs",'gear')
con <- c("drat","wt")
print(cat)
print(con)

### Question 5
### ggplot(mtcars, aes(x-disp, y=mpg)) This is an error: could not find function ggplot
### geom_point is missing
install.packages("ggplot2")
library(ggplot2)

ggplot(mtcars, aes(x=disp, y=mpg)) + geom_point()

###Question 6

data(mpg)
ggplot(data = mpg, aes(x=displ, y=hwy, color = factor(cyl))) + geom_point() + labs(color='Cylinders')
### leaving my comments here
### There seems to be a inverse/negative relationship between displ and hwy. As displ goes up hwy goes down.
### Each cylinder has varied hwy and displ

### Question 7
ggplot(data = mpg, aes(x=displ, y=hwy, color = factor(cyl))) + geom_point() + facet_wrap(~drv) + labs(color='Cylinders')

### Rear wheel drives have more cylinders, have larger displ and lower hwy