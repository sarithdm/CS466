#Ordered and unordered factors
#A factor is a vector object used to specify a discrete classification (grouping) of the components
#of other vectors of the same length
#R provides both ordered and unordered factors.
#4.1 A specific example
#Suppose, for example, we have a sample of 30 tax accountants from all the states and territories
#of Australia1 and their individual state of origin is specified by a character vector of state
#mnemonics as
state <- c("tas", "sa", "qld", "nsw", "nsw", "nt", "wa", "wa",
           "qld", "vic", "nsw", "vic", "qld", "qld", "sa", "tas",
           "sa", "nt", "wa", "vic", "qld", "nsw", "nsw", "wa",
           "sa", "act", "nsw", "vic", "vic", "act")
#Notice that in the case of a character vector, "sorted" means sorted in alphabetical order.
# factor is similarly created using the factor() function:
statef <- factor(state)
statef

#To find out the levels of a factor the function levels() can be used.
levels(statef)
#4.2 The function tapply() and ragged arrays
#suppose we have the incomes of the same tax accountants in
#another vector (in suitably large units of money)
incomes <- c(60, 49, 40, 61, 64, 60, 59, 54, 62, 69, 70, 42, 56,
             61, 61, 61, 58, 51, 48, 65, 49, 49, 41, 48, 52, 46,
             59, 46, 58, 43)
incomes
#To calculate the sample mean income for each state we can now use the special function
#tapply():
incmeans <- tapply(incomes, statef, mean)
incmeans
#Suppose further we needed to calculate the standard errors of the state income means.
stderr <- function(x) sqrt(var(x)/length(x))
incster <- tapply(incomes, statef, stderr)
incster
#4.3 Ordered factors
#Sometimes the levels will have a natural ordering that we want to record and want our
#statistical analysis to make use of. The ordered() function creates such ordered factors but
#is otherwise identical to factor
stateo <- ordered(state)
stateo

#5 Arrays and matrices
#An array can be considered as a multiply subscripted collection of data entries, for example
#numeric.

#A dimension vector is a vector of non-negative integers. If its length is k then the array is
#k-dimensional, e.g. a matrix is a 2-dimensional array. The dimensions are indexed from one up
#to the values given in the dimension vector
#A vector can be used by R as an array only if it has a dimension vector as its dim attribute.
#Suppose, for example, z is a vector of 1500 elements.
#> dim(z) <- c(3,5,100)
#For example if the dimension vector for an array, say a, is c(3,4,2) then there are 3  4 
#2 = 24 entries in a and the data vector holds them in the order a[1,1,1], a[2,1,1], ...,
#a[2,4,2], a[3,4,2].
#in that order. a[,,] stands for the entire array, which is the same as omitting the subscripts
#entirely and using a alone
a <- array(1:20)
a
a[1]
a[20]
a[-1]
#5.3 Index matrices
x <- array(1:20, dim=c(4,5))
x
y <- array(21:40, dim=c(4,5))
y
#The array() function

Z <- array(1:24, dim=c(3,4,2))
Z

Z <- array(1:36, dim=c(3,4,3))
Z

Z <- array(1:72, dim=c(3,4,3,2))
Z

Z <- array(1:144, dim=c(3,4,3,2,2))
Z

x <- array(1:60)
x


Z <- array(x, dim=c(5,2,2))
Z

Z <- array(x, dim=c(5,4,2))
Z

A <- array(x, dim=c(5,2,1))
A

B <- array(y, dim=c(5,2,1))
B

A+B
A*B
B+1

A
B
#5.5 The outer product of two arrays
#If a and b are two numeric arrays,
#their outer product is an array whose dimension vector is obtained by concatenating their two
#dimension vectors (order is important), and whose data vector is got by forming all possible
#products of elements of the data vector of a with those of b.
p = c(1,2,3)
q =c(10,20)
pq <- outer(p, q, "*")
pq

qp <- outer(q, p, "*")
qp


a = c(10,100)
b = c(1,2,3,4)
ab <-outer(a,b,"*")
ab
xy <- outer(x, y, "*")
x

#5.7 Matrix facilities

x <- array(1:20, dim=c(4,5))
x
y <- array(21:40, dim=c(4,5))
y

y <-t(y)
y

#matrix of element by element products a
x * y

x <- array(c(10,20,12,30), dim=c(2,2))
x
y <- array(c(5,7,6,8), dim=c(2,2))
y
x * y

x %*% y
x <- array(1:4, dim=c(2,2))
x
y <- array(10:13, dim=c(2,2))
y

x * y

#matrix product
x %*% y

y <- t(y)

x %*% y



#6 Lists and data frames
#An R list is an object consisting of an ordered collection of objects known as its components.
Lst <- list(name="Fred", wife="Mary", no.children=3,
            child.ages=c(4,7,9))

length(Lst)
Lst
Lst$name
Lst$no.children


list.A <- list(name1="Fred")
list.B <- list(name2="John")
#6.2.1 Concatenating lists

list.AB <- c(list.A, list.B)

list.AB$name1
list.AB$name2

#Data frames


mydata <- data.frame(c1 = c(1, 2, 3, 4, 5),         
                   c2 = c(6, 7, 8, 9, 10),
                   c3 = c(11, 12, 13, 14, 15))

mydata


mydata1 <- data.frame(c11 = c(1, 2, 3, 4, 5),         
                     c22 = c(6, 7, 8, 9, 10),
                     c33 = c(11, 12, 13, 14, 15))

mydata1

mydata1$c11

c11

mydata$c1

c1

#The attach function allows to access variables of a data.frame without calling the data.frame.

attach(mydata1)

c22


library(readr)

#Reading data from files
UCCar <- read_csv("GitHub/cs466/Module1/data/car.data.csv")
View(UCCar)
Car <- read.table("GitHub/cs466/Module1/data/car.data.csv",sep=",",header = TRUE)
View(Car)                  
                 

#Probability distributions
#Waiting time between eruptions and the duration of the eruption for the Old Faithful geyser in Yellowstone National Park, Wyoming, USA.
#A data frame with 272 observations on 2 variables.
#[,1]	eruptions	numeric	Eruption time in mins
#[,2]	waiting	numeric	Waiting time to next eruption (in mins)
attach(faithful)
summary(eruptions)
fivenum(eruptions)

stem(eruptions)

hist(eruptions)
## make the bins smaller, make a plot of density
hist(eruptions, seq(1.6, 5.2, 0.2), prob=TRUE)
lines(density(eruptions, bw=0.1))
rug(eruptions) # show the actual data points


#Statistical models in R

x <- c(0.02, 0.02, 0.06, 0.06, 0.11, 0.11, 0.22, 0.22, 0.56, 0.56,
       1.10, 1.10)
y <- c(76, 47, 97, 107, 123, 139, 159, 152, 191, 201, 207, 200)
plot(x, y)

lm.out=lm(y ~ x)
lm.out
abline(103.5, 110.4)
for (i in 1:12){ print(paste(i<-(110.4*x[i])+103.5))}


#find equation of the function which relates x and y.
age = c(19,22,25,30,35,41,46,48,50,53,57,60,64,70)
chol_level = c(4.1,4.3,4.3,4.4,4.5,4.6,4.7,4.6,5,5,5.1,5.3,5.5,5.6)
lm.out=lm(chol_level ~ age)
lm.out

plot(age,chol_level)
abline(3.53714, 0.02819)
