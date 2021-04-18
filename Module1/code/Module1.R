#Loading data into R
library(readr)
#Working with data from files
#Working with well-structured data from files or URLs
#LOADING WELL-STRUCTURED DATA FROM FILES OR URLS
#Listing 2.1 Reading the UCI car data
uciCar <- read_csv("GitHub/cs466/Module1/data/car.data.csv")
#EXAMINING OUR DATA
#Listing 2.2 Exploring the car data
class(uciCar)
summary(uciCar)
dim(uciCar)

#Exploring data
custdata <- read_delim("GitHub/cs466/Module1/data/custdata.tsv","\t", escape_double = FALSE, trim_ws = TRUE)
#Using summary statistics to spot problems
#Listing 3.1 The summary() command
summary(custdata)
#Typical problems revealed by data summaries
#MISSING VALUES
#Listing 3.2 Will the variable is.employed be useful for modeling?
summary(custdata$is.employed)
#INVALID VALUES AND OUTLIERS
summary(custdata$income)
summary(custdata$age)
#DATA RANGE
#Listing 3.4 Looking at the data range of a variable
summary(custdata$income)
#UNITS
Income = custdata$income/1000
class(Income)
summary(Income)
#Spotting problems using graphics and visualization
summary(custdata$age)
#HISTOGRAMS
#Listing 3.6 Plotting a histogram
library(ggplot2) 
ggplot(custdata) +
  geom_histogram(aes(x=age),binwidth=5, fill="gray") 
#DENSITY PLOTS
ggplot(custdata) + geom_density(aes(x=income)) 
#Listing 3.8 Creating a log-scaled density plot
ggplot(custdata) + geom_density(aes(x=income)) +
  scale_x_log10(breaks=c(100,1000,10000,100000)) +
  annotation_logticks(sides="bt")
#BAR CHARTS
#Listing 3.9 Producing a horizontal bar chart
ggplot(custdata) +
  geom_bar(aes(x=state.of.res), fill="gray") +
  coord_flip() +
  theme(axis.text.y=element_text(size=rel(0.8)))
#Listing 3.10 Producing a bar chart with sorted categories
statesums <- table(custdata$state.of.res)
statef <- as.data.frame(statesums)
colnames(statef)<-c("state.of.res", "count")
summary(statef)
statef <- transform(statef,state.of.res=reorder(state.of.res, count))
summary(statef)
ggplot(statef)+ geom_bar(aes(x=state.of.res,y=count),
                         stat="identity",
                         fill="gray") +
  coord_flip() +
  theme(axis.text.y=element_text(size=rel(0.8)))

#Visually checking relationships between two variables
x <- runif(100)
y <- x^2 + 0.2*x
ggplot(data.frame(x=x,y=y), aes(x=x,y=y)) + geom_line()

#SCATTER PLOTS AND SMOOTHING CURVES
#Listing 3.12 Examining the correlation between age and income
custdata2 <- subset(custdata,
                    (custdata$age > 0 & custdata$age < 100
                     & custdata$income > 0))
cor(custdata2$age, custdata2$income)

ggplot(custdata2, aes(x=age, y=income)) +
  geom_point() + ylim(0, 200000)

ggplot(custdata2, aes(x=age, y=income)) + geom_point() +
  stat_smooth(method="lm") +
  ylim(0, 200000)

ggplot(custdata2, aes(x=age, y=income)) +
  geom_point() + geom_smooth() +
  ylim(0, 200000)

#Listing 3.13 Plotting the distribution of health.ins as a function of age
ggplot(custdata2, aes(x=age, y=as.numeric(health.ins))) +
  geom_point(position=position_jitter(w=0.05, h=0.05)) +
  geom_smooth()

#HEXBIN PLOTS
#Listing 3.14 Producing a hexbin plot
library(hexbin)
ggplot(custdata2, aes(x=age, y=income)) +
  geom_hex(binwidth=c(5, 10000)) +
  geom_smooth(color="white", se=F) +
  ylim(0,200000)

#BAR CHARTS FOR TWO CATEGORICAL VARIABLES
#Stacked bar chart, the default
ggplot(custdata) + geom_bar(aes(x=marital.stat,
                                fill=health.ins))
#Side-by-side bar chart
ggplot(custdata) + geom_bar(aes(x=marital.stat,
                                fill=health.ins),
                            position="dodge")
#Filled bar chart
ggplot(custdata) + geom_bar(aes(x=marital.stat,
                                fill=health.ins),
                            position="fill")

#Listing 3.16 Plotting data with a rug
ggplot(custdata, aes(x=marital.stat)) +
  geom_bar(aes(fill=health.ins), position="fill") +
  geom_point(aes(y=-0.05), size=0.75, alpha=0.3,
             position=position_jitter(h=0.01))
#Listing 3.17 Plotting a bar chart with facets
ggplot(custdata2) +
  geom_bar(aes(x=housing.type, fill=marital.stat ),
           position="dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Listing 3.17 Plotting a bar chart without facets
ggplot(custdata2) +
  geom_bar(aes(x=marital.stat), position="dodge",
           fill="darkgray") +
  facet_wrap(~housing.type, scales="free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Managing data
#Cleaning data
#Treating missing values (NAs)
#Listing 4.1 Checking locations of missing data
summary(custdata[is.na(custdata$housing.type),
                 c("recent.move","num.vehicles")])
#Listing 4.2 Remapping NA to a level
custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed),
                                   "missing",
                                   ifelse(custdata$is.employed==T,
                                          "employed",
                                          "not employed"))
summary(as.factor(custdata$is.employed.fix))
custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed),
                                   "not in active workforce",
                                   ifelse(custdata$is.employed==T,
                                          "employed",
                                          "not employed"))
#MISSING VALUES IN NUMERIC DATA
summary(custdata$income)
#WHEN VALUES ARE MISSING RANDOMLY
meanIncome <- mean(custdata$income, na.rm=T)
meanIncome
Income.fix <- ifelse(is.na(custdata$income),
                     meanIncome,
                     custdata$income)
summary(Income.fix)
#WHEN VALUES ARE MISSING SYSTEMATICALLY
#Listing 4.3 Converting missing numeric data to a level
breaks <-c(0, 10000, 50000, 100000, 250000, 1000000)
Income.groups <-
  cut(custdata$income,
      breaks=breaks, include.lowest=T)
summary(Income.groups)
Income.groups <- as.character(Income.groups)
Income.groups <- ifelse(is.na(Income.groups),
                        "no income", Income.groups)
summary(as.factor(Income.groups))
#Listing 4.4 Tracking original NAs with an extra categorical variable
missingIncome <- is.na(custdata$Income)
Income.fix <- ifelse(is.na(custdata$Income), 0, custdata$Income)

#Data transformations
custdata <- merge(custdata, medianincome,
                  by.x="state.of.res", by.y="State")
summary(custdata[,c("state.of.res", "income", "Median.Income.x")])
custdata$income.norm <- with(custdata, income/Median.Income.x)
summary(custdata$income.norm)

#CONVERTING CONTINUOUS VARIABLES TO DISCRETE
custdata$income.lt.20K <- custdata$income < 20000
summary(custdata$income.lt.20K)
#Listing 4.6 Converting age into ranges
brks <- c(0, 25, 65, Inf)
custdata$age.range <- cut(custdata$age,
                          breaks=brks, include.lowest=T)
summary(custdata$age.range)

#NORMALIZATION AND RESCALING
#Listing 4.7 Centering on mean age
summary(custdata$age)
meanage <- mean(custdata$age)
meanage
custdata$age.normalized <- custdata$age/meanage
summary(custdata$age.normalized)

#Listing 4.8 Summarizing age
summary(custdata$age)
meanage <- mean(custdata$age)
meanage
stdage <- sd(custdata$age)
stdage
custdata$age.normalized <- (custdata$age-meanage)/stdage
summary(custdata$age.normalized)


#Sampling for modeling and validation
#Creating a sample group column
#Listing 4.9 Splitting into test and training using a random group mark
custdata$gp <- runif(dim(custdata)[1])
testSet <- subset(custdata, custdata$gp <= 0.1)
trainingSet <- subset(custdata, custdata$gp > 0.1)
dim(custdata)
dim(testSet)[1]
dim(trainingSet)[1]

#Record grouping
#If you’re modeling a question at the household
#level rather than the customer level, then
#every member of a household should be in the
#same group (test or training).
#4.10 Ensuring test/train split doesn’t split inside a household
hh <- unique(hhdata$household_id)
households <- data.frame(household_id = hh, gp = runif(length(hh)))
hhdata <- merge(hhdata, households, by="household_id")
head(hhdata)
