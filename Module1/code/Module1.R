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

