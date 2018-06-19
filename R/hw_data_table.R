library(data.table)
library(ggplot2)

rm(list=ls())

## NOTE: Exercises taken from:
## http://r-tutorials.com/r-exercises-51-60-data-pre-processing-data-table/

library(ISLR)
dtcollege = data.table(College, keep.rownames = T); class(dtcollege)

## NOTE: 1. ‘College’ dataset – Basic row manipulations

## a. Transform ‘College’ from ‘ISLR’ to data.table. Make sure to keep the
## University identifier. We will use this new data.table called ‘dtcollege’
## throughout this block of exercises.
dtcollege = data.table(College, keep.rownames = T); class(dtcollege)

## b. Get familiar with the dataset and its variables.
?College
head(dtcollege)
summary(dtcollege)

## c. Extract rows 40 to 60 as a new data.table (‘mysubset’).
mysubset = dtcollege[40:60]; mysubset

## d. What is the average enrollment number in this subset?
mean.enrollment = mysubset[,mean(Enroll)]; mean.enrollment

## e. Round the average enrollment to one digit.
round(mean.enrollment, digits = 1)


## NOTE: 2. ‘College’ dataset – Advanced row selection

## a. Get a data.table with all rows except the ones with an ‘Outstate’ fee
## between 8000-14000 USD
dtcollege[!(Outstate) %between% c(8000,14000)]


## NOTE: 3. ‘College’ dataset – Basic column operations

## a. What are the top 10 Universities in terms of top
## 10% High School students (Top10perc)?
head(dtcollege[order(-Top10perc), rn],10)

## b. What are the top 10 Universities in terms of student enrollment vs. accepted
## applications (highest student enrollment ratio)? Add a new column to the
## data.table. Code this exercise step by step in several lines.
earatio = dtcollege$Enroll/dtcollege$Accept
dtcollegeplus = data.table(dtcollege, earatio); head(dtcollegeplus)
head(dtcollegeplus[order(-earatio), .(rn, earatio, Enroll, Accept)],10)

## c. What are the top 10 Universities in terms of favorable S.F.Ratio with a
## Room.Board cost lower 4000 USD?
rb4000 = dtcollege[Room.Board < 4000]
head(rb4000[order(S.F.Ratio), .(rn, S.F.Ratio, Room.Board)],10)

## NOTE: 4. ‘College’ dataset – Permanently changing the column order

## a. Create a new data.table ‘college.gradnr’ which consists of: ‘rn’,
## ‘F.Undergrad’, ‘P.Undergrad’, ‘Accept’, ‘Enroll’, ‘Apps’.
college.gradnr = dtcollege[,
                           c("rn", "F.Undergrad", "P.Undergrad", "Accept",
                             "Enroll", "Apps"),
                           with = F];
head(college.gradnr)

## b. Permanently change the column order as seen below:
setcolorder(college.gradnr, c(1,6,4,5,2,3)); head(college.gradnr)


## NOTE: 5. ‘College’ dataset – Adding and removing new columns

## a. Add a new column called ‘HighInterest’ to the data.table. The column has
## an integer 1 for all observations with a number of applications higher 1000.
dtcollege[Apps > 10000, HighInterest := 1L]; tail(dtcollege)

## b. Remove the ‘HighInterest’ column again.
dtcollege[, c("HighInterest") := NULL]; tail(dtcollege)


## NOTE: 6. ‘College’ dataset – Adding new columns; Advanced

## a. Add a new column ‘undergradratio’ which is the undergraduate ratio
## (F.Undergrad/P.Undergrad). Get the first ten observations starting with the
## highest ratio. The whole exercise should be coded in one line – Use chaining!
head(dtcollege[,undergradratio := F.Undergrad/P.Undergrad]
     [order(-undergradratio)],10)


## NOTE: 7. ‘College’ dataset – Counting observations

## a. How many Universities have instructional expenditures of over 20000 USD per
## year?
dtcollege[Expend > 20000, .N]

## b. How many Universities have a combined ‘Books’ and ‘Room.Board’ costs of over
## 7000 USD per year?
dtcollege[Books+Room.Board > 7000, .N]

## c. How many Universities are public and how many are private?
dtcollege[, by = Private, .N]


## NOTE: 8. ‘College’ dataset – Working with keys and subsetting

## a. Set two keys to your ‘College’ data.table: ‘F.Undergrad’ and
## ‘P.Undergrad’. Check if the order has changed.
head(dtcollege) setkey(dtcollege, F.Undergrad, P.Undergrad);

## b. Get a subset of the ‘College’ data with ‘F.Undergrad’ lower 1000 and
## ‘P.Undergrad’ lower 100 students.
dtcollege[F.Undergrad < 1000 & P.Undergrad < 100]

## c. Is there a college with exactly 393 full-time and 4 part-time
## undergraduate students?
dtcollege[.(393, 4)]


## NOTE: 9. ‘College’ dataset – Selecting existing columns and reshaping

## a. Get a data.table with all columns except ‘Apps’, ‘Accept’, ‘Enroll’. Use at
## least two different ways for this.
dtcollege[, !c("Apps", "Accept", "Enroll"), with = F]
dtcollege[, -c("Apps", "Accept", "Enroll"), with = F]

## b. Get a data.table with the three columns ‘Apps’, ‘Accept’, ‘Enroll’. Use at
## least two different code efficient methods.
dtcollege[, Apps:Enroll, with = F]
dtcollege[, 3:5, with = F]


## NOTE: 10. ‘College’ dataset – Getting counts for grouped data

## a. How many Colleges with less than 800 applications received, have a Top 10
## student percentage above 40?
dtcollege[Apps < 800, .N, by = (Top10perc > 40)]

## b. How many Colleges with less than 900 applications received and an ‘Out of
## state tuition’ below 10000, have a top 10 student percentage above 30?
dtcollege[Apps < 900 & Outstate < 10000, .N, by = (Top10perc > 30)]

## c. How many Colleges with less than 1000 applications received, have a
## ‘Top10perc’ above 20 OR a ‘Top25perc’ above 30?
dtcollege[Apps < 1000, .N, by = (Top10perc > 20 | Top25perc > 30 )]
