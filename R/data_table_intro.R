## TODO: need to get going with R studio and plan on doing all this shit from
## there...
## TODO: put data.table cheat sheet in folder
## Essentially everything is taken from here, by I ommit things that I'm
## guessing would be a bit much for the eager young minds of Psych 101.
## https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
## I saved it here as well:
## data_table_intro.pdf

library(data.table)
library(ggplot2)

## 1. Basics
## a) What is data.table?
##   data.table is an R package that provides an enhanced version of
##   data.frames. In the Data section, we already created a data.table using
##   fread(). We can also create one using the data.table() function. Here is an
##   example:
DT = data.table(ID = c("b","b","b","a","a","c"), a = 1:6, b = 7:12, c = 13:18)
DT

## Introduction to class()
## Basically, this is a reminder to pay attention to learn about data types...
## which I will surely repeat over and over again throughout the course
class(DT$ID)

## What's the deal with data.frames and data.tables?
## They look pretty darn similar.
DF = data.frame(ID = c("b","b","b","a","a","c"), a = 1:6, b = 7:12, c = 13:18)
DF

## If they are really the same, then shouldn't something like this work?
DF == DT

## Well, clearly they aren't exactly the same, or else we wouldn't have the
## above very annoying error. In any case, they are similar enough that it is
## trivial to convert one to another
DT2 = as.data.table(DF)
DT2 == DT

## NOTE: The technical relationship between data.frame and data.table involves
## the scary computer science word "subclass." In particular, data.table is a
## subclass of data.frame. This means that it must be able to do everything a
## data.frame can do, but that it can also do more. I think of data.table as
## data.frame on steroids. The HTML linked on line 1 above will tell you a bit
## more, if you'd care to know it.
class(DT)


## b) General form - in what way is a data.table enhanced?
##   In contrast to a data.frame, you can do a lot more than just subsetting
##   rows and selecting columns within the frame of a data.table, i.e., within [
##   ... ]. To understand it we will have to first look at the general form of
##   data.table syntax, as shown below:
## DT[i, j, by]
##   R:      i                 j        by
## SQL:  where   select | update  group by

## NOTE: SQL is a type of database -- which it turns out is a whole big thing in
## computer science -- but to us, and indeed, most of the stats and data science
## world, a database is simply a way to store and access lots of data. For a
## variety of reasons I hope you will find intuitive, it storing loads of data
## in excel or any other spreadsheet is not the greatest idea ever.

## Okay, if you were already familiar with data.frame, then this probably makes
## some sense to you. If not, probably not so much. Lets take a sec address what
## "subsetting" actually means.

# NOTE: Begin subsetting
## Look at the first two rows
DT[1:2]

## Look at the last row... note the fancy use of .N below, which is data.table
## thing -- that is, not a thing data.frame has -- that refers to the number of
## rows in the data.table. So, in this next call, we are asking to see the last
## row in the data.table
DT[.N]

## Look at rows 1, 3, and 5
DT[c(1,3,5)]

## NOTE: Begin sorting
## sort DT in increasing order based on column "a"
DT[order(a)]

## sort DT in decreasing order based on column "B"
DT[order(-b)]

## sort DT in increasing order on column "a" and then in decreasing order on
## column "b"
DT[order(a,-b)]

## Did that last call do anything?
## What's up with that?
DT[order(a)] == DT[order(a,-b)]

## NOTE: Lets return to that funky syntax from above
## DT[i, j, by]
##   R:      i                 j        by
## SQL:  where   select | update  group by
## d) Select column(s) in j

## select column "a" and return column "a" as a vector
## The leading comma in the below syntax is super important
DT[, a]

## PSA: Read about R data types. Vector is one such data type, and it will creep
## up often
class(DT[, a])

## select column "a" and return as another data.table
DT[, list(a)]

## looks like a data.table alright, but good practice to verify
class(DT[, list(a)])

## As far as I know, the reason why list() returns a data.table and just the
## column name returns a vector is idiosyncratic. You will learn to accept it.
## Also, as you will learn to love... I swear... data.table is laden with funky
## syntax. E.g., ".N" from above. Well, another funky bit is ".()" which is an
## alias for list()
DT[, .(a)]
class(DT[, .(a)])
DT[, list(a)] == DT[, .(a)]

## PSA: Read about R data types..."list" is another common data type.
## TODO: So, why have lists and vectors?
## TODO: I think we need a whole R file on data types... search for online
## tutorial

## select column a and b
DT[, .(a,b)]

## select a and rename to a_new
DT[, .(a_new = a)]

## select a and b and rename to a_new and b_new
DT[, .(a_new = a, b_new = b)]

## Can we select and rename without .() / list()?
## HINT: Nope. This all has to do with the magical inner workings of data.table,
## which are well beyond the scope of the course... and indeed, even my own
## current knowledge.
DT[,  a_new = a]

## select all columns except the "b" column
DT[, !c("b")]

## Notice above that I have to refer to "b" differently than if I were just
## calling b... just the way it is...
DT[, .(b)]

## NOTE: Begin: e) Compute or do in j

## In how many rows is a > b?
DT[, a > b]
DT[, sum(a > b)]

## For each row, what is the sum of a, b, and c.
DT[, a + b + c]

## Now return the last result as a data.table
## Notice the returned column is named -- generically -- V1
DT[, .(a + b + c)]

## NOTE: f) Subset in i and do in j

## What is the sum of all columns within the 'a' and 'b' IDs?
## Two different methods
DT[ID %in% c('a', 'b'), .(a+b+c)]
DT[!(ID %in% c('c')), .(a+b+c)]

## NOTE: 2. Aggregations

## We’ve already seen i and j from data.table’s general form in the previous
## section. In this section, we’ll see how they can be combined together with
## "by" to perform operations by group. Let’s look at some examples.

## NOTE: a) Grouping using by

## How many rows per ID?
## This is where data.table is really magic :)
DT[, .N, .(ID)] # .(is the general way of doing it)
DT[, .N, ID] # when only grouping by one variable, you don't HAVE to use .()

## The toy example we've been playing with so far is simple enough that you may
## not fully appreciate the awesomeness that is data.tale. So, lets level up:
## https://github.com/arunsrinivasan/flights/wiki/NYC-Flights-2014-data
flights <- fread("../data/flights.wiki/NYCflights14/flights14.csv")
flights

## This data might be a good opportunity to talk about big first meetings with
## big(ish) data.
## For instace, print methods can be more difficult to learn from.
## I think str() provides a nice summary in these cases
str(flights)

## I also often use unique() to probe larger data
unique(c(1,2,3,3,4,4,4,4,5))

## What carriers are present in the flights data?
flights[, unique(carrier)]

## How many carriers does the flights data span?
flights[, length(unique(carrier))]

## There's another cool way to answer this question.
flights[, .(unique(carrier))][, .N]

## What the heck is going on in that last line? First, we used .() --- remember
## inside of a data.table this is the same thing as list() --- and when we wrap
## up the "j" argument of a data.table in a list, we return another data.table
## Try it:
flights[, .(unique(carrier))]

## The second --- really freaking cool --- thing we did is called "chaining"
## Basically, data.table is smart enough to first evaluate the thing on the left
## (flights[, .(unique(carrier))]) and store it in a temporary variable. Lets
## call it "temp". Then data.table evaluates temp[, .N].
flights[, .(unique(carrier))][, .N]

## It's pretty cool stuff, and you can chain on and on and on if you like
## (assuming you pay attention to return data.tables from every step in the
## chain). This next example is kinda dumb, because why would you use an extra
## step if you didn't need to, but you may find situations where it makes more
## sense.
flights[, .(carrier)][, .(unique(carrier))][, .N]

## NOTE: Lets return to the magic of grouping
## How many trips for each origin airport?
flights[, .N, .(origin)]

## easy to group by more than one variable
flights[, .N, .(carrier, origin)]
flights[, .N, .(origin, carrier)]

## It's interesting to compare the output of the last two lines. It's clear that
## the data.table is sorted in some particular way, and the grouping order
## inlfuces which column is reported first, but not which column is used to sort
## the result. If you want to be sure the result will be sorted by some column
## in particular -- we know how to do that! -- use order()
flights[order(origin), .N, .(carrier, origin)]
flights[order(carrier), .N, .(carrier, origin)]

## But wait, there is yet another data.table way to order the results of
## grouping... simply use "keyby" in place of "by"
flights[, .N, keyby = .(carrier, origin)]
flights[, .N, keyby = .(origin, carrier)]

# what are the arrival delays for each combination of carrier, origin, and
# destination?
flights[, arr_delay, .(carrier, origin, dest)]

## TODO: There might be an opportunity here to get into "inference"... That is,
## start to answer questions / make decisions on the basis of what the data show

## Suppose you need to travel from JFK to LAX and you want to minimize your
## chances of experiencing a delay. Which airline should you fly?
flights[origin=='JFK' & dest=='LAX', .(arr_delay), .(carrier)]

## Hard to make a deicision based on the last line... there are too many lines
## of information for us to easily take in and interpret... we need to summarize
## it somehow... you might even say that we need to compute a summary statistic!
flights[origin=='JFK' & dest=='LAX', mean(arr_delay), .(carrier)]

## We might just say to take the minimum delay possible
flights[origin=='JFK' & dest=='LAX', mean(arr_delay), .(carrier)][order(V1)]
flights[origin=='JFK' & dest=='LAX', mean(arr_delay), .(carrier)][order(V1)][1]

## NOTE: Quick visit to the land of statistics:
## But are any of these differences "significant"???
## What the heck is a significant difference, anyway?
## When we ask if a difference is significant, we are really asking if we are
## confident that that difference will pan out again in the future

## Anyway, back to data.table awesomeness
## Get the average arrival and departure delay for each orig,dest pair for each
## month for carrier code 'AA'?
flights[carrier == "AA", mean(arr_delay), .(origin, dest, month)]
flights[carrier == "AA", mean(dep_delay), .(origin, dest, month)]

## Or... we can do it all at once... which is super cool
flights[carrier == "AA",
        .(mean(arr_delay), mean(dep_delay)),
        .(origin, dest, month)]


## NOTE: d) Expressions in by

## find out how many flights started late but arrived early (or on time)
## subsetting approach
flights[dep_delay>0 & arr_delay>0, .N]

## Or you can also get funky by using expressions in the group by arg
flights[, .N, .(dep_delay>0, arr_delay>0)]

## NOTE: e) Multiple columns in j - .SD
## Previously we looked at:
flights[carrier == "AA",
        .(mean(arr_delay), mean(dep_delay)),
        .(origin, dest, month)]

## Do we have to compute mean() for each column individually?
## Turns out nope, dat.table has a special variable named ".SD"
## .SD is just the entire data.table with the grouping columns removed
DT[, print(.SD), .(ID)]

flights[carrier == "AA", ## Only on trips with carrier "AA"
        lapply(.SD, mean), ## compute the mean
        by = .(origin, dest, month), ## for every 'origin,dest,month'
        .SDcols = c("arr_delay", "dep_delay")] ## for just those in .SDcols

## TODO: will we ever have cause to use .SD? I don't see it just yet.

## NOTE: f) Subset .SD for each group:
## return the first two rows for each month
flights[, head(.SD, 2), by = month]

## concatenate columns a and b for each group in ID
DT[, .(val = c(a,b)), by = ID]

## What if we would like to have all the values of column a and b concatenated, but
## returned as a list column?
DT[, .(val = list(c(a,b))), by = ID]
