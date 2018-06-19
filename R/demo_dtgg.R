## NOTE: The following text is copied from codebook.txt
## This data was collected online and used the Wagner Preference Inventory. Wagner,
## Rudolph F., and Kelly A. Wells.
## "A refined neurobehavioral inventory of hemispheric preference." Journal of
## clinical psychology 41.5 (1985): 671-676.

## The WPI was appended as an optional survey on the end of several on line
## personality tests. At the end of each test the user would be asked if they would
## be willing to complete a short optional research survey. About half said yes and
## they were administered the WPI before they viewed their personality test
## results.

## The WPI has 12 questions. In each question the subject chooses from a group of
## four activities which they would like to do the most.

## Q1	1=Major in Logic, 2=Write a letter, 3=Fix things at home, 4=Major in art
## Q2	1=Be a movie critic, 2=Learn new words, 3=Improve your skills in a game, 4=Create a new toy
## Q3	1=Improve your strategy in a game, 2=Remember people's names, 3=Engage in sports, 4=Play an instrument by ear
## Q4	1=Review a book, 2=Write for a magazine, 3=Build new shelves at home, 4=Draw a landscape or seascape
## Q5	1=Analyze market trends, 2=Write a movie script, 3=Do carpentry work, 4=Imagine a new play
## Q6	1=Analyze management practices, 2=Locate words in a dictionary, 3=Put jigsaw puzzles together, 4=Paint in oil
## Q7	1=Be in charge of computer programming, 2=Study word origins and meaning, 3=Putter in the yard, 4=Invent a new gadget
## Q8	1=Analyze production costs, 2=Describe a new product in words, 3=Sell a new product on the market, 4=Draw a picture of a new product
## Q9	1=Explain the logic of a theory, 2=Be a copy writer for ads, 3=Work with wood and clay, 4=Invent a story
## Q10	1=Be a comparison shopper, 2=Read about famous men and women, 3=Run a traffic control tower, 4=Mold with clay and putty
## Q11	1=Analyze your budget, 2=Study literature, 3=Visualize and re-arrange furniture, 4=Be an artist
## Q11	1=Plan a trip and make a budget, 2=Write a novel, 3=Build a house or shack, 4=Make crafts your hobby

## After these questions the participant was asked if they has answered these
## questions accurately and to confirm their consent. Participants who answered
## "no" were not recorded.

## The following value was calculated from technical infromation:
## country	Where the user connected to from. ISO country code.

library(data.table)
library(ggplot2)

d <- fread('/Users/crossley/Downloads/Wagner/data.csv')

## Take a first look at the data
str(d)

## Also, data.table has a pretty clean print function, so I often just try:
print(d)

## or just:
d

## How many people responded to the survey?
d[, .N]

## How many people responded to the survey, per country?
dd <- d[, .N, .(country)]

## How many people responded to the survey, per top 10 countries?
dd[order(-N)][1:10]

## How many people responded to the survey, per top 10 countries?
## this time with a pretty graph.
ggplot(dd, aes(x=country, y=N)) +
  geom_bar(stat='identity')

## The graph might look better if we rank ordered the countries
## NOTE: This won't work because yuck.
ggplot(dd[order(-N)], aes(x=country, y=N)) +
  geom_bar(stat='identity')

## Deal with it here.
dd[, country := factor(country, )]
ggplot(dd, aes(x=reorder(as.factor(country), N), y=N)) +
  geom_bar(stat='identity')

## limit to top 10
ggplot(dd[1:10], aes(x=reorder(as.factor(country), N), y=N)) +
  geom_bar(stat='identity')

## NOTE: Wait, what is reorder(as.factor(country) doing? turns out that to order
## the bars according to N, ggplot needs us to turn country into a "factor" ...
## and the order those factors appear determine where ggplot will put them. It's
## a pain in the ass, honestly, but I'm sure there's a good reason for it. For
## now, put on your todo list: "learn about factors, and all other base R data
## types, too" ... this will be an excellent thing for our GSIs to help us with.
## # For now, lets poke around the data a bit more.

## How did Q1 shake out in the US?
## Q1	1=Major in Logic, 2=Write a letter, 3=Fix things at home, 4=Major in art
dd <- d[country=='US', Q1, .(country)]
ggplot(dd, aes(Q1)) +
  geom_bar(stat='count')

## Cool, but lets doll it up a bit
ggplot(dd, aes(x=factor(Q1, levels=c(1,2,3,4), labels=c('a','b','c','d')))) +
  geom_bar(stat='count') +
  xlab('Q1')

## NOTE: Ugh... more factor silliness. Remember to work hard to wrap
## your head around different R data types!

## Lets compare Q1 in the US and in AU (where I'm moving!)
dd <- d[country%in%c('US','AU'), Q1, .(country)]
ggplot(dd, aes(x=factor(Q1, levels=c(1,2,3,4), labels=c('a','b','c','d')))) +
  geom_bar(stat='count') +
  xlab('Q1') +
  facet_wrap(~country)

##  way more responders in US than in AU, so lets look at proportions instead
ggplot(dd, aes(x=factor(Q1, levels=c(1,2,3,4), labels=c('a','b','c','d')))) +
  geom_bar(aes(y=..prop.., group=1)) +
  xlab('Q1') +
  facet_wrap(~country)

## NOTE: There's some magic happening there fore sure....
## Sometimes you just gotta google it:
## https://stackoverflow.com/questions/36604127/creating-a-bar-plot-with-proportions-on-ggplot

## TODO: error bars or something that shows variability in a statistic
## TODO: R simulation-based walk thru of the 5 step program
