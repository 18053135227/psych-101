The `flights` package enables automated download of On-Time flights data from the [Bureau of Transportation Statistics](http://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236), using the function `downloadflightsdata`. 

The function `flights` can be used to load the entire flights data so that one can filter the data one requires easily. It automatically downloads logs that doesn't exist.

In addition, it contains functions for generating flights that departed from NYC (i.e. `JFK`, `LGA` or `EWR`), similar to the [nycflights13](https://github.com/hadley/nycflights13) package, except for year 2014. It also provides additional functions to generate `delay` and `weather delay` data during the same period. 

The [README](https://github.com/arunsrinivasan/flights/blob/master/README.md) file and the function man pages should be sufficient to use these functions.

I'd also like to add functions to automate generate `weather` reports for any set of airports for a given year, and use it to generate weather reports for 2014. Along with `flights` and `weather delay` data sets, it should provide enough to play with.

These data sets can also be used in `data.table` vignettes as well.
