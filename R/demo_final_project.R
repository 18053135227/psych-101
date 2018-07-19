library('data.table')
library('ggplot2')
library('gtools')

rm(list = ls())

dir_data_delay = '../final_project/data/delay/'
f_name <- 'delay_fb_sub201.txt'
f <- paste(dir_data_delay, f_name, sep='')

d <- fread(f)

col_names <- c(
  'trial',
  'trial_prob',
  'category boundary',
  'category label',
  'stimulus x value',
  'stimulus y value',
  'response',
  'reaction time'
)

setnames(d, col_names)
