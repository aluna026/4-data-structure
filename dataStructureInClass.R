# What packages do we need?
library(tidyverse)

# Character data (strings)
#  

# Just reading/cleaning data for the example...we come back to pivoting later
ds <- read_csv('data_cleaned/vocab_annoying_headers.csv') %>% 
  mutate(id = "Jonah", dob = as.Date("2017-08-30"), .before = "age_12")
ds <- ds %>% pivot_longer(-(id:dob), names_to = "age", values_to = "word", values_drop_na = T, names_prefix = "age_") %>%
  select(-id, -dob)

# Jonah's first words

ds$word

# Length, containing strings, counting strings
# mutate creates 
ds %>% mutate(word_length = str_length(word), # counts length of word (how many chrs)
              has_b = str_detect(word, "b"),  # str detect shows which word has B (T or F)
              num_b = str_count(word, "b"))   # how many B's does it have? counts them 

# Matching patterns for the "word" column

ds %>% mutate(contains_punct = str_detect(word, "[:punct:]")) # anything in this class (any that contain punctuation will have TRUE)
ds %>% mutate(word_cleaned = str_remove_all(word, "[:punct:]")) # removes punctuation
ds %>% mutate(word_cleaned = str_replace_all(word, "[:punct:]"," ")) # replace punctuations and add space
ds %>% mutate(vowel_count = str_count(word, "[aeiou]")) # counting vowels -- [^aeiou] is anything but these

fnames <- list.files(path="data_raw", full.names = T)
str_split(fnames, "/") 
str_replace_all(fnames, "data_raw/|.csv|vocab","") 
str_extract_all(fnames, "\\d{2}.\\d{1}|\\d{2}", simplify = T) 
str_extract_all(fnames, "\\.[:alpha:]{1,4}", simplify = T)  # \\. looks for literal period
str_extract_all(ds$word, boundary("word"), simplify = T) 

# Combining strings

age <- seq(from = 12, to = 24, by = .5) 
paste0("data_raw/vocab", age, ".csv") 
str_glue("data_raw/vocab{age}.csv") # stringr version of paste0
  # you can create a sequencing variable and gives the same output of paste0

# Logicals from text

# logical variables can sue a lot of different operators
contains_b <- str_detect(ds$word, "b") # TRUE if the word contains b
contains_b 
typeof(contains_b) 
sum(contains_b) 
mean(contains_b) 
!contains_b 
ds$word[contains_b] 
ds %>% mutate(contains_b = contains_b) 
ds %>% filter(contains_b) 
ds %>% mutate(b_and_vowels = contains_b & str_detect(word, "[aeiou]"))
ds <- ds %>% mutate(b_or_startswithd = contains_b | str_starts(word, "d"))  # stores this as an object

# Vectors

v <- c(1, 2, 3, 4, 5)
v
typeof(v) # double
length(v) # there are 5
v[1] # Use brackets to access a vector element
  # in this case its pulling out the actual 1 (the number 1 in the vector)
v[3]
v <- c(v, 6, 7, 8) # use c to add items (this is making a new vector)
v <- c(1, 2, 3, "4", "5", "6") # 3 numbers and 3 characters -- BUT it only makes a full chr vector
v # You cannot mix types in a vector, one of them will force the others to be the same (e.g., chr will force all of them to be chr)
v <- c(x = 1, y = 2, z = 3) # Named vector
v
v['y'] # show us the y value in vector v

files <- list.files(path = "data_raw")
files

files[1] # Character vector, this gives us the vocab12.5.csv since its the first one in the vector
files[2] # this gives us vocab12.csv
length(files) 

# Lists

lt <- list(x = 1, y = "3", z = 1:5) #int, character 3, and list 1-5
lt # shows that there are different lists
lt$z # Access named list elements with $ -- in this case its calling z (1-5)
lt[[1]] # Use double brackets to get list elements -- this is saying give me x
lt[[3]] <- 5:7 # this is creating a new output for when list 3 is called

# List of lists
lt <- list(lt1 = lt, lt2 = lt, lt3 = lt)
lt$lt1 # so now you get 1, 3, and 5-7 instead of 1-5

model <- lm(height ~ mass, data = starwars)
typeof(model)
model$coefficients


# Working with dates and times
# date times are character data and usually always in quotations

dates <- tibble(dates1 = c("2017-09-01", "2016-09-01", "2015-09-01", "2014-09-01"),
                dates2 = c("9/01/2018", "09/01/2017", "9/1/2016", "9/1/2015"),
                dates3 = c("March 1, 2019", "May 1, 2018", "January 20, 2017", "February 4, 2020"))

# Parsing dates

dates
dates %>% mutate(dates1_parsed = ymd(dates1)) # already in year month date but making sure they all output as ymd
dates %>% mutate(dates2_parsed = mdy(dates2)) # in month day year formate and turns into ymd
dates %>% mutate(dates3_parsed = parse_date(dates3, format = "%B %d, %Y")) # create it as a date time but format is in quotations
  # % is regular expression (B = month, day, year)

# Time is weird

time_length(mdy("05-27-2022")-mdy("05-27-1983"), "years")
time_length(as.period(interval(mdy("05-27-1983"), mdy("05-27-2022"))), "years")
time_length(as.period(interval(mdy("05-27-1983"), mdy("05-28-2022"))), "years")

# Age math

birth_date <- ymd("2001-09-01", "2000-09-01")
test_date <- ymd("2020-09-01", "2019-09-01")
(test_date-birth_date)/365.25
as.period(interval(start = birth_date, end = test_date), unit = "years")

test_date2 <- test_date - years(1) + days(7)
test_date2
(test_date2-birth_date)/365.25
as.period(interval(start = birth_date, end = test_date2), unit = "years")

# Time Zones

t1 <- as_datetime("2022-01-27 10:39:28", tz = "America/Los_Angeles")
t1
tz(t1)
t2 <- as_datetime("2022-01-27 13:39:28", tz = "America/New_York") # this means theyre both in the samae timezone and no time difference just outpute in EST vs PST
t2-t1
t3 <- as_datetime("2021-10-15 10:39:28", tz = "America/Los_Angeles")
t3 # this is pacific daylight due to time change
t4 <- as_datetime(now())
t4
as.period(interval(start = t1, end = t4), unit = "days") # using pacific time (PST and PDT)
with_tz(t4, "America/New_York") # this gives us the date/time with any timezone we ask for (same time but printed in EDT)
force_tz(t4, "America/New_York") # this is now forcing the EDT time to match the time to be in PDT (both at 14hr)

# Factors represent categories

x <- c(1, 1, 2, 3, 1, 4)
x <- factor(x, levels = c(1,2,3), labels = c("rarely", "neutral", "frequently")) # making the numbers mean different levels, anything outside these labels will be missing
x

# Factors

ds <- starwars
unique(ds$eye_color)
ds <- ds %>% mutate(eye_f = factor(eye_color)) # creating new variable of eye thats factor
fct_count(ds$eye_f, sort = T) %>% slice_head(n = 5) # only showing us 5 rows of data (without it, it will give all 15), gives us frequency table
  # highest frequency is at the top
fct_count(ds$eye_f, prop = T) %>% slice_head(n = 5) # in order of the dataset since were not using sort

# Setting factors levels

ds <- ds %>% select(name, eye_f)
ds 
ds %>% mutate(eye_f2 = factor(eye_f, levels = c("blue", "brown"))) 
ds %>% mutate(eye_f2 = factor(eye_f, levels = c("blue", "brown"), labels = c("bl","br"))) 
ds %>% mutate(eye_f2 = fct_lump(eye_f, n = 3)) 
ds %>% mutate(eye_type = fct_collapse(eye_f, human = c("blue", "brown"), nonhuman = c("yellow", "red"))) 
  # blue-grey remains the same so you want to make sure that all the original factors are included to be renamed otherwise those will stay the same
# Reordering factors levels

fct_count(ds$eye_f) 
fct_count(fct_rev(ds$eye_f)) 
fct_count(fct_infreq(ds$eye_f)) # most to least frequently occurring
fct_count(factor(ds$eye_f, levels = c("red", "brown", "blue"))) 

### Tidying Data

# Data Transformations

# Multiple Columns -> One Variable 

ds <- read_csv('data_cleaned/vocab_annoying_headers.csv') %>% 
  mutate(id = "Jonah", dob = as.Date("2017-08-30"), .before = "age_12")
ds %>% print(n = 25) 
ds %>% pivot_longer(-(id:dob), names_to = "age", values_to = "word") 
ds %>% pivot_longer(-(id:dob), names_to = "age", 
                    names_prefix = "age_", values_to = "word")
ds %>% pivot_longer(-(id:dob), names_to = "age", 
                    values_to = "word", values_drop_na = T, names_prefix = "age_")

# One Column -> Multiple variables

ds <- read_delim(list.files(path = "data_headers", full.names = T),
                 col_names = c("field", "value"), col_types = c("cc"), 
                 delim = " ", n_max = 8, id = "filename")
print(ds)

ds <- ds %>% pivot_wider(id_cols = "filename", 
                         names_from = "field", 
                         values_from = "value")
ds

# One Variable -> Multiple Variables

ds %>% separate_wider_delim(filename, delim = "/", names = c("dir", "file"))

# Multiple Variables -> One Variable

ds %>% unite(col = "block_speed_condition", Block, speed, condition, sep = "_")

# Binding Columns

star_wars_numeric <- starwars %>% select(where(is.numeric)) #87x3
star_wars_numeric
star_wars_char <- starwars %>% select(where(is.character)) #87x8
star_wars_char
star_wars_combined <- bind_cols(star_wars_numeric, star_wars_char) #87x11
star_wars_combined

# Binding Rows

s1 <- starwars %>% slice_sample(n = 2) #2x14
s1
s2 <- starwars %>% slice_sample(n = 2) #2x14
s2
s3 <- starwars %>% slice_sample(n = 2) #2x14
s3
star_wars_combined <- bind_rows(s1, s2, s3) #6x14
star_wars_combined

# Joining by key value

fnames <- list.files(path = "data_headers", full.names = T)
header <- read_delim(fnames, col_names = c("field", "value"), col_types = c("cc"), delim = " ", n_max = 7, id = "filename") %>% pivot_wider(id_cols = "filename", names_from = "field", values_from = "value")
trials <- read_tsv(fnames, col_names = c("trial_num", "speed_actual", "speed_resp", "correct"), skip = 8, id = "filename")
header 
trials 
left_join(header, trials, by = "filename") 
left_join(slice_sample(header, n = 5), trials, by = "filename") 
left_join(slice_sample(trials, n = 5), header, by = "filename") 

