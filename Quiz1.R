library(tm)

setwd("D:/Coursera/Data Science/Capstone Project/final/en_US")

#Question 3
con <- file("en_US.twitter.txt", "r")

max_line_length <- 0L

repeat
{
  line <- readLines(con, 1)
  
  if (length(line) < 1)
      break
  
  line_length <- nchar(line)
  if (line_length > max_line_length)
      max_line_length <- line_length
}

print(max_line_length)

close(con)

#Question 4
con <- file("en_US.twitter.txt", "r")

love_strings = 0L
hate_strings = 0L

repeat
{
    line <- readLines(con, 1)
    
    if (length(line) < 1)
        break
    
    if (length(grep("love",line,fixed=TRUE)) > 0)
        love_strings <- love_strings + 1
    
    if (length(grep("hate",line,fixed=TRUE)) > 0)
        hate_strings <- hate_strings + 1
}

print(love_strings / hate_strings)

close(con)

#Question 5

con <- file("en_US.twitter.txt", "r")

repeat
{
    line <- readLines(con, 1)
    
    if (length(line) < 1)
        break
    
    if (length(grep("biostat",line,fixed=TRUE)) > 0)
    {
        print(line)
        break
    }
}

close(con)

#Question 6

con <- file("en_US.twitter.txt", "r")

twits_count = 0L

repeat
{
    line <- readLines(con, 1)
    
    if (length(line) < 1)
        break
    
    if (length(grep("A computer once beat me at chess, but it was no match for me at kickboxing",line,fixed=TRUE)) > 0)
        twits_count <- twits_count + 1
}

print(twits_count)

close(con)

