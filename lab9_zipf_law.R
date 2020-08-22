# Program reads text from a file to generate a log-log plot of word count versus rank and print the 10 most frequent words
#
# CS150 Fall 2019 Lab 9
#
# Name: Aditya Jain
# Section: A
#
# Creativity:
# ggplot_display() uses library ggplot for the log-log plot

# Import required R packages (suppressing conflict messages)
library(plyr, warn.conflicts=FALSE)
library(stringr, warn.conflicts=FALSE)
library(ggplot2, warn.conflicts=FALSE)


# Split strings on word boundaries, removing any punctuation
# Args:
#   strings: A vector of strings
# Returns a vector of strings (the words)
split_and_strip <- function(strings) {
  # str_subset removes any empty strings, or strings that contain only whitespace.
  # \S is the character class for non white-space characters
  unlist(str_split(str_subset(strings, "\\S+"), boundary("word")))
}

# Read file into a vector of cleaned and normalized words
# Args:
#   filename: Filename to analyze as a string
# Returns a vector of cleaned and normalized words
file_to_words <- function(filename) {
  split_and_strip(tolower(readLines(filename, warn = FALSE)))
}


# Create a ranked data frame of words and their counts
# Args:
#   words: Vector of cleaned words
# Returns a data.frame of words and counts in descending order of count
count_and_rank <- function(words) {
  frame <- plyr::count(words)
  colnames(frame) <- c("Word", "Count")
  frame[order(frame$Count, decreasing = TRUE),]
}

# Prompt the user for a file name and construct ranks data.frame
filename <- readline(prompt="Enter a filename: ")
words <- file_to_words(filename)
counts <- count_and_rank(words)
ranks <- data.frame(
  counts,
  "Rank" = 1:nrow(counts)
)

# Using library ggplot to construct log-log plot of count vs rank
# Returns a log-log plot
ggplot_display <- function(){
  ggplot(ranks, aes(x=Rank, y=Count)) + 
    geom_point(shape = 1, colour = "blue", fill = "white", size = 1) + 
    scale_x_log10() +
    scale_y_log10()+
    labs(x= "Rank", y ="Count", title = "Log-log plot of count vs. rank of words in text corpus")
}

plot_2 <- ggplot_display() # store value returned from ggplot_display()

# Print 10 most common words and generate a log-log plot count vs. rank
print(ranks[1:10,1:2], row.names = FALSE)
plot(ranks$Rank, ranks$Count, type = "p", log = "xy", xlab="Rank", ylab="Count", 
     main = "Log-log plot of count vs. rank of words in text corpus")
print(plot_2)