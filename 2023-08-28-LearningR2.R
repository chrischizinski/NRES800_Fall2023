# Chris Chizinski
# August 28, 2023
# Learning more R, the basics


# packages ----------------------------------------------------------------
install.packages('palmerpenguins') # install packages


# penguins data -----------------------------------------------------------
library(palmerpenguins)
?palmerpenguins # get help on the dataset

data(penguins)
ls()
# Ways to "look" at the data
View(penguins)
head(penguins) # first six rows
head(penguins, 10)
tail(penguins)

penguins
penguins[1:6, ] # display first six rows, all columns using brackets
penguins[c(1:6,10:12),] # display first six rows and rows 10:12, all columns using brackets
penguins[, 1:6]

penguins[,3] # all rows, third column
penguins$bill_length_mm # reference by name using $
penguins[, "bill_length_mm"] # or we can use brackets and the column header
names(penguins) # column names
dim(penguins) # dimensions of the data , rows by columns
nrow(penguins) # number of rows
ncol(penguins) # number of columns

## Little challenge
# Calculate the flipper length to body mass ratio for each penguin
penguins[,"flipper_length_mm"]/penguins[,"body_mass_g"]
penguins$flipper_length_mm / penguins$body_mass_g

## Challenge 2
# Calculate the flipper length to body mass ratio for only the Adelie species
head(penguins)
penguins$flipper_length_mm[penguins$species == "Adelie"] / penguins$body_mass_g[penguins$species == "Adelie"]

## Challenge 3
# Calculate the flipper length to body mass ratio for only the female Adelie species
penguins$flipper_length_mm[penguins$species == "Adelie" & penguins$sex == "female"] / penguins$body_mass_g[penguins$species == "Adelie" & penguins$sex == "female"]

## Challenge 4
# calculate the average and standard deviation flipper length to body mass ratio for female Adelie species, without NA
flipper_body_ratio <- penguins$flipper_length_mm[penguins$species == "Adelie" & penguins$sex == "female"] / penguins$body_mass_g[penguins$species == "Adelie" & penguins$sex == "female"]

flipper_body_ratio

mean(flipper_body_ratio, na.rm = T)
sd(flipper_body_ratio, na.rm = T)
sd(flipper_body_ratio, na.rm = T)/sqrt(length(flipper_body_ratio))

adelie_female_flipper <-  penguins$flipper_length_mm[penguins$species == "Adelie" & penguins$sex == "female"]
adelie_female_mass <- penguins$body_mass_g[penguins$species == "Adelie" & penguins$sex == "female"]

flip_mass <- cbind(adelie_female_flipper, adelie_female_mass) # bind the two vectors together
ind <- which(is.na(flip_mass), arr.ind = TRUE) # use arr.ind = T to get the rows and columns every where there is an NA
uniq_ind <- unique(ind[,1]) # find the unique values of rows, in order to complete rowwise deletion of items with NA
mean(adelie_female_flipper[-ind] / adelie_female_mass[-ind]) # use the minus symbol to remove the rows that contain NAs


mean(na.omit(flipper_body_ratio)) # you can also use NA omit to do the same thing we did above


# Coding styles -----------------------------------------------------------

# Advanced R Style Guide
# variables and functions should be lowercase
# use an _ to separate words within a name
# variable names should be nouns
# functions should be verbs
# strive for names that are concise and meaningful (this is not easy!)
# do NOT create objects that are also function names 😕
# cannot lead with a number
# emphasizes using the <- over the = for assign objects
unique(penguins$species)
penguins$body_mass_g[penguins$species == "Adelie" | penguins$species == "Chinstrap"]

penguins$body_mass_g[penguins$species %in% c("Adelie", "Chinstrap")]

?mean

# reprex ------------------------------------------------------------------
#mac cmd-shift-R
# windows cntrl-shift-R

# install.packages(c('reprex','praise'))
# install.packages('styler')

? reprex::reprex()

# NOTE:  This is where we went wrong in the lecture.  Highlight rows 99 through 104 and hit copy (cntrl (PC) or Cmd (Mac) + C)
library(praise)

template <- "${EXCLAMATION} your reprex is ${adjective}!"

praise(template)

## Then run the reprex code below
our_reprex_example <- reprex::reprex()

## The output is below

# ``` r
# library(praise)
# 
# template <- "${EXCLAMATION} your reprex is ${adjective}!"
# 
# praise(template)
# #> [1] "OH your reprex is breathtaking!"
# ```
# 
# <sup>Created on 2023-08-29 with [reprex v2.0.2](https://reprex.tidyverse.org)</sup>

# This code can be pasted into a site like stack overflow to have a nicely formatted example that makes it easy to share
# a reproducible example.  You can see a lot more detail about reproducible examples and reprex here: https://www.youtube.com/watch?v=5gqksthQ0cM
  