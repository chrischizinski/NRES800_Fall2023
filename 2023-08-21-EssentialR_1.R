# 2023-08-21
# Chris Chizinski
# Learning some of the basic of R
  
cat <- 1234 # Assign a value to an object using arrow
cat = 1234 # or you can assign using equals

cat <- c(1,2,3,4,5)  # you can assign a vector of vales to an object
Cat <- c(1,2,3,4,5) # case matters in R, Cat is different than cat
dog <- c(5,4,3,2,1)


# logical operators
cat == Cat # logical, TRUE FALSE is equal
cat != Cat # not equal `! =`

cat > dog
cat >= dog
cat <= dog

3 %in% dog # use in when using a vector
dog %in% 3

#classes of numeric data
# numeric vs integer
dog <- c(1.3, 4.5,6.6)
class(dog)
typeof(dog)
as.integer(dog)

# other classes
Inf # Infinity
-Inf # negative infinity
NaN # Not a number
NA # missing data

3 + Inf
3 + NaN
3 + NA

## Characters and strings

"A" # Need to make sure you close the end of all quotes or parentheses,  otherwise you will get a hanging '+' down in the console.  To get rid of it so that you can rerun, click in the console and hit the escape key 2x
'a' 
"aa'" # Doesnt work

animals <- c("dog", "cat", "fish", "bird") # you can also asign vectors of strings to an object

animals <- factor(animals, levels = c("bird", "dog", "fish", "cat")) # factor allows you to assign a numerical level to a character value
animals

as.numeric(animals)
as.character(as.numeric(animals))

attitudes <- c("disagree", "neutral", "agree")
factor(attitudes, levels = c("disagree", "neutral", "agree"), ordered = TRUE) # ordered is a special case where like a factor there is a numerical order but also that there is a greater than or less than relationship in those values

as.factor(attitudes)

## 2-dimensional data
matrix(2, 3, 5) # first value is the number, then rows, then columns
?matrix
help(matrix)

matrix("A", 4, 3)
matrix(c(1,2,3,4,5,6), 2, 3)
matrix(c(1,2,3,4,5,6), 2, 3, byrow = TRUE)

matrix(c(1,2,3,NA,5,6), 2, 3, byrow = TRUE)
matrix(c(1,2,3,"A",5,6), 2, 3, byrow = TRUE) # in matrices, all values must be the same type

# data.frames are also 2 dimensional but allow different types of data by column
data.frame(x = c(1,2,3,4,5),
           y = c(LETTERS[1:5])) -> my_df

my_df # You can use brackets to access the elements in a data.frame or matrix
my_df[3,1] # 3rd row, 1st column  value
my_df[,2] # all rows, second column
my_df[1:5,2] # rows 1 through 5, second column
my_df$y # you can also use $ to access entire columns by name rather than position

colnames(my_df) # column names
rownames(my_df) # rownames
names(my_df) # names

as.data.frame(matrix(c(1,2,3,4,5,6), 2, 3, byrow = TRUE))

# Changing names accessing attributes
names(my_df) <- c("A", "B")
my_df
names(my_df)[2] <- "b"

### multidimensional lists
animal_list <- list(dog, cat, Cat, "bird", "fish", my_df)
animal_list
animal_list[[6]] # access elements of a list using [[ and ]]
