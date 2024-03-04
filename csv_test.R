#Replace variable (with corresponding columnar data)
#Replace data_set_name (with file_name), data_set_name2 (delineates new instance)
#Variable corresponds to columnar data
#Spacing, structure, and order matter

# Load csv file, header (TRUE/FALSE), sep == delimiter
data_set_name <- read.csv(file='file.csv', header=TRUE, sep",") 

# Converting appropriate variables to factors
data_set_name2 <- within(file_name, {
  variable <- factor(variable)
  variable <- factor(variable)
  variable <- factor(variable)
  variable <- factor(variable)
  variable <- factor(variable) #etc as many as needed
})

#Variable and thier type
print("Variables")
sapply(data_set_name2, class)

#Print the data set
print("dataset")
data_set_name2

#Print x rows; X = number of rows disired
print("head")
head(data_set_name2, X)