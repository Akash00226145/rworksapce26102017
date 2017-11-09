wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

str(wbcd)
# drop the id feature
wbcd <- wbcd[-1]
# table of diagnosis
table(wbcd$diagnosis)
# recode diagnosis as a factor
wbcd$diagnosis <- factor(wbcd$diagnosis,
                         levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

# table or proportions with more informative labels
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)
# summarize three numeric features
summary(wbcd[c("radius_mean", "area_mean",
               "smoothness_mean")])
# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# test normalization function - result should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

# normalize the wbcd data
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
# confirm that normalization worked
summary(wbcd_n$area_mean)
# create training and test data
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
# create labels for training and test data
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

## Step 3: Training a model on the data ----
library(class)
wbcd_test_pred <- knn(train = wbcd_train, test =
                        wbcd_test, cl = wbcd_train_labels, k=21)
## Step 4: Evaluating model performance ----
# load the "gmodels" library
install.packages("gmodels")
library(gmodels)
# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)




#################################################################
### Q2
#Apply kNN to the iris data as follows.
#. Randomize the order of the data items.
#. Scale the numeric properties using z-scaling.
#. split the data into 100 training items and 50 test items.
#. find the kNN predictions for the test items.
#. print out a confusion matrix for the actual and predicted species. 
#################################################################
library(gmodels)
library(class)


data <- iris

set.seed(1)
irisRand <- data[order(runif(150)), ]

data_z <- as.data.frame(scale(irisRand[-5]))

iris_train <- data_z[1:100, ]
iris_test  <- data_z[101:150, ]

# create labels for training and test data

iris_train_species <- irisRand[1:100, 5]
iris_test_species <- irisRand[101:150, 5]

# load the "class" library
library(class)


iris_test_pred <- knn(train = iris_train, test = iris_test, cl = iris_train_species, k=3 , prob=TRUE)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = iris_test_species, y = iris_test_pred, prop.chisq=FALSE)



######################################################################
# Lab 7
#Q1
x= seq(from = -5, to = 5, by = 0.1)
y = 1/(1+exp(-x))
plot(x,y, type="l")

#Q2 




