# You should create one R script called run_analysis.R that does the following. 

# 1. Merges the training and the test sets to create one data set.

xTest = read.table("./gdProject/UCI HAR Dataset/test/X_test.txt") # read in all datasets
xTrain = read.table("./gdProject/UCI HAR Dataset/train/X_train.txt")
yTest = read.table("./gdProject/UCI HAR Dataset/test/Y_test.txt")
yTrain = read.table("./gdProject/UCI HAR Dataset/train/Y_train.txt")
subTest = read.table("./gdProject/UCI HAR Dataset/test/subject_test.txt")
subTrain = read.table("./gdProject/UCI HAR Dataset/train/subject_train.txt")

test = cbind(subTest, yTest, xTest) # combine test data
test$type = "Test" # label
train = cbind(subTrain, yTrain, xTrain) # combine train data
train$type = "Train" # label

data = rbind(test, train) # combine test and train data
names(data)[1] = "subjectID" # name variables
names(data)[2] = "activity"

adminData = data[,c(1,2,length(data))] # create administrative dataset
outputData = data[,c(4:length(data)-1)] # create result dataset

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

features = read.table("./gdProject/UCI HAR Dataset/features.txt")[2] # read in variable names
matchTerms = c("mean()", "std()") # set terms to search variables for
matches = grep(paste(matchTerms,collapse="|"), features[,1]) # returns column IDs with matched variables
filteredData = outputData[,matches] # create dataset with mean/std measurements

# 3. Uses descriptive activity names to name the activities in the data set

activity = read.table("./gdProject/UCI HAR Dataset/activity_labels.txt") # read in activity labels
adminData[,2] = activity[adminData[,2], 2] # replace activity code with acticity names

# 4. Appropriately labels the data set with descriptive variable names. 

names(filteredData) = features[matches, 1] # add descriptive variable names
cleanData = data.frame(adminData, filteredData) # combine to make clean dataset

# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

averageData = data.frame(aggregate(cleanData[,3] ~ activity + subjectID, cleanData, mean))[1:2] # make dataframe with ID variables

for (i in 4:length(cleanData)) { # loop through remaining variables and return means
        averageData = cbind(averageData, data.frame(aggregate(cleanData[,i] ~ activity + subjectID, cleanData, mean))[3])
}

names(averageData)[3:length(averageData)] = names(filteredData) # replace default names with clean variable names
write.table(averageData, "tidy_average_data.txt") # write tidy data file based on average data

