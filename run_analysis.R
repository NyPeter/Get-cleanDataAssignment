## First step
train_data <- read.table("UCI HAR Dataset/train/X_train.txt") ## read the training data
train_labels <- read.table("UCI HAR Dataset/train/y_train.txt", col.names="activity") ## read the training activities
train_subjects <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names="subject") ## read the subjects of training 
train <- cbind(train_subjects, train_labels, train_data) ## merge the training files into one dataset

test_data <- read.table("UCI HAR Dataset/test/X_test.txt") ## read the test data
test_labels <- read.table("UCI HAR Dataset/test/y_test.txt", col.names="activity") ## read the test activities
test_subjects <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names="subject") ## read the subjects of test
test <- cbind(test_subjects, test_labels, test_data) ## merge the test files into one dataset

First_step <- rbind(test,train) ## merge them into one dataframe

## Second step
j <- nrow(First_step)
mean <- data.frame()
sd <- data.frame()
for (i in 1:j) {
    Mean <- mean(as.numeric(First_step[i,3:563]))
    Standard_Deviation <- sd(as.numeric(First_step[i,3:563]))
    mean <- rbind(mean, Mean)
    sd <- rbind(sd, Standard_Deviation)
}
Second_step <- transmute(First_step, subject = subject, activity = activity)
Second_step <- cbind(Second_step, mean, sd)
rm(mean,sd)

## Third step
for (i in 1:j) {
    y <- as.numeric(Second_step[i,2])
    if (y == 1) {
        Second_step[i,2] <- c("Walking")
    }
    else if (y == 2) {
        Second_step[i,2] <- c("Walking_upstairs")
    }
    else if (y == 3) {
        Second_step[i,2] <- c("Walking_downstairs")
    }
    else if (y == 4) {
        Second_step[i,2] <- c("Sitting")
    }
    else if (y == 5) {
        Second_step[i,2] <- c("Standing")
    }
    else if (y == 6) {
        Second_step[i,2] <- c("Laying")
    }
    else {
        Second_step[i,2] <- c("Error!")
    }
}

## Forth step
Forth_step <- rename(Second_step, mean=X.0.586932495926453, standard_deviation=X0.53293889474142)

## Fifth step
dataframe2<-Forth_step
dataframe2$groups<-paste(dataframe2$subject,dataframe2$activity, sep="-")
f<-character()
c<-numeric()
d<-numeric()
for(i in unique(dataframe2$groups)){
    a<-mean(as.numeric(dataframe2[dataframe2$groups==i,3]))
    b<-mean(as.numeric(dataframe2[dataframe2$groups==i,4]))
    c<-append(c,a)
    d<-append(d,b)
    f<-append(f,i)
}
dataframe2<-cbind(f,c,d)
dataframe<-rename(dataframe2, groups=f, mean=c, sd=d)