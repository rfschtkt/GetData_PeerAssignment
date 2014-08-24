# download the zip file and unzip it:
# ===================================

zippedUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zippedPath <- "UCI HAR Dataset.zip" # expand %20 to space
if (!file.exists(zippedPath)) download.file(zippedUrl, zippedPath, method="curl")

unzippedPath <- "UCI HAR Dataset" # a posteriori
if (!file.exists(unzippedPath)) unzip(zippedPath)

# common tables:
# ==============

# activity_labels links names to numbers in y, so we'll substitute those below
# features gives names to columns in X, so we'll use them later to put those names in the table (sort of)

activity_labels <- read.table(paste(unzippedPath, "activity_labels.txt", sep = "/"))
features        <- read.table(paste(unzippedPath,        "features.txt", sep = "/"))

# tables to be merged:
# ====================

# Merged means between training data and testing data,
# because that happens to be the assignment.

# Subject is a person, only known by number.

subject <- rbind(
  read.table(paste(unzippedPath, "train/subject_train.txt", sep = "/")),
  read.table(paste(unzippedPath,  "test/subject_test.txt" , sep = "/"))
)

# X is the features data.

X <- rbind(
  read.table(paste(unzippedPath, "train/X_train.txt", sep = "/")),
  read.table(paste(unzippedPath,  "test/X_test.txt" , sep = "/"))
)

# y is the activity, a index into activity_labels.

y <- rbind(
  read.table(paste(unzippedPath, "train/y_train.txt", sep = "/")),
  read.table(paste(unzippedPath,  "test/y_test.txt" , sep = "/"))
)

# tables to be merged but ignored for this assignment (each 10299 observations of 128 variables):
# ===============================================================================================

# They can be ignored because we're going to do a projection to mean and std columns anyway.
# If they're not part of the question, there's no use inserting them only to remove them later on.

# body_acc_x <- rbind(
#   read.table(paste(unzippedPath, "train/Inertial Signals/body_acc_x_train.txt", sep = "/")),
#   read.table(paste(unzippedPath,  "test/Inertial Signals/body_acc_x_test.txt" , sep = "/"))
# )
# body_acc_y <- rbind(
#   read.table(paste(unzippedPath, "train/Inertial Signals/body_acc_y_train.txt", sep = "/")),
#   read.table(paste(unzippedPath,  "test/Inertial Signals/body_acc_y_test.txt" , sep = "/"))
# )
# body_acc_z <- rbind(
#   read.table(paste(unzippedPath, "train/Inertial Signals/body_acc_z_train.txt", sep = "/")),
#   read.table(paste(unzippedPath,  "test/Inertial Signals/body_acc_z_test.txt" , sep = "/"))
# )
# 
# body_gyro_x <- rbind(
#   read.table(paste(unzippedPath, "train/Inertial Signals/body_gyro_x_train.txt", sep = "/")),
#   read.table(paste(unzippedPath,  "test/Inertial Signals/body_gyro_x_test.txt" , sep = "/"))
# )
# body_gyro_y <- rbind(
#   read.table(paste(unzippedPath, "train/Inertial Signals/body_gyro_y_train.txt", sep = "/")),
#   read.table(paste(unzippedPath,  "test/Inertial Signals/body_gyro_y_test.txt" , sep = "/"))
# )
# body_gyro_z <- rbind(
#   read.table(paste(unzippedPath, "train/Inertial Signals/body_gyro_z_train.txt", sep = "/")),
#   read.table(paste(unzippedPath,  "test/Inertial Signals/body_gyro_z_test.txt" , sep = "/"))
# )
# 
# total_acc_x <- rbind(
#   read.table(paste(unzippedPath, "train/Inertial Signals/total_acc_x_train.txt", sep = "/")),
#   read.table(paste(unzippedPath,  "test/Inertial Signals/total_acc_x_test.txt" , sep = "/"))
# )
# total_acc_y <- rbind(
#   read.table(paste(unzippedPath, "train/Inertial Signals/total_acc_y_train.txt", sep = "/")),
#   read.table(paste(unzippedPath,  "test/Inertial Signals/total_acc_y_test.txt" , sep = "/"))
# )
# total_acc_z <- rbind(
#   read.table(paste(unzippedPath, "train/Inertial Signals/total_acc_z_train.txt", sep = "/")),
#   read.table(paste(unzippedPath,  "test/Inertial Signals/total_acc_z_test.txt" , sep = "/"))
# )

# So what we have now is 10299 observations in X, with its columns described by rows in "features".

# For each observation, y specifies the activity (described by activity_labels),
# so maybe that can be inserted directly as a level in y to eliminate activity_labels?

# There seem to be 30 subjects, but they remain anonymous.

# Data extraction (point 1. was done above):

# "2. Extracts only the measurements on the mean and standard deviation for each measurement."
# "4. Appropriately labels the data set with descriptive names."
selectFeatures <- features[grepl("(mean|std)\\(", features$V2), ] # auxiliary table
selectFeatures$V2 <- tolower(gsub("\\(\\)|-", "", selectFeatures$V2))
selectX <- X[, selectFeatures$V1] # 2
colnames(selectX) <- selectFeatures$V2 # 4

selectX$activity <- y$V1
selectX$subject <- subject$V1

# This is a dirty trick while all the information is still numeric.
# The columns involved in the grouping are also averaged, but will remain the same under that operation.
# Only after this step can proper labels be substituted for activity values.

tidydata <- aggregate(selectX, by = list(selectX$activity, selectX$subject), mean)
tidydata <- tidydata[, 3:ncol(tidydata)] # didn't ask for those extra columns; TODO: seems fragile

# "3. Uses descriptive activity names to name the activities in the data set"
tidydata$activity <- sapply(tidydata$activity, function(x) activity_labels$V2[x])

# Now write the result:
# =====================

write.table(tidydata, "tidydata.txt", row.names = FALSE)
