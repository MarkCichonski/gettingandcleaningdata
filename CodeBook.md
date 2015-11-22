CodeBook for Assignment
=============================

Data source
-----------
This dataset is derived from the "Human Activity Recognition Using Smartphones Data Set" which was originally made avaiable here: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Variable Selection 
-----------------
You can refer to the README and features.txt in the origial data to get a full picture of all the factors collected.

This database comes from the accelerometer and gyroscope on the Samsung Galaxy S Phone. 

We were tasked with extracting mean and standard deviation variables only.

The R Code pulls out the following 89 factors.

activity
subject
tbodyaccmeanx
tbodyaccmeany
tbodyaccmeanz
tbodyaccstdx
tbodyaccstdy
tbodyaccstdz
tgravityaccmeanx
tgravityaccmeany
tgravityaccmeanz
tgravityaccstdx
tgravityaccstdy
tgravityaccstdz
tbodyaccjerkmeanx
tbodyaccjerkmeany
tbodyaccjerkmeanz
tbodyaccjerkstdx
tbodyaccjerkstdy
tbodyaccjerkstdz
tbodygyromeanx
tbodygyromeany
tbodygyromeanz
tbodygyrostdx
tbodygyrostdy
tbodygyrostdz
tbodygyrojerkmeanx
tbodygyrojerkmeany
tbodygyrojerkmeanz
tbodygyrojerkstdx
tbodygyrojerkstdy
tbodygyrojerkstdz
tbodyaccmagmean
tbodyaccmagstd
tgravityaccmagmean
tgravityaccmagstd
tbodyaccjerkmagmean
tbodyaccjerkmagstd
tbodygyromagmean
tbodygyromagstd
tbodygyrojerkmagmean
tbodygyrojerkmagstd
fbodyaccmeanx
fbodyaccmeany
fbodyaccmeanz
fbodyaccstdx
fbodyaccstdy
fbodyaccstdz
fbodyaccmeanfreqx
fbodyaccmeanfreqy
fbodyaccmeanfreqz
fbodyaccjerkmeanx
fbodyaccjerkmeany
fbodyaccjerkmeanz
fbodyaccjerkstdx
fbodyaccjerkstdy
fbodyaccjerkstdz
fbodyaccjerkmeanfreqx
fbodyaccjerkmeanfreqy
fbodyaccjerkmeanfreqz
fbodygyromeanx
fbodygyromeany
fbodygyromeanz
fbodygyrostdx
fbodygyrostdy
fbodygyrostdz
fbodygyromeanfreqx
fbodygyromeanfreqy
fbodygyromeanfreqz
fbodyaccmagmean
fbodyaccmagstd
fbodyaccmagmeanfreq
fbodybodyaccjerkmagmean
fbodybodyaccjerkmagstd
fbodybodyaccjerkmagmeanfreq
fbodybodygyromagmean
fbodybodygyromagstd
fbodybodygyromagmeanfreq
fbodybodygyrojerkmagmean
fbodybodygyrojerkmagstd
fbodybodygyrojerkmagmeanfreq
angletbodyaccmean,gravity
angletbodyaccjerkmean,gravitymean
angletbodygyromean,gravitymean
angletbodygyrojerkmean,gravitymean
anglex,gravitymean
angley,gravitymean
anglez,gravitymean

These were performed while the subject was:

LAYING
SITTING
STANDING
WALKING
WALKING_DOWNSTAIRS
WALKING_UPSTAIRS

There were 30 different subjects who did these activities so there are 180 entries in the result table.
