The Experimental Design:
======================================
NOTE: As described in raw dataset.

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details.

Preprocessing
=================
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals. These time domain signals were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (TemporalBodyAccelerationInXYZ and TemporalGravityAcceleration-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz.

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (TemporalBodyJerkInXYZ and TemporalBodyAngularAccelerationInXYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (TemporalBodyAccelerationMagnitude, TemporalGravityAccelerationMagnitude, TemporalBodyJerkMagnitude, TemporalBodyAngularVelocityMagitude, TemporalBodyAngularAccelerationMagitude).

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing FrequencyBodyAccelerationInXYZ, FrequencyBodyJerkInXYZ, FrequencyBodyAngularVelocityInXYZ, FrequencyBodyJerkMagnitude, FrequencyBodyAngularVelocityMagnitude, FrequencyBodyAngularAccelerationMagnitude.

These signals were used to estimate variables of the feature vector for each pattern:
'InXYZ' is used to denote 3-axial signals in the X, Y and Z directions.

Features are normalized and bounded within [-1,1].

Features
==================
The Dataset contains the following features.

 [1] "ActivityLabels" - 6 possible activities carried out in the experiment
 [2] "Subjects"- 30 anonymous subjects

 Mean and Standard Deviation of Body Acceleration in X, Y and Z.

 [3] "MeanOfTemporalBodyAccelerationInX"\\
 [4] "MeanOfTemporalBodyAccelerationInY"
 [5] "MeanOfTemporalBodyAccelerationInZ"
 [6] "StandardDeviationOfTemporalBodyAccelerationInX"
 [7] "StandardDeviationOfTemporalBodyAccelerationInY"
 [8] "StandardDeviationOfTemporalBodyAccelerationInZ"

Mean and Standard Deviation of Gravitational Acceleration in X, Y and Z.

 [9] "MeanOfTemporalGravityAccelerationInX"
[10] "MeanOfTemporalGravityAccelerationInY"
[11] "MeanOfTemporalGravityAccelerationInZ"
[12] "StandardDeviationOfTemporalGravityAccelerationInX"
[13] "StandardDeviationOfTemporalGravityAccelerationInY"
[14] "StandardDeviationOfTemporalGravityAccelerationInZ"

Mean and Standard Deviation of Body Jerk in X, Y and Z.

Note: Acceleration Jerk makes no sense physically.
[15] "MeanOfTemporalBodyJerkInX"
[16] "MeanOfTemporalBodyJerkInY"
[17] "MeanOfTemporalBodyJerkInZ"
[18] "StandardDeviationOfTemporalBodyJerkInX"
[19] "StandardDeviationOfTemporalBodyJerkInY"
[20] "StandardDeviationOfTemporalBodyJerkInZ"

Mean and Standard Deviation of Angular Velocity in X, Y and Z.
Note: Gyroscope is the instrument to measure angular velocity not the physical measurement.

[21] "MeanOfTemporalBodyAngularVelocityInX"
[22] "MeanOfTemporalBodyAngularVelocityInY"
[23] "MeanOfTemporalBodyAngularVelocityInZ"
[24] "StandardDeviationOfTemporalBodyAngularVelocityInX"
[25] "StandardDeviationOfTemporalBodyAngularVelocityInX"
[26] "StandardDeviationOfTemporalBodyAngularVelocityInZ"

Mean and Standard Deviation of Angular Acceleration in X, Y and Z.

[27] "MeanOfTemporalBodyAngularAccelerationInX"
[28] "MeanOfTemporalBodyAngularAccelerationInY"
[29] "MeanOfTemporalBodyAngularAccelerationInZ"
[30] "StandardDeviationOfTemporalBodyAngularAccelerationInX"
[31] "StandardDeviationOfTemporalBodyAngularAccelerationInY"
[32] "StandardDeviationOfTemporalBodyAngularAccelerationInZ"

Mean and Standard Deviations of magnitudes of all the previous measurements. In this case the Euclidean norm is use on the 3D parameters.

[33] "MeanOfTemporalBodyAccelerationMagnitude"
[34] "StandardDeviationOfTemporalBodyAccelerationMagnitude"
[35] "MeanOfTemporalGravityAccelerationMagnitude"
[36] "StandardDeviationOfTemporalGravityAccelerationMagnitude"
[37] "MeanOfTemporalBodyJerkMagnitude"
[38] "StandardDeviationOfTemporalBodyJerkMagnitude"
[39] "MeanOfTemporalBodyAngularVelocityMagnitude"
[40] "StandardDevationOfTemporalBodyAngularVelocityMagnitude"
[41] "MeanOfTemporalBodyAngularVelocityMagnitude"
[42] "StandardDevationOfTemporalBodyAngularVelocityMagnitude"

Mean and Standard Deviation of Body Acceleration in frequency space for X, Y and Z.

[43] "MeanOfFrequencyBodyAccelerationInX"
[44] "MeanOfFrequencyBodyAccelerationInY"
[45] "MeanOfFrequencyBodyAccelerationInZ"
[46] "StandardDeviationOfFrequencyBodyAccelerationInX"
[47] "StandardDeviationOfFrequencyBodyAccelerationInY"
[48] "StandardDeviationOfFrequencyBodyAccelerationInZ"

Mean and Standard Deviation of Body Jerk in frequency space for X, Y and Z.

[49] "MeanOfFrequencyBodyJerkInX"
[50] "MeanOfFrequencyBodyJerkInY"
[51] "MeanOfFrequencyBodyJerkInZ"
[52] "StandardDeviationOfFrequencyBodyJerkInX"
[53] "StandardDeviationOfFrequencyBodyJerkInY"
[54] "StandardDeviationOfFrequencyBodyJerkInZ"

Mean and Standard Deviation of Angular Velocity in frequency space for X, Y and Z.

[55] "MeanOfFrequencyBodyAngularVelocityInX"
[56] "MeanOfFrequencyBodyAngularVelocityInX"
[57] "MeanOfFrequencyBodyAngularVelocityInX"
[58] "StandardDeviationOfFrequencyBodyAngularVelocityInX"
[59] "StandardDeviationOfFrequencyBodyAngularVelocityInY"
[60] "StandardDeviationOfFrequencyBodyAngularVelocityInZ"

Mean and Standard Deviations of magnitudes of all the previous measurements. In this case the Euclidean norm is use on the 3D parameters.

[61] "MeanOfFrequencyBodyAccelerationMagnitude"
[62] "StandardDeviationOfFrequencyBodyAccelerationMagnitude"
[63] "MeanOfFrequencyBodyJerkMagnitude"
[64] "StandardDeviationOfFrequencyBodyJerkMagnitude"
[65] "MeanOfFrequencyBodyAngularVelocityMagnitude"
[66] "StandardDevationOfFrequencyBodyAngularVelocityMagnitude"
[67] "MeanOfTemporalBodyAngularVelocityMagnitude"
[68] "StandardDevationOfTemporalBodyAngularVelocityMagnitude"
