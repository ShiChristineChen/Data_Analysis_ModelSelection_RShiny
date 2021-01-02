# Data_Analysis_ModelSelection_RShiny
This project is focus on the model selection part based on MSE evaluation. 
## Purpose
1. Using the supplied dataset (in the test folder) and Shiny App to find the best performing regression model (models are from caret package).
2. To know the process and what to do in different situation.

## Data

In this dataset, there are 1280 observations and 21 variables. The target variable is “Y”, which is numeric. 

Other 20 variables can be selected as predictors. 18 of them are numeric and 2 of them are nominal.  One of the nominal variables is “BloodType”, which has 4 types. The other is “TreatmentDate”, which has been transformed as “date” type in the pre-processing pipeline. 

It should be pay attention, when using numeric method to do the prediction, these two nominal variables should be transformed to numeric in pre-processing part. This will explained in more detail in the strategy part. 

By comparing the “ID” and observations, there are no duplicate observations and similar observations. The resample methods like K-fold cross validation and Boot. In this case, using “boot” as re-sampling method.

In this case, by default “IQR multiplier” = 1.5, there are 4 variables (“Alcohol”, “Coffee”, “exercise”, “MDocVisits”) do not have outliers. Other variables’ outliers are obvious. However, most of them will be removed when increasing “IQR multiplier” up to 2.1. But the outliers of target variable “Y” are extremely significant and in a large amount. Even increasing the “IQR multiplier” to 2.6, a bunch of outliers still cannot be removed. It suggests that the method which robust outliers should be preferred. 

As to missing values, there are no excessively missing variables. The target variable doesn’t having missing value. So there is no need to delete observations. The two nominal variables also has no missing values. So imputation can come before dummy. On the other hand, there are 320 observations (25%) contain missing values. It is more sensible to impute the missing value rather than remove them directly. 

From correlation matrix, there are four groups of variables are in strong linear co-relationship. Except one of these groups show slightly negative linear relation with target “Y”, other variables show non-linear relationship with “Y”. It suggest that non-linear regression methods may perform better than the linear regression methods.


## Process
1. Random start from different methods group, find the relatively good model and study the pattern shared in these models
2. Preprocessing recipe pipeline to deal with missing values, nominal variables, resampling.
3. Model selection
4. Model tuning
5. Report

More detial please refer to the report.

## Output
1. Report
2. Visualisation Link: https://sch405.shinyapps.io/ModelSelection/
(It may not open properly for the SavedModels are not added successfully in the server of this path.)


Notice:
RShiny is a very useful tool during the whole process! But pay attention to the library installation and compution resources! I came to some challenges when doing these project. The best way to solve it is to check the library and sometimes you need to install a high level package first, just follow the error message. 

Another suggestion is that put the data (.csv file in the test folder) and SavedModels under the same directory of the codes after you download them and go have a try. It will save your time to retrain the models, some model will take a very long time and a lot of memory by using cross-validation to train.  Please do the same thing with other similar projects. 

Author: Shi Chen
E-mail: yuejianyingmm@icloud.com
