# HousePrice_Prediction
The objective of the project is to apply different machine learning algorithms to real-world tasks. We will show how to clean data, applying pre-processing, features selection and regression. Given the provided datasets, we would like to understand and predict a house's price based on the provided data.
# Preprocessing techniques applied on the dataset:
We applied different techniques to fill nulls in different columns:                                                                         
1- With Median			2-With Zero			3-With Mode                                                                  
We applied one hot encoding on categorical columns
We found some columns which their values are ordinal so,can be replaced with ranking dictionaries.                                 
We applied Scaling (Normalization).                                          
In column “CentralAir” we convert the value ‘Y’ into 1 and the value ‘N’ into 0                                          
We subtract the values in columns “YearBuilt”, “GarageYrBlt”, “YrSold” and “YearRemodAdd” from the current year to get the number of years "which is important info.
# Features Selection
We calculated the correlation between each feature and “SalePrice” column to see how each feature affects the value of this column and to decide which features are important
![image](https://github.com/mariiamhossam/HousePrice_Prediction/assets/65025096/60ace10b-69f3-4569-8c14-ae2f3890db74)
![image](https://github.com/mariiamhossam/HousePrice_Prediction/assets/65025096/6eb8644a-e3ec-45de-94f0-829edd9895e8)
![image](https://github.com/mariiamhossam/HousePrice_Prediction/assets/65025096/e09f2f92-474a-478f-a114-ffc0907ab4c0)
![image](https://github.com/mariiamhossam/HousePrice_Prediction/assets/65025096/98ae6eac-9d3a-44d2-b4ac-76cff3ed7740)                       
We decided to eliminate the features that have correlation between -0.12 and 0.12 as this gives the highest accuracy (lowest error)
# Experiments and Results
We have tried different models to get the best accuracy:
SVM -> MSE = 0.20832                                                                                                   
Decision Tree -> MSE = 0.44859                                                                       
Linear Regression -> MSE = 0.16123 (The Best)                                                                               
Gradient Boosting Regression -> MSE = 0.23894                                                                                                                  
# Visualization
![image](https://github.com/mariiamhossam/HousePrice_Prediction/assets/65025096/62e00944-1760-4c8b-baa7-736942a0556e)

 
