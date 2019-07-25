# Multilevel Bayesian Model for U.S. Labor Force Participation Rate

### Research Background: 
Labor force participation rate, defined as the inverse of the unemployment rate, is the percentage of working people in a group of people who are currently available for work. Rising labor force participation rate is seen as a sign of a strong economy, with fast growth and great spending. The determination of the health of the economy also helps to set monetary policy. 

### Objective & Data Source:
To gain more insights about U.S labor force participation rate, our group looks for the U.S. Current Population Survey (https://cps.ipums.org/cps/) from 1976 to 2015 providing individual-level data on geographic and demographic characteristics, including sex, race, age, skill, annual income, state, year, and labor market outcomes.  

### Methodology: 

Adopting Multilevel Logistic Regression Model to predict whether a U.S. citizen participates in the job market or not is based on 7 variables, including sex, race, age, skill, annual income, state, and year, which could reach above 70% accuracy.  

To analyze more about the effect of interactions among variables, we adopt Varying Intercept and Slope Model.

### Limitation & Future Improvement:
Our grouping of regions into 4 parts is too board, which captures less information. And there is little correlation between state and income variable.  

Therefore, considering more about the grouping of variables and the relationships among them might better improve our model to predict the U.S. labor force participation rate. 
 
### Tools:
This project will use R version 3.5.0 and Stan version 2.17.3. 
