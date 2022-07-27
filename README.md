# epbc
!!!it's in beta!!!

This package helps you dealing with multiple models with multiple explanatory variables while you create your linear or nonlinear model. This package takes your data and puts them thru stepwise AIC then puts every model thru chisq test and checks significance of each variable. It automatically eliminates variables that resulted below the threshold value. If there were no explanatory variable left of the model that model will be excluded from the results.

Example data:

|                | Explanatory 1  | Explanatory 2 | Explanatory 3 | Model Var 1 | Model Var 2 | Model Var 3 |
|----------------|----------------|---------------|---------------|-------------|-------------|-------------|
| Observation 1  | 1              | 1             | 8             | 2           | 3           | 5           |
| Observation 2  | 5              | 2             | 2             | 0           | 7           | 4           |
| Observation 3  | 3              | 3             | 6             | 1           | 0           | 8           |
| Observation 4  | 3              | 5             | 0             | 3           | 5           | 5           |
| Observation 5  | 9              | 1             | 4             | 7           | 1           | 3           |
| Observation 6  | 0              | 9             | 1             | 0           | 3           | 5           |

Example usage:

`elko(example_data,3,0.05)`
