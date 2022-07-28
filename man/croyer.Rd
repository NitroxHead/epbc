\name{croyer}
\alias{croyer}
\title{Optimising models by reducing explanatory variables}
\usage{
croyer(data,first_part_of_model,second_part_of_model,separation_number_of_column,threshold_point,table_of_explanatory,table_after_test_or_not,requested_test)
}
\description{
croyer is suitable when dealing with multiple models. This package is useful to eliminate non-significant models from your results. It speeds up the process where you need to make all the adjustments by yourself. The difference between elko and croyer is croyer is works with any model you would like. for compatibility, it is compatible with any kind of modelling as long as that modelling is compatible with stepwise AIC.
Unlike ciyl it gives you option to create output where it creates table of the explanatory variables. That table can also show the significance of explanatory variable using desired test such as "Chisq". That table can be seen at [[2]] of saved variable. The same output as ciyl can be seen by [[1]]
Warning: it will remove the models if non of their explanatory variables are significant.
}
\examples{
library("MASS")
fhof = "glm("
shof = ", family = poisson(link='log'))"

variable1 = croyer(sa,fhof,shof,12,0.05,1,1,"Chisq")
}
