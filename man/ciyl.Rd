\name{ciyl}
\alias{ciyl}
\title{Optimising models by reducing explanatory variables}
\usage{
ciyl(data,first_part_of_model,second_part_of_model,separation_number_of_column,threshold_point)
}
\description{
ciyl is suitable when dealing with multiple models. This package is useful to eliminate non-significant models from your results. It speeds up the process where you need to make all the adjustments by yourself. The difference between elko and ciyl is ciyl is works with any model you would like. for compatibility, it is compatible with any kind of modelling as long as that modelling is compatible with stepwise AIC.
Warning: it will remove the models if non of their explanatory variables are significant.
}
\examples{
library("MASS")
fhof = "glm("
shof = ", family = poisson(link='log'))"

ciyl(sa,fhof,shof,12,0.05)
}
