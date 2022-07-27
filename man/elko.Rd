\name{elko}
\alias{elko}
\title{Optimising models by reducing explanatory variables}
\usage{
elko(data,separation_number_of_column,threshold_point)
}
\description{
elko is suitable when dealing with multiple models. This package is usefull to eleminate non significant models from your results. It speedsup proccess where you need to make all the adjustents by yourself.
Warning: it will remove the models if non of their explanatory variables are significant.
}
\examples{
elko(df,12,0.05)
}
