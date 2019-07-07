\name{getTopFeatures}
\alias{getTopFeatures}
\title{
Extract top x percent of features
}
\description{
Sorts features by a specific column and extracts the top x percent
}
\usage{
getTopFeatures(spatialDataFrame, column, percent)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spatialDataFrame}{
    The spatial data frame to export the features from
  }
  \item{column}{
    The column to use to extract the features from
  }
  \item{percent}{
    Percentage of features (to total) to extract
  }
}
\value{
Returns a new spatial data frame with the top x features.
}
\author{
Kimon Krenz
Petros Koutsolampros
}
