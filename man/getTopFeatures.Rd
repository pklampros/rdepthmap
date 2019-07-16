% Copyright 2019 Kimon Krenz
% Copyright 2019 Petros Koutsolampros
%
% This file is part of rdepthmap
%
% rdepthmap is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
%
% rdepthmap is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with rdepthmap  If not, see <https://www.gnu.org/licenses/>.

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
