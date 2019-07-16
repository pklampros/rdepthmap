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

\name{processPointMap}
\alias{processPointMap}
\alias{processPointMapAndLinks}
\title{
Import a PointMap
}
\description{
The processPointMap* functions are provided to import exported map data back into R
}
\usage{
processPointMap(mapPath, scale = 1, sep = "\t")
processPointMapAndLinks(mapPath, linkPath = NA, scale = 1, sep = "\t")
}
\arguments{
  \item{mapPath}{
    Path to pointmap map CSV
  }
  \item{linkPath}{
    Path to pointmap links CSV
  }
  \item{scale}{
    Multiplier to rescale map after import
  }
  \item{sep}{
    Optional. Separator for import
  }
}
\value{
Returns a SptialPixelsDataFrame
}

\author{
Petros Koutsolampros
}
