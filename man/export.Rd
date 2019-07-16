% Copyright 2019 Fani Kostourou
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

\name{export}
\alias{export}
\alias{getPointmapData}
\alias{getPointmapLinks}
\alias{getPointmapDataAndLinks}
\alias{getShapeGraph}
\title{
Export data
}
\description{
Exports data from a graph file. The getPointmap* functions are provided for convenience to export to temporary file and import to R directly.
}
\usage{
export(graphFileIn, fileOut, exportType,
       cliPath = getDefaultCLILocation(), verbose = FALSE)

getPointmapData(graphFileIn, scale = 1,
       cliPath = getDefaultCLILocation(), verbose = FALSE)
getPointmapLinks(graphFileIn,
       cliPath = getDefaultCLILocation(), verbose = FALSE)
getPointmapDataAndLinks(graphFileIn, scale = 1,
       cliPath = getDefaultCLILocation(), verbose = FALSE)

getShapeGraph(graphFileIn,
       cliPath = getDefaultCLILocation(), verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graphFileIn}{
    The input .graph file.
  }
  \item{fileOut}{
    The output file.
  }
  \item{exportType}{
    What to export. Available types are:
    "pointmap-data-csv",
    "pointmap-connections-csv",
    "pointmap-links-csv",
    "shapegraph-map-csv",
    "shapegraph-map-mif" or
    "shapegraph-connections-csv".
  }
  \item{scale}{
    Provided for when importing a pointmap directly. Will scale the whole map.
  }
  \item{cliPath}{
    Optional. Provide the path to a different CLI.
  }
  \item{verbose}{
    Optional. Show more information of the process.
  }
}
\value{
No value returned, process is between files.
}

\author{
Petros Koutsolampros
}
