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

\name{import}
\alias{import}
\alias{importLines}
\title{
Import data into a graph file
}
\description{
Import data into a graph file. importLines specifically imports SpatialLines or SpatialLinesDataFrame objects.
}
\usage{
import(filesToImport, graphFileOut, cliPath = getDefaultCLILocation(), verbose = FALSE)
importLines(linesIn, graphFileOut, cliPath = getDefaultCLILocation(), verbose = FALSE)
}
\arguments{
  \item{filesToImport}{
    A file to import. Currently CSV, TSV and DXF files are accepted
  }
  \item{linesIn}{
    A SpatialLines or SpatialLinesDataFrame object.
  }
  \item{graphFileOut}{
    The output graph file.
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
