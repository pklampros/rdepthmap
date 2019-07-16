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

\name{convertMap}
\alias{convertMap}
\title{
Convert Map
}
\description{
Converts the currently visible map in the graph file to another type of map
}
\usage{
convertMap(graphFileIn, graphFileOut, newMapType,
           newMapName = generateRandomCapString(10), removeInputMap = FALSE,
           copyAttributes = FALSE, stubLengthToRemove = NA,
           cliPath = getDefaultCLILocation(), verbose = FALSE)
}
\arguments{
  \item{graphFileIn}{
    The input graph file.
  }
  \item{graphFileOut}{
    The output graph file. If ommited will use the input file.
  }
  \item{newMapType}{
    The new map type. Can be one of:
        "drawing",
        "axial",
        "segment",
        "data" or
        "convex"
  }
  \item{newMapName}{
    The new map name. If no name is given creates one with 10 random characters
  }
  \item{removeInputMap}{
    Set to TRUE to remove the input map
  }
  \item{copyAttributes}{
    Set to TRUE to copy attributes to output map (Only between DATA, AXIAL and SEGMENT)
  }
  \item{stubLengthToRemove}{
    Percent of line length of axial stubs to remove (Only for AXIAL -> SEGMENT)
  }
  \item{cliPath}{
    Optional. Provide the path to a different CLI
  }
  \item{verbose}{
    Optional. Show more information of the process
  }
}
\value{
No value returned, process is between files
}
\author{
Petros Koutsolampros
}
