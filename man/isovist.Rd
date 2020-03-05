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

\name{isovist}
\alias{isovist}
\alias{isovist2pts}
\alias{makeIsovists}
\title{
Isovist
}
\description{
Creates an isovist at a specific point.
To create an isovist with two points (one the origin, the other the direction) use isovist2pts.
makeIsovists is a helper function that creates and imports multiple Isovists
}
\usage{
isovist(graphFileIn, graphFileOut, x, y, angle = NA, viewangle = NA,
        cliPath = getDefaultCLILocation(), verbose = FALSE)
isovist2pts(graphFileIn, graphFileOut, x, y, toX, toY, viewangle,
        cliPath = getDefaultCLILocation(), verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graphFileIn}{
    The input graph file.
  }
  \item{graphFileOut}{
    The output graph file. If ommited will use the input file.
  }
  \item{x}{
    The x-value of isovist origin point.
  }
  \item{y}{
    The y-value of isovist origin point.
  }
  \item{angle}{
    The angle defining the view orientation of the isovist (in degrees, for partial isovists).
  }
  \item{toX}{
    The x-value of a second point showing the direction of a targeted isovist.
  }
  \item{toY}{
    The y-value of a second point showing the direction of a targeted isovist.
  }
  \item{viewangle}{
    The angle defining the view range of the isovist (in degrees, for partial isovists).
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
