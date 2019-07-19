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

\name{makeVGAGraph}
\alias{makeVGAGraph}
\alias{unmakeVGAGraph}
\title{
Make/Unmake the VGA Graph.
}
\description{
Makes or unmakes the graph.
}
\usage{
makeVGAGraph(graphFileIn, graphFileOut = NA, maxVisibility = NA, boundaryGraph = FALSE,
             cliPath = getDefaultCLILocation(), verbose = FALSE)
unmakeVGAGraph(graphFileIn, graphFileOut = NA, removeLinks = FALSE,
               cliPath = getDefaultCLILocation(), verbose = FALSE)
}
\arguments{
  \item{graphFileIn}{
    The input graph file.
  }
  \item{graphFileOut}{
    The output graph file. If ommited will use the input file.
  }
  \item{maxVisibility}{
    Set a maximum visibility distance when making the graph.
  }
  \item{boundaryGraph}{
    Create a boundary graph when making the graph.
  }
  \item{removeLinks}{
    Remove existing links when unmaking the graph.
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
