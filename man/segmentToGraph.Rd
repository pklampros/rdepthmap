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

\name{segmentToGraph}
\alias{segmentToGraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Segment Map to Weighted Graph conversion
}
\description{
Creates igraph based on the connections and the x,y coordinates of the centroids of segment map. Assigns weight to the graph edges based on a variable.
}
\usage{
segmentToGraph(graphFile, weightcolumn = "Segment Length")
}
\arguments{
  \item{graphFile}{
    The input graph file.
  }
  \item{weightcolumn}{
    The variable used to assign weight to graph edges. Defaults to Segment Length.
  }
}
\details{
Edge connections weight is calculated by taking the average of the variable of the connected nodes.
}
\value{
  Returns graph.data.frame.
}
\author{
Fani Kostourou
}
