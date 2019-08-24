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

\name{shapegraphToIGraph}
\alias{shapegraphToIGraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Conversion of shapegraph to igraph
}
\description{
Creates igraph based on the connections and the x,y coordinates of the centroids of shapes in a shapegraph (axial,segment, convex). Specify weightcolumn to assign weight to graph edges.
}
\usage{
shapegraphToIGraph(graphFile, weightcolumn = NA)
}
\arguments{
  \item{graphFile}{
    The input graph file.
  }
  \item{weightcolumn}{
    Optional.The variable used to assign weight to graph edges
  }
}
\details{
If weightcolumn is provided, edge connections weight is calculated by taking the average of the variable of the connected nodes.
}
\value{
  Returns graph.data.frame.
}
\author{
Fani Kostourou
Petros Koutsolampros
}
