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

\name{axialToGraph}
\alias{axialToGraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Axial Map to Graph conversion
}
\description{
Creates igraph based on the connections and the x,y coordinates of the centroids of axial map.
}
\usage{
axialToGraph(graphFile)
}
\arguments{
  \item{graphFile}{
    The input graph file.
  }
}
\value{
  Returns graph.data.frame.
}
\author{
Fani Kostourou
}
