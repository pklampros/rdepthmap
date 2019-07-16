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

\name{matchPointsToLines}
\alias{matchPointsToLines}
\title{
Match points to their closest line
}
\description{
Matches (spatial-join) points to lines. Finds the point closest to a line. One point is attached to one line, thus if fewer points than lines are given then some lines will have no point attached.
}
\usage{
matchPointsToLines(points, lines, getIndex = FALSE)
}
\arguments{
  \item{points}{
    Points to attach.
  }
  \item{lines}{
    Lines to attach to.
  }
  \item{getIndex}{
    Get the index returned and not the data.
  }
}
\value{
If getIndex is TRUE then the index of the points as they relate to the matching lines are given. If not, then the data from the points dataframe is returned.
}
\author{
Petros Koutsolampros
}
