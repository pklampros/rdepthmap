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

\name{refIDtoIndex}
\alias{refIDtoIndex}
\title{
Ref ID to index
}
\description{
Converts a depthmapX "Ref" ID to the indices (x,y) of a pixel
}
\usage{
refIDtoIndex(refID)
}
\arguments{
  \item{refID}{
    The Ref ID
  }
}
\value{
A pair of indices (x,y)
}
\author{
Petros Koutsolampros
}
\examples{
idx = refIDtoIndex(852645)
# outputs:
#    i   j
# 1 13 677
}
