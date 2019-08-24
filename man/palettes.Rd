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

\name{palettes}
\alias{depthmap.classic.colour}
\alias{makeDepthmapClassicColour}
\alias{depthmap.axmanesque.colour}
\alias{makeAxmanesqueColour}
\alias{depthmap.purpleorange.colour}
\alias{makePurpleOrangeColour}
\alias{depthmap.bluered.colour}
\alias{makeBlueRedColour}
\alias{depthmap.grayscale.colour}
\alias{makeGreyScaleColour}
\alias{depthmap.nicehsb.colour}
\alias{makeNiceHSBColour}
\title{
Colour Palettes from depthmapX
}
\description{
Create a single colour or a vector of n contiguous colours taken from depthmapX.
}
\usage{
depthmap.classic.colour(n)
depthmap.axmanesque.colour(n)
depthmap.purpleorange.colour(n)
depthmap.bluered.colour(n)
depthmap.grayscale.colour(n)
depthmap.nicehsb.colour(n)

makeDepthmapClassicColour(value, rangeMin, rangeMax)
makeAxmanesqueColour(value, rangeMin = 0, rangeMax = 1)
makePurpleOrangeColour(value, rangeMin = 0, rangeMax = 1)
makeBlueRedColour(value, rangeMin = 0, rangeMax = 1)
makeGreyScaleColour(value, rangeMin = 0, rangeMax = 1)
makeNiceHSBColour(value, rangeMin = 0, rangeMax = 1)
}
\arguments{
  \item{n}{
    Numer of colours to generate
  }
  \item{value}{
    The number to generate the colour from
  }
  \item{rangeMin}{
    The min value of the range
  }
  \item{rangeMax}{
    The max value of the range
  }
}
\value{
Returns a single colour or a vector of colours.
}
\author{
Petros Koutsolampros
Fani Kostourou
}
