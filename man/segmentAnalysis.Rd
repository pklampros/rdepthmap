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

\name{segmentAnalysis}
\alias{segmentAnalysis}
\title{
Segment Analysis
}
\description{
Calls the depthmapXcli to carry out Segment Analysis on the currently visible map
}
\usage{
segmentAnalysis(graphFileIn, graphFileOut, analysisType, radii, radiusType,
                tulipBins = NA, weightWithColumn = NA, includeChoice = FALSE,
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
  \item{analysisType}{
    The type of analysis. One of:
    "tulip" (Angular Tulip - Faster),
    "angular" (Angular Full - Slower),
    "topological" or
    "metric"
  }
  \item{radii}{
    One or more radii to limit the analysis at.
  }
  \item{radiusType}{
    The type of radius to limit the analysus at. One of:
    "steps",
    "metric" or
    "angular".
  }
  \item{tulipBins}{
    Number of tulip bins. Can be from 4 to 1024, 1024 approximates full angular.
  }
  \item{weightWithColumn}{
    Perform weighted analysis using this attribute (only for Tulip).
  }
  \item{includeChoice}{
    Set to TRUE to include choice.
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
Fani Kostourou
}
