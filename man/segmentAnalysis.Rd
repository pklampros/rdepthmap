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
}
