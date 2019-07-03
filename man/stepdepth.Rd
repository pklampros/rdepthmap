\name{stepdepth}
\alias{stepdepth}
\title{
  Step depth
}
\description{
Calculates step depth from a specific origin
}
\usage{
stepdepth(graphFileIn, graphFileOut, depthType, fromX, fromY,
          cliPath = getDefaultCLILocation(), verbose = FALSE)
}
\arguments{
  \item{graphFileIn}{
    The input graph file.
  }
  \item{graphFileOut}{
    The output graph file. If ommited will use the input file.
  }
  \item{depthType}{
    The type of depth to measure steps in. Can be "metric", "angular" or "visual".
  }
  \item{fromX}{
    The x-value of the origin point.
  }
  \item{fromY}{
    The x-value of the origin point.
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
