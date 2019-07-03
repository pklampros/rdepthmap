\name{isovist}
\alias{isovist}
\alias{makeIsovists}
\title{
Isovist
}
\description{
Creates an isovist at a specific point.
makeIsovists is a helper function that creates and imports multiple Isovists
}
\usage{
isovist(graphFileIn, graphFileOut, x, y, angle = NA, viewangle = NA,
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
  \item{x}{
    The x-value of isovist origin point.
  }
  \item{y}{
    The y-value of isovist origin point.
  }
  \item{angle}{
    The angle defining the view orientation of the isovist (in partial isovists).
  }
  \item{viewangle}{
    The angle defining the view range of the isovist (in partial isovists).
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
