\name{VGA}
\alias{VGA}
\title{
Visibility Graph Analysis
}
\description{
Calls the depthmapXcli to carry out Visibility Graph Analysis on the currently visible map
}
\usage{
VGA(graphFileIn, graphFileOut, vgaMode, radii,
    cliPath = getDefaultCLILocation(), verbose = FALSE)
}
\arguments{
  \item{graphFileIn}{
    The input graph file.
  }
  \item{graphFileOut}{
    The output graph file. If ommited will use the input file.
  }
  \item{vgaMode}{
    The VGA mode. One of:
    "isovist",
    "visiblity-global",
    "visiblity-local",
    "metric",
    "angular" or
    "thruvision"
  }
  \item{radii}{
    One or more radii to limit the analysis at.
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
