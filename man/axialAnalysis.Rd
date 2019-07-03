\name{axialAnalysis}
\alias{axialAnalysis}
\title{
Axial Analysis
}
\description{
Calls the depthmapXcli to carry out Axial Analysis on the currently visible map
}
\usage{
axialAnalysis(graphFileIn, graphFileOut, radii, includeChoice = FALSE,
              includeLocal = FALSE, includeIntermediateMetrics = FALSE,
              cliPath = getDefaultCLILocation(), verbose = FALSE)
}
\arguments{
  \item{graphFileIn}{
    The input graph file.
  }
  \item{graphFileOut}{
    The output graph file. If ommited will use the input file.
  }
  \item{radii}{
    One or more radii to limit the analysis at.
  }
  \item{includeChoice}{
    Set to TRUE to include choice.
  }
  \item{includeLocal}{
    Set to TRUE to include local measures.
  }
  \item{includeIntermediateMetrics}{
    Set to TRUE to include intermediate measures (RA, RRA and total depth).
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
