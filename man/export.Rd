\name{export}
\alias{export}
\alias{getPointmapData}
\alias{getPointmapLinks}
\alias{getPointmapDataAndLinks}
\alias{getShapeGraph}
\title{
Export data
}
\description{
Exports data from a graph file. The getPointmap* functions are provided for convenience to export to temporary file and import to R directly.
}
\usage{
export(graphFileIn, fileOut, exportType,
       cliPath = getDefaultCLILocation(), verbose = FALSE)

getPointmapData(graphFileIn, scale = 1,
       cliPath = getDefaultCLILocation(), verbose = FALSE)
getPointmapLinks(graphFileIn,
       cliPath = getDefaultCLILocation(), verbose = FALSE)
getPointmapDataAndLinks(graphFileIn, scale = 1,
       cliPath = getDefaultCLILocation(), verbose = FALSE)

getShapeGraph(graphFileIn,
       cliPath = getDefaultCLILocation(), verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graphFileIn}{
    The input .graph file.
  }
  \item{fileOut}{
    The output file.
  }
  \item{exportType}{
    What to export. Available types are:
    "pointmap-data-csv",
    "pointmap-connections-csv",
    "pointmap-links-csv",
    "shapegraph-map-csv",
    "shapegraph-map-mif" or
    "shapegraph-connections-csv".
  }
  \item{scale}{
    Provided for when importing a pointmap directly. Will scale the whole map.
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
