\name{import}
\alias{import}
\alias{importLines}
\title{
Import data into a graph file
}
\description{
Import data into a graph file. importLines specifically imports SpatialLines or SpatialLinesDataFrame objects.
}
\usage{
import(filesToImport, graphFileOut, cliPath = getDefaultCLILocation(), verbose = FALSE)
importLines(linesIn, graphFileOut, cliPath = getDefaultCLILocation(), verbose = FALSE)
}
\arguments{
  \item{filesToImport}{
    A file to import. Currently CSV, TSV and DXF files are accepted
  }
  \item{linesIn}{
    A SpatialLines or SpatialLinesDataFrame object.
  }
  \item{graphFileOut}{
    The output graph file.
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
