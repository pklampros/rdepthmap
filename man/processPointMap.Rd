\name{processPointMap}
\alias{processPointMap}
\alias{processPointMapAndLinks}
\title{
Import a PointMap
}
\description{
The processPointMap* functions are provided to import exported map data back into R
}
\usage{
processPointMap(mapPath, scale = 1, sep = "\t")
processPointMapAndLinks(mapPath, linkPath = NA, scale = 1, sep = "\t")
}
\arguments{
  \item{mapPath}{
    Path to pointmap map CSV
  }
  \item{linkPath}{
    Path to pointmap links CSV
  }
  \item{scale}{
    Multiplier to rescale map after import
  }
  \item{sep}{
    Optional. Separator for import
  }
}
\value{
Returns a SptialPixelsDataFrame
}

\author{
Petros Koutsolampros
}
