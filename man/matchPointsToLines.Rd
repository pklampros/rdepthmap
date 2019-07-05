\name{matchPointsToLines}
\alias{matchPointsToLines}
\title{
Match points to their closest line
}
\description{
Matches (spatial-join) points to lines. Finds the point closest to a line. One point is attached to one line, thus if fewer points than lines are given then some lines will have no point attached.
}
\usage{
matchPointsToLines(points, lines, getIndex = FALSE)
}
\arguments{
  \item{points}{
    Points to attach.
  }
  \item{lines}{
    Lines to attach to.
  }
  \item{getIndex}{
    Get the index returned and not the data.
  }
}
\value{
If getIndex is TRUE then the index of the points as they relate to the matching lines are given. If not, then the data from the points dataframe is returned.
}
\author{
Petros Koutsolampros
}
