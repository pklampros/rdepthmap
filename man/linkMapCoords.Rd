\name{link}
\alias{linkMapCoords}
\alias{linkMapRefs}
\title{
Link map elements
}
\description{
Calls upon the depthmapXcli to link elements (cells in VGA, lines in axial/segment)
}
\usage{
linkMapCoords(graphFileIn, graphFileOut = NA, linkFromX, linkFromY,
              linkToX, linkToY, unlink = FALSE, mapTypeToLink = "pointmaps",
              cliPath = getDefaultCLILocation(), verbose = FALSE)
linkMapRefs(graphFileIn, graphFileOut = NA, linkFrom, linkTo,
            mapTypeToLink = "pointmaps", unlink = FALSE,
            cliPath = getDefaultCLILocation(), verbose = FALSE)
}
\arguments{
  \item{graphFileIn}{
    The input graph file.
  }
  \item{graphFileOut}{
    The output graph file. If ommited will use the input file.
  }
  \item{linkFromX}{
    Link start x-coordinate.
  }
  \item{linkFromY}{
    Link start y-coordinate.
  }
  \item{linkToX}{
    Link end x-coordinate.
  }
  \item{linkToY}{
    Link end y-coordinate.
  }
  \item{linkFrom}{
    Link start Ref.
  }
  \item{linkTo}{
    Link end Ref.
  }
  \item{unlink}{
    Set to TRUE to unlink instead of linking.
  }
  \item{mapTypeToLink}{
    Map type group to select displayed map from. One of:
    "pointmaps" (default, vga: link) or
    "shapegraphs" (axial:link/unlink, segment:link, convex:link)
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
