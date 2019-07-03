\name{refIDtoIndex}
\alias{refIDtoIndex}
\title{
Ref ID to index
}
\description{
Converts a depthmapX "Ref" ID to the indices (x,y) of a pixel
}
\usage{
refIDtoIndex(refID)
}
\arguments{
  \item{refID}{
    The Ref ID
  }
}
\value{
A pair of indices (x,y)
}
\author{
Petros Koutsolampros
}
\examples{
idx = refIDtoIndex(852645)
# outputs:
#    i   j
# 1 13 677
}
