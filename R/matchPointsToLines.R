# Copyright 2019 Kimon Krenz
# Copyright 2019 Petros Koutsolampros
#
# This file is part of rdepthmap
#
# rdepthmap is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# rdepthmap is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with rdepthmap  If not, see <https://www.gnu.org/licenses/>.

matchPointsToLines = function(points, lines, getIndex = FALSE) {

  d = sf::st_distance(lines, points)

  # this is done to retain the name of the
  # row/column through filtering
  rownames(d) = 1:nrow(d)
  colnames(d) = 1:ncol(d)

  {
    # the process needs to be done for the fewer objects
    # i.e. if we have 1000 lines, but 10 points then the
    # process only needs to run 10 times
    mindim = min(ncol(d), nrow(d))
    # create a row of matches to store the row/column names
    matches = matrix(ncol = 2, nrow = mindim)
    # first run is special as it's on the raw distance matrix
    i = 1
    matches[i,] = arrayInd(which.min(d), dim(d))
    # for all other runs, except the last:
    lapply(2:(mindim - 1), function(i) {
      # filter the matrix based on the previous matches
      m = d[-matches[1:(i-1),1],
            -matches[1:(i-1),2]]
      # find the index of the smallest value (closest point to line)
      idcs = arrayInd(which.min(m), dim(m))
      # add to matches, making sure to take row/column names
      matches[i,] <<- as.integer(c(rownames(m)[idcs[,1]], colnames(m)[idcs[,2]]))
    })
    # for the last run R converts the matrix to a list
    # because there's only one row left
    i=mindim
    m = d[!(rownames(d) %in% matches[1:(i-1),1]),
          !(colnames(d) %in% matches[1:(i-1),2])]
    # reconvert to matrix
    m = as.matrix(m)
    idcs = arrayInd(which.min(as.matrix(m)), dim(as.matrix(m)))
    matches[i,] = as.integer(c(rownames(m)[idcs[,1]], colnames(m)[idcs[,2]]))
    matches <<- matches
  }

  rowidx = rep(NA, nrow(lines))
  rowidx[matches[,1]] = matches[,2]
  if(getIndex) {
    return(rowidx)
  } else {
    return(points[rowidx,])
  }
}
