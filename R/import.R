# Copyright 2019 Fani Kostourou
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

import = function(filesToImport, graphFileOut,
                  cliPath = getDefaultCLILocation(), verbose = FALSE) {
  params = c("-f", formatForCLI(filesToImport[1]),
             "-o", formatForCLI(graphFileOut),
             "-m", "IMPORT");
  for (fileToImport in tail(filesToImport, length(filesToImport) - 1)) {
    params = c(params, "-if", fileToImport)
  }
  depthmapXcli(params, cliPath, verbose);
}

importLines = function(linesIn, graphFileOut,
                       cliPath = getDefaultCLILocation(), verbose = FALSE) {
  if (class(linesIn)[1] != "sf") {
    stop(paste0("Lines in can only be of type sf, not: ",
                class(linesIn)[1]))
  }

  startCoords = st_coordinates(st_line_sample(linesIn, sample=c(0)))
  endCoords = st_coordinates(st_line_sample(linesIn, sample=c(1)))

  linesDF = st_drop_geometry(linesIn)
  linesDF['x1'] = startCoords[,'X']
  linesDF['y1'] = startCoords[,'Y']
  linesDF['x2'] = endCoords[,'X']
  linesDF['y2'] = endCoords[,'Y']

  tmpGraph = tempfile(fileext = ".tsv");
  write.table(linesDF, tmpGraph, row.names = F, quote = F, sep = "\t")

  rdepthmap::import(tmpGraph, graphFileOut)
}
