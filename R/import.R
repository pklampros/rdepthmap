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
  if (class(linesIn)[1] == "SpatialLinesDataFrame") {
    lineData = linesIn@lines;
  } else if (class(linesIn)[1] == "SpatialLines") {
    lineData = linesIn;
  } else {
    stop(paste0("Lines in can only be of type SpatialLines or SpatialLinesDataFrame, not: ",
                class(linesIn)[1]))
  }
  linesDF = do.call("rbind", lapply(lineData, FUN = function(lines) {
    coords = lines@Lines[[1]]@coords
    return(data.frame(x1 = coords[1,1],
                      y1 = coords[1,2],
                      x2 = coords[2,1],
                      y2 = coords[2,2]))
  }));
  if (class(linesIn)[1] == "SpatialLinesDataFrame") {
    linesDF = cbind(linesDF, linesIn@data)
  }
  tmpGraph = tempfile(fileext = ".tsv");
  write.table(linesDF, tmpGraph, row.names = F, quote = F, sep = "\t")

  rdepthmap::import(tmpGraph, graphFileOut)
}
