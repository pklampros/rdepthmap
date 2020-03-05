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

isovist = function(graphFileIn, graphFileOut = NA, x, y, angle = NA, viewangle = NA,
                   cliPath = getDefaultCLILocation(), verbose = FALSE) {
  if (is.na(graphFileOut)) graphFileOut = graphFileIn;
  tmpPtz = tempfile(fileext = ".csv")
  dt = data.frame(x = x, y = y)
  targetedIsovist = F
  suppressWarnings({
    targetedIsovist = !is.na(angle)
  })
  if (targetedIsovist) {
    dt$angle = angle;
    dt$viewangle = viewangle;
  }
  write.table(dt, tmpPtz, row.names = F, quote = F, sep = ",")

  depthmapXcli(c("-f", formatForCLI(graphFileIn),
                 "-o", formatForCLI(graphFileOut),
                 "-m", "ISOVIST",
                 "-if", formatForCLI(tmpPtz)), cliPath, verbose);
  invisible(file.remove(tmpPtz));
}

isovist2pts = function(graphFileIn, graphFileOut = NA, x, y, toX, toY, viewangle,
                       cliPath = getDefaultCLILocation(), verbose = FALSE) {
  angles = 180 * atan2(toY - y, toX - x) / pi
  angles = ifelse(angles < 0, 360 + angles, angles)
  isovist(graphFileIn, graphFileOut, x, y, angles, viewangle, cliPath, verbose);
}

makeIsovists = function(graphFilePath, originX, originY, scale = 1,
                        cliPath = getDefaultCLILocation(), verbose = FALSE) {
  tmpGraph = tempfile(fileext = ".graph");
  rdepthmap::isovist(graphFilePath, tmpGraph, originX, originY)
  rdepthmap::convertMap(tmpGraph, tmpGraph, "convex")

  tmpMap = tempfile(fileext = ".mif")
  rdepthmap::export(tmpGraph, tmpMap, "shapegraph-map-mif")
  isovists = readOGR(tmpMap, verbose = verbose)

  file.remove(tmpGraph);
  file.remove(tmpMap);

  return(elide(isovists, bb = matrix(c(0,0,1,1), ncol = 2), scale = scale))
}
