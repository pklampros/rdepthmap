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

export = function(graphFileIn, fileOut, exportType,
                  cliPath = getDefaultCLILocation(), verbose = FALSE) {
  depthmapXcli(c("-f", formatForCLI(graphFileIn),
                 "-o", formatForCLI(fileOut),
                 "-m", "EXPORT",
                 "-em", exportType), cliPath, verbose);
}

getPointmapData = function(graphFileIn, scale = 1,
                           cliPath = getDefaultCLILocation(), verbose = FALSE) {
  mapFile = tempfile()
  rdepthmap::export(graphFileIn, mapFile, "pointmap-data-csv", cliPath, verbose)
  dpm = processPointMap(mapFile, scale, ",")
  file.remove(mapFile)
  return(dpm);
}

getPointmapLinks = function(graphFileIn,
                            cliPath = getDefaultCLILocation(), verbose = FALSE) {
  csvFile = tempfile()
  rdepthmap::export(graphFileIn, csvFile, "pointmap-links-csv", cliPath, verbose)
  links = read.csv(csvFile)
  file.remove(csvFile)
  return(links);
}


getPointmapDataAndLinks = function(graphFileIn, scale = 1,
                                   cliPath = getDefaultCLILocation(), verbose = FALSE) {
  mapFile = tempfile()
  rdepthmap::export(graphFileIn, mapFile, "pointmap-data-csv")
  linkFile = tempfile()
  rdepthmap::export(graphFileIn, linkFile, "pointmap-links-csv")
  dpm = processPointMapAndLinks(mapFile, linkFile, scale, ",")
  file.remove(mapFile)
  file.remove(linkFile)
  return(dpm);
}

processPointMap = function(mapPath, scale = 1, sep = "\t") {
  pointMapData = read.csv(mapPath, sep = sep);
  pointMapData$x = pointMapData$x*scale;
  pointMapData$y = pointMapData$y*scale;
  pointMapData = cbind(pointMapData, refIDtoIndex(pointMapData$Ref));
  xycols = c("x","y");
  pointMapPoints = pointMapData[, xycols];
  pointMapData = pointMapData[,!(names(pointMapData) %in% xycols)];
  dpm = SpatialPointsDataFrame(pointMapPoints, data = pointMapData);
  makeGridded = function() {gridded(dpm) <<- TRUE}
  suppressWarnings(makeGridded())
  return(list(map = dpm, importScale = scale));
}

processPointMapAndLinks = function(mapPath, linkPath = NA, scale = 1, sep = "\t") {
  pointMap = processPointMap(mapPath, scale, sep)
  pointMap$links = NA
  if (!is.na(linkPath)) {
    pointMap$links = read.csv(linkPath, sep = sep)
  }
  return(pointMap);
}

getShapeGraph = function(graphFileIn,
                         cliPath = getDefaultCLILocation(), verbose = FALSE) {
  mapFile = tempfile(fileext = ".mif")
  rdepthmap::export(graphFileIn, mapFile, "shapegraph-map-mif", cliPath, verbose)
  ogr = readOGR(mapFile, verbose = verbose)
  file.remove(mapFile)
  return(ogr);
}
