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

linkMapCoords = function(graphFileIn, graphFileOut = NA, linkFromX, linkFromY,
                         linkToX, linkToY, unlink = FALSE, mapTypeToLink = "pointmaps",
                         cliPath = getDefaultCLILocation(), verbose = FALSE) {
  if (is.na(graphFileOut)) graphFileOut = graphFileIn;
  if (!(mapTypeToLink %in% c("pointmaps", "shapegraphs"))) {
    stop(paste0("Unknown map type: ", mapTypeToLink))
  }

  tmpPtz = paste0(tempfile(), ".tsv");
  dt = data.frame(x1 = linkFromX, y1 = linkFromY, x2 = linkToX, y2 = linkToY)
  write.table(dt, tmpPtz, row.names = F, quote = F, sep = "\t")


  params = c("-f", formatForCLI(graphFileIn),
             "-o", formatForCLI(graphFileOut),
             "-m", "LINK",
             "-lmt", mapTypeToLink,
             "-lm", ifelse(unlink, "unlink", "link"),
             "-lt", "coords",
             "-lf", tmpPtz)


  depthmapXcli(params, cliPath, verbose);
  removed = file.remove(tmpPtz)
}

linkMapRefs = function(graphFileIn, graphFileOut = NA, linkFrom, linkTo,
                       mapTypeToLink = "pointmaps", unlink = FALSE,
                       cliPath = getDefaultCLILocation(), verbose = FALSE) {
  if (is.na(graphFileOut)) graphFileOut = graphFileIn;
  if (!(mapTypeToLink %in% c("pointmaps", "shapegraphs"))) {
    stop(paste0("Unknown map type: ", mapTypeToLink))
  }

  tmpPtz = paste0(tempfile(), ".csv");
  dt = data.frame(reffrom = linkFrom, refto = linkTo)
  write.table(dt, tmpPtz, row.names = F, quote = F, sep = ",")


  params = c("-f", formatForCLI(graphFileIn),
             "-o", formatForCLI(graphFileOut),
             "-m", "LINK",
             "-lmt", mapTypeToLink,
             "-lm", ifelse(unlink, "unlink", "link"),
             "-lt", "refs",
             "-lf", tmpPtz)

  depthmapXcli(params, cliPath, verbose);
  removed = file.remove(tmpPtz)
}
