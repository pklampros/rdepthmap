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

convertMap = function(graphFileIn, graphFileOut = NA, newMapType, newMapName = generateRandomCapString(10),
                      removeInputMap = FALSE, copyAttributes = FALSE, stubLengthToRemove = NA,
                      cliPath = getDefaultCLILocation(), verbose = FALSE) {
  if (is.na(graphFileOut)) graphFileOut = graphFileIn;
  if (!(newMapType %in% c("drawing", "axial", "segment", "data", "convex"))) {
    stop(paste0("Unknown map type: ", newMapType))
  }
  params = c("-f", formatForCLI(graphFileIn),
             "-o", formatForCLI(graphFileOut),
             "-m", "MAPCONVERT",
             "-co", newMapType,
             "-con", formatForCLI(newMapName))
  if (removeInputMap) params = c(params, "-cir")
  if (copyAttributes) params = c(params, "-coc")
  if (!is.na(stubLengthToRemove)) params = c(params, "-crsl", stubLengthToRemove)
  depthmapXcli(params, cliPath, verbose);
}
