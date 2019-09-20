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

VGA = function(graphFileIn, graphFileOut = NA, vgaMode, radii,
               cliPath = getDefaultCLILocation(), verbose = FALSE) {
  if (is.na(graphFileOut)) graphFileOut = graphFileIn;
  if (!(vgaMode %in% c("isovist", "visibility-global", "visibility-local",
                       "metric", "angular", "thruvision"))) {
    stop(paste0("Unknown VGA mode: ", vgaMode))
  }
  params = c("-f", formatForCLI(graphFileIn),
             "-o", formatForCLI(graphFileOut),
             "-m", "VGA",
             "-vr", paste0(radii, collapse = ","))
  if (vgaMode %in% c("isovist", "metric", "angular", "thruvision")) {
    params = c(params, "-vm", vgaMode);
  } else if (vgaMode == "visibility-global") {
    params = c(params, "-vm", "visibility");
    params = c(params, "-vg")
  } else if (vgaMode == "visibility-local") {
    params = c(params, "-vm", "visibility");
    params = c(params, "-local")
  }
  depthmapXcli(params, cliPath, verbose);
}
