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

agentAnalysis = function(graphFileIn, graphFileOut = NA, lookMode, timesteps, releaseRate,
                         agentFOV, agentSteps, agentLife, originX = NA, originY = NA,
                         locationSeed = 0, numberOfTrails = NA, outputType = "graph",
                         cliPath = getDefaultCLILocation(), verbose = FALSE) {
  if (is.na(graphFileOut)) graphFileOut = graphFileIn;
  if (!(lookMode %in% c("standard", "los-length", "occ-length", "occ-any", "occ-group-45",
                        "occ-group-60", "occ-furthest", "bin-far-dist", "bin-angle",
                        "bin-far-dist-angle", "bin-memory"))) {
    stop(paste0("Unknown agent look mode: ", lookMode))
  }
  if (!(outputType %in% c("graph", "gatecounts", "trails"))) {
    stop(paste0("Unknown output type: ", outputType))
  }
  params = c("-f", formatForCLI(graphFileIn),
             "-o", formatForCLI(graphFileOut),
             "-m", "AGENTS",
             "-am", lookMode,
             "-ats", timesteps,
             "-arr", releaseRate,
             "-afov", agentFOV,
             "-asteps", agentSteps,
             "-alife", agentLife,
             "-alocseed", locationSeed,
             "-ot", outputType)
  if (!is.na(numberOfTrails)) params = c(params, "-atrails", numberOfTrails)
  if (!is.na(originX)) {
    source("makeTempPointFile")
    tmpPtz = makeTempPointFile(originX, originY);
    params = c(params, "-alocfile", tmpPtz)
  }
  depthmapXcli(params, cliPath, verbose);
}
