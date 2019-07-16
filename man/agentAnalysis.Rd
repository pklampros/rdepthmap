% Copyright 2019 Petros Koutsolampros
%
% This file is part of rdepthmap
%
% rdepthmap is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
%
% rdepthmap is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with rdepthmap  If not, see <https://www.gnu.org/licenses/>.

\name{agentAnalysis}
\alias{agentAnalysis}
\title{
Agent Analysis
}
\description{
Calls the depthmapXcli to carry out Agent Analysis on the currently visible map
}
\usage{
agentAnalysis(graphFileIn, graphFileOut = NA, lookMode, timesteps, releaseRate,
              agentFOV, agentSteps, agentLife, originX = NA, originY = NA,
              locationSeed = 0, numberOfTrails = NA, outputType = "graph",
              cliPath = getDefaultCLILocation(), verbose = FALSE)
}
\arguments{
  \item{graphFileIn}{
    The input graph file.
  }
  \item{graphFileOut}{
    The output graph file. If ommited will use the input file.
  }
  \item{lookMode}{
    Agent look mode. One of:
    "standard",
    "los-length" (Line of Sight length),
    "occ-length" (Occluded length),
    "occ-any" (Any occlusions),
    "occ-group-45" (Occlusion group bins - 45 degrees),
    "occ-group-60" (Occlusion group bins - 60 degrees),
    "occ-furthest" (Furthest occlusion per bin),
    "bin-far-dist" (Per bin far distance weighted),
    "bin-angle" (Per bin angle weighted),
    "bin-far-dist-angle" (Per bin far-distance and angle weighted) or
    "bin-memory" (Per bin memory).
  }
  \item{timesteps}{
    Number of total system timesteps.
  }
  \item{releaseRate}{
    Agent release rate (likelyhood of release per timestep).
  }
  \item{agentFOV}{
    Agent field-of-view (out of 32 bins = 360).
  }
  \item{agentSteps}{
    Agent steps before turn decision.
  }
  \item{agentLife}{
    Agent total lifetime (in timesteps).
  }
  \item{originX}{
    Agent starting point (x coordinates).
  }
  \item{originY}{
    Agent starting point (y coordinates).
  }
  \item{locationSeed}{
    Agents to start at random locations with specific seed (0 to 10). Default is 0.
  }
  \item{numberOfTrails}{
    Record trails for this amount of agents (set to 0 to record all, with max possible currently = 50).
  }
  \item{outputType}{
    Set output type. One of:
    "graph" (graph file, default),
    "gatecounts" (csv with cells of grid with gate counts) or
    "trails" (csv with lines showing path traversed by each agent).
  }
  \item{cliPath}{
    Optional. Provide the path to a different CLI.
  }
  \item{verbose}{
    Optional. Show more information of the process.
  }
}
\value{
No value returned, process is between files.
}
\author{
Petros Koutsolampros
}
