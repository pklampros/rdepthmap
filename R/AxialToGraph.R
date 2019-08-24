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

axialToGraph = function(graphFile){
  ax.ogr = rdepthmap::getShapeGraph(graphFile)
  ax.connections = rdepthmap::getShapeGraphConnections(graphFile)
  for (i in 1:nrow(ax.connections)){
    ax.connections[i, ] = sort(ax.connections[i,c("refA","refB")])
  }
  ax.connections = ax.connections[!duplicated(ax.connections),]

  ax.ogr@data$x.coords = as.data.frame(gCentroid(ax.ogr, byid = T))[,1]
  ax.ogr@data$y.coords = as.data.frame(gCentroid(ax.ogr, byid = T))[,2]
  col = c("x.coords","y.coords")
  ax.ogr$ax.coords = as.matrix(ax.ogr@data[,col])

  ax.graph = graph.data.frame(ax.connections, directed = FALSE, vertices = ax.ogr@data$Depthmap_Ref)
  return(ax.graph);
}





