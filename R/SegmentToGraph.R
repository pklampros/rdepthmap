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


segm.df = read.table(graphFileOut, header = TRUE, sep = ",", check.names = FALSE)
segm.ogr = SpatialLinesDataFrame(SpatialLines(lapply(1:nrow(segm.df), FUN=function(i) {
  Lines(Line(matrix(c(segm.df[i,'x1'], segm.df[i,'x2'], segm.df[i,'y1'], segm.df[i,'y2']),ncol=2)), ID=i)})),
  data = data.frame(segm.df[,!(names(segm.df) %in% c('x1', 'x2', 'y1', 'y2'))], row.names = 1:nrow(segm.df), check.names = FALSE))
segm.ogr@proj4string@projargs = linesIn@proj4string@projargs


segm.connections = read.table(graphFileOut,header = TRUE, sep = ",")
for (i in 1:nrow(segm.connections)){
  segm.connections[i, ] = sort(segm.connections[i,c("refA","refB")])
}
segm.connections = segm.connections[!duplicated(segm.connections),]


segm.ogr@data$x.coords = as.data.frame(gCentroid(Csegm.ogr, byid = T))[,1]
segm.ogr@data$y.coords = as.data.frame(gCentroid(segm.ogr, byid = T))[,2]
segm.ogr$segm.coords = as.matrix(segm.ogr@data[,col])

refA = segm.connections$refA
refB = segm.connections$refB
Depth_Ref = segm.ogr@data$Ref
segm.connections$weight = ((segm.ogr@data[match(refA, Depth_Ref), "Segment Length"])+(segm.ogr@data[match(refB, Depth_Ref), "Segment Length"]))/2

segm.graph = graph.data.frame(segm.connections, directed = FALSE, vertices = segm.ogr@data$Ref)
E(segm.graph)$weight = segm.connections$weight
