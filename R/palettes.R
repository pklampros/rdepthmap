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

makeDepthmapClassicColour = function(value, rangeMin, rangeMax) {
  if (rangeMin > rangeMax) {
    value = 1.0 - value;
    rangeMin = 1.0 - rangeMin;
    rangeMax = 1.0 - rangeMax;
  }
  green = rangeMin + (rangeMax - rangeMin) / 10.0;
  r = g = b = 0;
  offset = 0.0333;

  htmlByte = function(colorByte) {
    return(floor((colorByte + offset) * 15.0) * 17);
  }

  # NB previously included colour muting: the 1.0 was originally 0.9 to mute the colours slightly
  if ((value >= 0.0) && (value < rangeMin)) {
    r = htmlByte(0.5 * (rangeMin - value)/rangeMin)
    b = 0xFF;
  }
  else if ((value >= rangeMin) && (value < (green + rangeMin)/2.0)) {
    b = 0xFF;
    g = htmlByte(2.0*(value - rangeMin)/(green - rangeMin))
  }
  else if ((value >= (green + rangeMin)/2.0) && (value < green)) {
    b = htmlByte(2.0*(green - value)/(green - rangeMin))
    g = 0xFF;
  }
  else if ((value >= green) && (value < (green + rangeMax)/2.0)) {
    g = 0xFF;
    r = htmlByte(2.0*(value - green)/(rangeMax - green))
  }
  else if ((value >= (green + rangeMax)/2.0) && (value < rangeMax)) {
    g = htmlByte(2.0*(rangeMax - value)/(rangeMax - green))
    r = 0xFF;
  }
  else if (value >= rangeMax) {
    r = 0xFF;
    b = htmlByte(0.5 * (value - rangeMax)/(1.0 - rangeMax))
  }
  return(grDevices::rgb(r/255.0,g/255.0,b/255.0));
}
depthmap.classic.colour = function(n) {
  unlist(lapply(0:(n - 1), function(i) {
    makeDepthmapClassicColour(i/n, 0, 1)
  }))
}

makeAxmanesqueColour = function(value, rangeMin = 0, rangeMax = 1) {
  g_nicecolor = c(
    "#3333DD", # 0 blue
    "#3388DD", # 1
    "#22CCDD", # 2
    "#22CCBB", # 3
    "#22DD88", # 4
    "#88DD22", # 5
    "#BBCC22", # 6
    "#DDCC22", # 7
    "#DD8833", # 8
    "#DD3333" # 9 red
  );
  f = (value - rangeMin) / (rangeMax - rangeMin);
  if (rangeMin > rangeMax) {
    f = 1.0 - f;
  }
  return(g_nicecolor[1 + as.integer((f - 1e-9) * length(g_nicecolor))]);
}
depthmap.axmanesque.colour = function(n) {
  unlist(lapply(0:(n - 1), function(i) {
    makeAxmanesqueColour(i/n)
  }))
}

makePurpleOrangeColour = function(value, rangeMin = 0, rangeMax = 1) {
  g_purpleorange = c(
    "#542788", # 0 purple
    "#998EC3", # 1
    "#D8DAEB", # 2
    "#F7F7F7", # 3
    "#FEE0B6", # 4
    "#F1A340", # 5
    "#B35806" # 6 orange
  );
  f = (value - rangeMin) / (rangeMax - rangeMin);
  if (rangeMin > rangeMax) {
    f = 1.0 - f;
  }
  return(g_purpleorange[1 + as.integer((f - 1e-9) * length(g_purpleorange))]);
}
depthmap.purpleorange.colour = function(n) {
  unlist(lapply(0:(n - 1), function(i) {
    makePurpleOrangeColour(i/n)
  }))
}

makeBlueRedColour = function(value, rangeMin = 0, rangeMax = 1) {
  g_bluered = c(
    "#4575B4", # 0 blue
    "#91BFDB", # 1
    "#E0F3F8", # 2
    "#FFFFBF", # 3
    "#FEE090", # 4
    "#FC8D59", # 5
    "#D73027" # 6 red
  );
  f = (value - rangeMin) / (rangeMax - rangeMin);
  if (rangeMin > rangeMax) {
    f = 1.0 - f;
  }
  return(g_bluered[1 + as.integer((f - 1e-9) * length(g_bluered))]);
}
depthmap.bluered.colour = function(n) {
  unlist(lapply(0:(n - 1), function(i) {
    makeBlueRedColour(i/n)
  }))
}

makeGreyScaleColour = function(value, rangeMin = 0, rangeMax = 1) {
  g_greyscale = c(
    "#000000", # 0 black
    "#444444", # 1
    "#777777", # 2
    "#AAAAAA", # 3
    "#CCCCCC", # 4
    "#EEEEEE", # 5
    "#FFFFFF" # 6 white
  );
  f = (value - rangeMin) / (rangeMax - rangeMin);
  if (rangeMin > rangeMax) {
    f = 1.0 - f;
  }
  return(g_greyscale[1 + as.integer((f - 1e-9) * length(g_greyscale))]);
}
depthmap.grayscale.colour = function(n) {
  unlist(lapply(0:(n - 1), function(i) {
    makeGreyScaleColour(i/n)
  }))
}

makeNiceHSBColour = function(value, rangeMin = 0, rangeMax = 1) {
  g_nicecolorhsb = c(
    "#3333DD", # 0 blue
    "#3377DD", # 1
    "#33BBDD", # 2
    "#33DDBB", # 3
    "#33DD55", # 4
    "#55DD33", # 5
    "#BBDD33", # 6
    "#DDBB33", # 7
    "#DD7733", # 8
    "#DD3333" # 9 red
  );
  f = (value - rangeMin) / (rangeMax - rangeMin);
  if (rangeMin > rangeMax) {
    f = 1.0 - f;
  }
  return(g_nicecolorhsb[1 + as.integer((f - 1e-9) * length(g_nicecolorhsb))]);
}
depthmap.nicehsb.colour = function(n) {
  unlist(lapply(0:(n - 1), function(i) {
    makeNiceHSBColour(i/n)
  }))
}
