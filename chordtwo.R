Skip to content
modern data  new generation of data science	
PLOTCON

SUPPORT

CONSULTING
FILLED CHORD DIAGRAM IN R USING PLOTLY
Published November 8, 2016 by Riddhiman in Business Intelligence, Dashboards, Data Visualization, R
4
In this post we’ll create a Filled Chord Diagram using plotly. The post is inspired by Plotly’s Python documentation.

INSTALL / UPDATE PACKAGES
Just to ensure we are working with the latest dev version of plotly.


# Install packages if needed
# install.packages(c("devtools", "dplyr"))
# library(devtools)
# install_github("ropensci/plotly")

library(plotly)
library(dplyr)
1
2
3
4
5
6
7
# Install packages if needed
# install.packages(c("devtools", "dplyr"))
# library(devtools)
# install_github("ropensci/plotly")

library(plotly)
library(dplyr)
DATASET
The dataset we’ll use consists of the number of comments a person made on her own facebook posts as well as the number of comments on her friend’s facebook posts.


# Dataset for creating chord diagram ------------------------------------------
# Data for number of facebook posts
# See https://plot.ly/python/filled-chord-diagram/

df <- rbind(c(16, 3, 28, 0, 18),
            c(18, 0, 12, 5, 29),
            c(9, 11, 17, 27, 0),
            c(19, 0, 31, 11, 12),
            c(23, 17, 10, 0, 34))
df <- data.frame(df)

colnames(df) <- c('Emma', 'Isabella', 'Ava', 'Olivia', 'Sophia')
rownames(df) <- c('Emma', 'Isabella', 'Ava', 'Olivia', 'Sophia')
1
2
3
4
5
6
7
8
9
10
11
12
13
# Dataset for creating chord diagram ------------------------------------------
# Data for number of facebook posts
# See https://plot.ly/python/filled-chord-diagram/

df <- rbind(c(16, 3, 28, 0, 18),
            c(18, 0, 12, 5, 29),
            c(9, 11, 17, 27, 0),
            c(19, 0, 31, 11, 12),
            c(23, 17, 10, 0, 34))
df <- data.frame(df)

colnames(df) <- c('Emma', 'Isabella', 'Ava', 'Olivia', 'Sophia')
rownames(df) <- c('Emma', 'Isabella', 'Ava', 'Olivia', 'Sophia')
GLOBAL SETTINGS
These are some plot related settings which we can setup right now. These settings will be fed to plot_ly() later on. Also, having these aesthetic settings accessible now will make it easier for us to make changes later on.


# Settings --------------------------------------------------------------------
# Over all plot settings like color and transparency

cols <- RColorBrewer::brewer.pal(nrow(df), "Set1")  # Set of colors (n = number of rows in data)
opacity <- 0.5  # Opacity of ideogram
chord.opacity <- 0.3  # Opcaity of individual chords
linecolor <- "black"
circlefill <- "#f2f2f2"
inner.radius <- 0.93
gap <- 0.02
1
2
3
4
5
6
7
8
9
10
# Settings --------------------------------------------------------------------
# Over all plot settings like color and transparency

cols <- RColorBrewer::brewer.pal(nrow(df), "Set1")  # Set of colors (n = number of rows in data)
opacity <- 0.5  # Opacity of ideogram
chord.opacity <- 0.3  # Opcaity of individual chords
linecolor <- "black"
circlefill <- "#f2f2f2"
inner.radius <- 0.93
gap <- 0.02
CREATING THE IDEOGRAM
We’ll first create the ideogram. The ideogram which is essentially a set of sectors plotted on a unit circle which’ll represent the rowsums of the dataset i.e. the total number of comments made by each person (to themselves and their friends). We’ll first create some helper functions – toAngular() to map a vector of numeric values onto the unit circle using cumulative sums essentially creating sectors and addGaps() which’ll create some space between each sector for aesthetic purposes.


# Function Definition: addGaps() ----------------------------------------------
addGaps <- function(theta, gap = 0.05){
  
  # Takes a vector of angles and adds a gap in-between them
  # Adds and subtracts the gap value from computed angle
  
  newtheta <- data.frame()
  
  for (i in 1:length(theta)) {
    
    if(i == 1){
      x <- 0 + gap
      y <- theta[i] - gap
      newtheta <- rbind(newtheta, c(x, y))
    }else{
      x <- theta[i - 1] + gap
      y <- theta[i] - gap
      newtheta <- rbind(newtheta, c(x, y))
    }
  }
  
  newtheta <- data.frame(theta, newtheta)
  colnames(newtheta) <- c("theta", "start", "end")
  
  return(newtheta)
}

# Function Definition: toAngular() --------------------------------------------
toAngular <- function(x, rad = 1, gap = 0.05, lower = 0, upper = 2*pi, addgaps = T){
  
  # Maps a set of numbers onto the unit circle by computing cumulative
  # sums and assigning angles to each sum
  
  cumtotals <- cumsum(x / sum(x))
  
  # Upper and lower bounds are the angle limits to which mapping
  # is limited. Ex 0 - 2PI
  delta <- ifelse(upper > lower, upper - lower, (2*pi) - lower + upper)
  theta <-  cumtotals * delta
  
  x <- rad * cos(theta)
  y <- rad * sin(theta)
  
  df <- data.frame(x, y)
  
  # Additionally, add gaps in between each sector using the addGaps() function
  if (addgaps == T) {
    gaps <- addGaps(theta, gap = gap)
    ret <- list(theta = theta,
                coord = df,
                gaps = gaps)
  }else{
    ret <- list(theta = theta,
                coord = df)
  }
  
  return(ret)
}
1
2
3
4
5
6
7
8
9
10
11
12
13
14
15
16
17
18
19
20
21
22
23
24
25
26
27
28
29
30
31
32
33
34
35
36
37
38
39
40
41
42
43
44
45
46
47
48
49
50
51
52
53
54
55
56
57
58
# Function Definition: addGaps() ----------------------------------------------
addGaps <- function(theta, gap = 0.05){
  
  # Takes a vector of angles and adds a gap in-between them
  # Adds and subtracts the gap value from computed angle
  
  newtheta <- data.frame()
  
  for (i in 1:length(theta)) {
    
    if(i == 1){
      x <- 0 + gap
      y <- theta[i] - gap
      newtheta <- rbind(newtheta, c(x, y))
    }else{
      x <- theta[i - 1] + gap
      y <- theta[i] - gap
      newtheta <- rbind(newtheta, c(x, y))
    }
  }
  
  newtheta <- data.frame(theta, newtheta)
  colnames(newtheta) <- c("theta", "start", "end")
  
  return(newtheta)
}

# Function Definition: toAngular() --------------------------------------------
toAngular <- function(x, rad = 1, gap = 0.05, lower = 0, upper = 2*pi, addgaps = T){
  
  # Maps a set of numbers onto the unit circle by computing cumulative
  # sums and assigning angles to each sum
  
  cumtotals <- cumsum(x / sum(x))
  
  # Upper and lower bounds are the angle limits to which mapping
  # is limited. Ex 0 - 2PI
  delta <- ifelse(upper > lower, upper - lower, (2*pi) - lower + upper)
  theta <-  cumtotals * delta
  
  x <- rad * cos(theta)
  y <- rad * sin(theta)
  
  df <- data.frame(x, y)
  
  # Additionally, add gaps in between each sector using the addGaps() function
  if (addgaps == T) {
    gaps <- addGaps(theta, gap = gap)
    ret <- list(theta = theta,
                coord = df,
                gaps = gaps)
  }else{
    ret <- list(theta = theta,
                coord = df)
  }
  
  return(ret)
}
Now that the functions are defined we need to create the ideogram. We do so by:
  
  Creating an outer ring which’ll lie on the unit circle
Creating an inner ring which lie inside the outer circle (radius is defined in the settings section above)
Creating an SVG path for each sector bounded by the outer and inner circles and the end points of each gap

# Create ideogram -------------------------------------------------------------
# See See https://plot.ly/python/filled-chord-diagram/

# The ideogram is constructed of the row sums i.e total interactions in each row
dat <- rowSums(df)

# Outer ring is the unit circle
outer <- toAngular(dat, gap = gap)

# Inner ring has radius < 1
inner <- toAngular(dat, rad = inner.radius, gap = gap)

# Ideogram is charted as a svg path and fed to plot_ly() as a shape
# Compute a path for each sector of the ideogram by combining the 
# coordinates of the outer and inner circles
outer.inner <- rbind(outer$gaps, inner$gaps) %>% arrange(theta)  # arrange in increasing order of theta

# Each sector of the ideogram is made of four points - 
# the start and end points of the outer and inner circles
# Hence increment by 2 and not 1
vec <- seq(1, nrow(outer.inner), by = 2)

# Create and empty dataframe
ideogram <- data.frame()

k <- 1  # Counter for each row / group

# Loop through each sector and create a svg path using the start and end
# points of the outer and inner circles
for (i in vec) {
  
  # Get starting and ending point for 'i' th sector
  start <- outer.inner$start[i]
  end <- outer.inner$end[i]
  
  # Ensure starting point is always less than ending point of sector
  if (start > end) start <- start - (2*pi)
  
  # Create a sequence of thetas along the sector
  thetas <- seq(start, end, length.out = 100)
  
  # Compute x and y coordinates
  x <- c(cos(thetas), inner.radius * cos(rev(thetas)))
  y <- c(sin(thetas), inner.radius * sin(rev(thetas)))
  
  # Add a group for easy subsetting later on
  coords <- data.frame(x, y, group = k)
  ideogram <- rbind(ideogram, coords)
  
  # Increment group number
  k <- k + 1
}

# Function definition: createPath() -------------------------------------------
createPath <- function(df){
  
  # Given x and y coordinates creates a string containing a svg path
  # that can be fed to plotly as a shape
  
  start <- paste("M", df$x[1], df$y[1])
  path <- paste("L", df$x[-1], df$y[-1], collapse = " ")
  path <- paste(start, path, "Z")
  return(path)
}

# Use group numbers assigned to each sector to subset and create a path string
ideogram.path <- by(ideogram, ideogram$group, createPath)

# Plot the ideogram (just as a check). Chord diagram is generated separately later
# Create shape list
ideogram.shapes <- list()  # Used later on

for (i in 1:nrow(df)) {
  
  # Use plotly syntax to save shapes of each sector as a list
  ideogram.shapes[[i]] <- list(type = "path",
                               path = ideogram.path[i],
                               fillcolor = cols[i],
                               line = list(color = linecolor, width = 1),
                               opacity = opacity)
}

# Just to check if things are looking okay
ideogram.plot <- plot_ly(height = 800, width = 800) %>%
  layout(
    xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
    yaxis = list(showgrid = F, zeroline = F, showticklabels = F),
    shapes = ideogram.shapes)

ideogram.plot
1
2
3
4
5
6
7
8
9
10
11
12
13
14
15
16
17
18
19
20
21
22
23
24
25
26
27
28
29
30
31
32
33
34
35
36
37
38
39
40
41
42
43
44
45
46
47
48
49
50
51
52
53
54
55
56
57
58
59
60
61
62
63
64
65
66
67
68
69
70
71
72
73
74
75
76
77
78
79
80
81
82
83
84
85
86
87
88
89
90
# Create ideogram -------------------------------------------------------------
# See See https://plot.ly/python/filled-chord-diagram/

# The ideogram is constructed of the row sums i.e total interactions in each row
dat <- rowSums(df)

# Outer ring is the unit circle
outer <- toAngular(dat, gap = gap)

# Inner ring has radius < 1
inner <- toAngular(dat, rad = inner.radius, gap = gap)

# Ideogram is charted as a svg path and fed to plot_ly() as a shape
# Compute a path for each sector of the ideogram by combining the 
# coordinates of the outer and inner circles
outer.inner <- rbind(outer$gaps, inner$gaps) %>% arrange(theta)  # arrange in increasing order of theta

# Each sector of the ideogram is made of four points - 
# the start and end points of the outer and inner circles
# Hence increment by 2 and not 1
vec <- seq(1, nrow(outer.inner), by = 2)

# Create and empty dataframe
ideogram <- data.frame()

k <- 1  # Counter for each row / group

# Loop through each sector and create a svg path using the start and end
# points of the outer and inner circles
for (i in vec) {
  
  # Get starting and ending point for 'i' th sector
  start <- outer.inner$start[i]
  end <- outer.inner$end[i]
  
  # Ensure starting point is always less than ending point of sector
  if (start > end) start <- start - (2*pi)
  
  # Create a sequence of thetas along the sector
  thetas <- seq(start, end, length.out = 100)
  
  # Compute x and y coordinates
  x <- c(cos(thetas), inner.radius * cos(rev(thetas)))
  y <- c(sin(thetas), inner.radius * sin(rev(thetas)))
  
  # Add a group for easy subsetting later on
  coords <- data.frame(x, y, group = k)
  ideogram <- rbind(ideogram, coords)
  
  # Increment group number
  k <- k + 1
}

# Function definition: createPath() -------------------------------------------
createPath <- function(df){
  
  # Given x and y coordinates creates a string containing a svg path
  # that can be fed to plotly as a shape
  
  start <- paste("M", df$x[1], df$y[1])
  path <- paste("L", df$x[-1], df$y[-1], collapse = " ")
  path <- paste(start, path, "Z")
  return(path)
}

# Use group numbers assigned to each sector to subset and create a path string
ideogram.path <- by(ideogram, ideogram$group, createPath)

# Plot the ideogram (just as a check). Chord diagram is generated separately later
# Create shape list
ideogram.shapes <- list()  # Used later on

for (i in 1:nrow(df)) {
  
  # Use plotly syntax to save shapes of each sector as a list
  ideogram.shapes[[i]] <- list(type = "path",
                               path = ideogram.path[i],
                               fillcolor = cols[i],
                               line = list(color = linecolor, width = 1),
                               opacity = opacity)
}

# Just to check if things are looking okay
ideogram.plot <- plot_ly(height = 800, width = 800) %>%
  layout(
    xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
    yaxis = list(showgrid = F, zeroline = F, showticklabels = F),
    shapes = ideogram.shapes)

ideogram.plot
You should now have something similar to this:
  
  
  
  CREATING CHORDS
The following set of code snippets essentially do this:
  
  Divide each sector on the ideogram into sub – sectors based on the number of comments made by each person i.e. traverse the dataset row-wise and map the numeric vectors in each row onto the associated sector (not the unit circle). Example: Emma has a total of 65 comments amongst herself and her friends. The sector corresponding to Emma’s 65 comments needs to be divided into further sectors based on the 16, 3, 28, 0 and 18 comments.
Find the four points that bind each chord. A chord is a planar shape that is bound by two bezier curves and two circular arcs. For each bezier curve the control point is generated by finding the mean of the angluar coordinates of the end points.
Fill each chord (ribbon) with the appropriate color. Example: Emma made 18 comments on Sophia’s posts but Sophia made 23 comments on Emma’s posts. Since Sophia made more comments, the chord depicting the interaction betweem Emma and Sophia is colored using the same color that is used for coloring Sophia’s sector on the ideogram.
Create SVG shapes for each chord and then plot using plot_ly()
Note that the ordering of the endpoints of bezier curves and circular arcs is tricky and was done based on trial and error.


# Create chords ---------------------------------------------------------------
# Divide each sector corresponding to each interaction in each row
sector.angles <- inner$gaps
angle.list <- data.frame()
for (i in 1:nrow(sector.angles)) {
  # Get starting and ending points of each sector
  start <- sector.angles$start[i]
  end <- sector.angles$end[i]
  
  # Sort each row from increasing to decreasing
  dat <- sort(df[i,])
  
  # Use toAngular() function to get thetas corresponding to each row item
  angle <- toAngular(as.numeric(dat), lower = start, upper = end, addgaps = F)$theta
  
  # Offset by the starting point since the function returns values in 
  # the [0 - (start - end)] interval
  angle <- c(start + angle)
  
  # Collate all the data for each division of the sector 
  temp <- data.frame(from = rownames(sector.angles)[i],
                     to = names(dat),
                     value = as.numeric(dat),
                     angle,
                     x = inner.radius * cos(angle),
                     y = inner.radius * sin(angle),
                     stringsAsFactors = F)
  
  # Add the starting point to the divisions
  # If min value in a row is zero then starting point for that division
  # must be the starting point of the sector
  startrow <- data.frame(from = rownames(sector.angles)[i],
                         to = "start",
                         value = 0,
                         angle = sector.angles$start[i],
                         x = inner.radius * cos(sector.angles$start[i]),
                         y = inner.radius * sin(sector.angles$start[i]),
                         stringsAsFactors = F)
  
  angle.list <- rbind(angle.list, startrow, temp)
}

# Create unique path IDs i.e. each set of interactions gets a unique ID
# Example - A -> B and B -> A will get the same ID
k <- 1
angle.list$ID <- rep(0, nrow(angle.list))
revstr <- paste(angle.list$to, angle.list$from)

for (i in 1:nrow(angle.list)) {
  if (angle.list$ID[i] == 0) {
    from = angle.list$from[i]
    to = angle.list$to[i]
    str <- paste(from, to)
    mtch <- match(str, revstr)
    
    if (!is.na(mtch)) {
      angle.list$ID[c(i, mtch)] <- k
      k <- k + 1
    }
  }
}

# Each chord is bounded by four points: 
# 1. two actual data points corrosponding to the actual interaction i.e. A -> B (p1) and B -> A (p2)
# 2. And two previous data points to complete the polygon
# We'll create some helper functions

# Function definition: bezierCurve() ------------------------------------------
bezierCurve <- function(t1, t2){
  
  # Takes two angles as arguments and returns the x and y coordinates
  # of a quadratic bezier curve 
  
  t <- seq(0, 1, length.out = 100)
  
  p0 <- c(inner.radius * cos(t1), inner.radius * sin(t1))  # Starting point (t1)
  p2 <- c(inner.radius * cos(t2), inner.radius * sin(t2))  # Ending point (t2)
  p1 <- c(-inner.radius * cos(mean(t1, t2)), -inner.radius * sin(mean(t1, t2)))  # Control point
  
  # Curve =  (1 - t^2)*p0 + 2(t-1)t*p1 + t^2*p2
  x <- (1 - t**2) * p0[1] + 2*(1 - t)*t * p1[1] + t**2 * p2[1]
  y <- (1 - t**2) * p0[2] + 2*(1 - t)*t * p1[2] + t**2 * p2[2]
  df <- data.frame(x, y)
  
  return(df)
}

# Function definition: circleCurve() ------------------------------------------
circleCurve <- function(t1, t2){
  
  # Returns the x and y coordinates of points lying on the inner 
  # boundary of the ideogram bounded by two angles t1 and t2 
  
  t <- seq(min(t1, t2), max(t1, t2), length.out = 50)
  x <- inner.radius * cos(t)
  y <- inner.radius * sin(t)
  
  df <- data.frame(x, y)
  
  return(df)
}

# Function definition: opposite() ---------------------------------------------
opposite <- function(df){
  
  # Given a dataframe, simply returns the dataframe in reverse order
  
  n <- nrow(df)
  df <- df[n:1,]
  return(df)
}

# Function definition: chordShape() -------------------------------------------
chordShape <- function(ID){
  
  # Function to create svg path for a chord given by a unique ID (created earler)
  
  id <- which(angle.list$ID == ID)
  
  # Get color based on higher number of connects
  idx <- which.max(angle.list$value[id])
  fillcolor <- angle.list$from[id[idx]]
  fillcolor <- cols[which(rownames(df) == fillcolor)]
  
  # Append the two prior points to complete polygon
  id <- c(id, id - 1)
  t <- angle.list$angle[id]
  
  # Each chord is made of two bezier curves and two (one) curve lying on the 
  # inner boundary of the ideogram
  if(length(t) == 4){
    a <- bezierCurve(t[1], t[4])
    b <- bezierCurve(t[3], t[2])
    c <- circleCurve(t[1], t[3])
    d <- circleCurve(t[2], t[4])
    
    df <- rbind(a, d, opposite(b), c)
    
    pth <- createPath(df)
    shp <- list(type = "path",
                path = pth,
                fillcolor = fillcolor,
                line = list(color = linecolor, width = 1),
                opacity = chord.opacity)
    
  }else{
    
    # Case when there are zero interactions i.e. 
    # A -> B > 0 but B -> A = 0 or viceversa
    a <- bezierCurve(t[1], t[2])
    b <- circleCurve(t[1], t[2])
    
    df <- rbind(a, b)
    
    pth <- createPath(df)
    shp <- list(type = "path",
                path = pth,
                fillcolor = fillcolor,
                line = list(color = linecolor, width = 1),
                opacity = chord.opacity)
  }
  
  return(shp)
}

# Loop through each unique ID and create a shape for each corrosponding polygon
chord.shapes <- list()
for(i in unique(angle.list$ID)){
  if(i != 0){
    chord.shapes[[i]] <- chordShape(ID = i)
  }
}

# Create a grey circle on the inside for aesthetics
ang <- seq(0, (2*pi), length.out = 100)
x <- 1 * cos(ang)
y <- 1 * sin(ang)

pth <- createPath(df = data.frame(x, y))
inner.circle <- list(list(type = "path",
                          path = pth,
                          fillcolor = circlefill,
                          line = list(color = linecolor, width = 1),
                          opacity = 0.2))

# Add all shapes to same list
all.shapes <- c(ideogram.shapes, chord.shapes, inner.circle)
length(all.shapes)

# Plot chord diagram ----------------------------------------------------------
# Just a description of chord diagram
description <- paste0("<i>","A chord diagram is a graphical method of displaying the inter-relationships ",
                      "between data in a matrix. The data is <br> arranged radially around a circle ",
                      "with the relationships between the points typically drawn as arcs connecting ",
                      "the<br>data together - <b>Wikipedia</b>","</i>")

# Coordinates for labels
labels <- data.frame(x = 1.1 * cos(outer$theta - pi/5),
                     y = 1.1 * sin(outer$theta - pi/5),
                     text = paste0("<b>", rownames(df), "</b>"))

# Plot using plot_ly()
chord.plot <- plot_ly(width = 800, height = 800) %>%
  
  # Add labels to sectors
  add_text(data = labels, x = ~x, y = ~y, text = ~text, hoverinfo = "none",
           textfont = list(family = "serif", size = 14, color = "#999999")) %>%
  
  # Layout for shapes, annotations and axis options
  layout(
    xaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F, domain = c(0, 0.9)),
    yaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F, domain = c(0, 0.9)),
    shapes = all.shapes,
    
    annotations = list(
      list(xref = "paper", yref = "paper",
           xanchor = "left", yanchor = "top",
           x = 0, y = 1, showarrow = F,
           text = "<b>Filled Chord Diagram</b>",
           font = list(family = "serif", size = 25, color = "black")),
      
      list(xref = "paper", yref = "paper",
           xanchor = "left", yanchor = "top",
           x = 0, y = 0.95, showarrow = F,
           text = description,
           align = "left",
           font = list(family = "arial", size = 10, color = "black"))
    ))

print(chord.plot)
1
2
3
4
5
6
7
8
9
10
11
12
13
14
15
16
17
18
19
20
21
22
23
24
25
26
27
28
29
30
31
32
33
34
35
36
37
38
39
40
41
42
43
44
45
46
47
48
49
50
51
52
53
54
55
56
57
58
59
60
61
62
63
64
65
66
67
68
69
70
71
72
73
74
75
76
77
78
79
80
81
82
83
84
85
86
87
88
89
90
91
92
93
94
95
96
97
98
99
100
101
102
103
104
105
106
107
108
109
110
111
112
113
114
115
116
117
118
119
120
121
122
123
124
125
126
127
128
129
130
131
132
133
134
135
136
137
138
139
140
141
142
143
144
145
146
147
148
149
150
151
152
153
154
155
156
157
158
159
160
161
162
163
164
165
166
167
168
169
170
171
172
173
174
175
176
177
178
179
180
181
182
183
184
185
186
187
188
189
190
191
192
193
194
195
196
197
198
199
200
201
202
203
204
205
206
207
208
209
210
211
212
213
214
215
216
217
218
219
220
221
222
223
224
225
226
227
228
229
230
# Create chords ---------------------------------------------------------------
# Divide each sector corresponding to each interaction in each row
sector.angles <- inner$gaps
angle.list <- data.frame()
for (i in 1:nrow(sector.angles)) {
  # Get starting and ending points of each sector
  start <- sector.angles$start[i]
  end <- sector.angles$end[i]
  
  # Sort each row from increasing to decreasing
  dat <- sort(df[i,])
  
  # Use toAngular() function to get thetas corresponding to each row item
  angle <- toAngular(as.numeric(dat), lower = start, upper = end, addgaps = F)$theta
  
  # Offset by the starting point since the function returns values in 
  # the [0 - (start - end)] interval
  angle <- c(start + angle)
  
  # Collate all the data for each division of the sector 
  temp <- data.frame(from = rownames(sector.angles)[i],
                     to = names(dat),
                     value = as.numeric(dat),
                     angle,
                     x = inner.radius * cos(angle),
                     y = inner.radius * sin(angle),
                     stringsAsFactors = F)
  
  # Add the starting point to the divisions
  # If min value in a row is zero then starting point for that division
  # must be the starting point of the sector
  startrow <- data.frame(from = rownames(sector.angles)[i],
                         to = "start",
                         value = 0,
                         angle = sector.angles$start[i],
                         x = inner.radius * cos(sector.angles$start[i]),
                         y = inner.radius * sin(sector.angles$start[i]),
                         stringsAsFactors = F)
  
  angle.list <- rbind(angle.list, startrow, temp)
}

# Create unique path IDs i.e. each set of interactions gets a unique ID
# Example - A -> B and B -> A will get the same ID
k <- 1
angle.list$ID <- rep(0, nrow(angle.list))
revstr <- paste(angle.list$to, angle.list$from)

for (i in 1:nrow(angle.list)) {
  if (angle.list$ID[i] == 0) {
    from = angle.list$from[i]
    to = angle.list$to[i]
    str <- paste(from, to)
    mtch <- match(str, revstr)
    
    if (!is.na(mtch)) {
      angle.list$ID[c(i, mtch)] <- k
      k <- k + 1
    }
  }
}

# Each chord is bounded by four points: 
# 1. two actual data points corrosponding to the actual interaction i.e. A -> B (p1) and B -> A (p2)
# 2. And two previous data points to complete the polygon
# We'll create some helper functions

# Function definition: bezierCurve() ------------------------------------------
bezierCurve <- function(t1, t2){
  
  # Takes two angles as arguments and returns the x and y coordinates
  # of a quadratic bezier curve 
  
  t <- seq(0, 1, length.out = 100)
  
  p0 <- c(inner.radius * cos(t1), inner.radius * sin(t1))  # Starting point (t1)
  p2 <- c(inner.radius * cos(t2), inner.radius * sin(t2))  # Ending point (t2)
  p1 <- c(-inner.radius * cos(mean(t1, t2)), -inner.radius * sin(mean(t1, t2)))  # Control point
  
  # Curve =  (1 - t^2)*p0 + 2(t-1)t*p1 + t^2*p2
  x <- (1 - t**2) * p0[1] + 2*(1 - t)*t * p1[1] + t**2 * p2[1]
  y <- (1 - t**2) * p0[2] + 2*(1 - t)*t * p1[2] + t**2 * p2[2]
  df <- data.frame(x, y)
  
  return(df)
}

# Function definition: circleCurve() ------------------------------------------
circleCurve <- function(t1, t2){
  
  # Returns the x and y coordinates of points lying on the inner 
  # boundary of the ideogram bounded by two angles t1 and t2 
  
  t <- seq(min(t1, t2), max(t1, t2), length.out = 50)
  x <- inner.radius * cos(t)
  y <- inner.radius * sin(t)
  
  df <- data.frame(x, y)
  
  return(df)
}

# Function definition: opposite() ---------------------------------------------
opposite <- function(df){
  
  # Given a dataframe, simply returns the dataframe in reverse order
  
  n <- nrow(df)
  df <- df[n:1,]
  return(df)
}

# Function definition: chordShape() -------------------------------------------
chordShape <- function(ID){
  
  # Function to create svg path for a chord given by a unique ID (created earler)
  
  id <- which(angle.list$ID == ID)
  
  # Get color based on higher number of connects
  idx <- which.max(angle.list$value[id])
  fillcolor <- angle.list$from[id[idx]]
  fillcolor <- cols[which(rownames(df) == fillcolor)]
  
  # Append the two prior points to complete polygon
  id <- c(id, id - 1)
  t <- angle.list$angle[id]
  
  # Each chord is made of two bezier curves and two (one) curve lying on the 
  # inner boundary of the ideogram
  if(length(t) == 4){
    a <- bezierCurve(t[1], t[4])
    b <- bezierCurve(t[3], t[2])
    c <- circleCurve(t[1], t[3])
    d <- circleCurve(t[2], t[4])
    
    df <- rbind(a, d, opposite(b), c)
    
    pth <- createPath(df)
    shp <- list(type = "path",
                path = pth,
                fillcolor = fillcolor,
                line = list(color = linecolor, width = 1),
                opacity = chord.opacity)
    
  }else{
    
    # Case when there are zero interactions i.e. 
    # A -> B > 0 but B -> A = 0 or viceversa
    a <- bezierCurve(t[1], t[2])
    b <- circleCurve(t[1], t[2])
    
    df <- rbind(a, b)
    
    pth <- createPath(df)
    shp <- list(type = "path",
                path = pth,
                fillcolor = fillcolor,
                line = list(color = linecolor, width = 1),
                opacity = chord.opacity)
  }
  
  return(shp)
}

# Loop through each unique ID and create a shape for each corrosponding polygon
chord.shapes <- list()
for(i in unique(angle.list$ID)){
  if(i != 0){
    chord.shapes[[i]] <- chordShape(ID = i)
  }
}

# Create a grey circle on the inside for aesthetics
ang <- seq(0, (2*pi), length.out = 100)
x <- 1 * cos(ang)
y <- 1 * sin(ang)

pth <- createPath(df = data.frame(x, y))
inner.circle <- list(list(type = "path",
                          path = pth,
                          fillcolor = circlefill,
                          line = list(color = linecolor, width = 1),
                          opacity = 0.2))

# Add all shapes to same list
all.shapes <- c(ideogram.shapes, chord.shapes, inner.circle)
length(all.shapes)

# Plot chord diagram ----------------------------------------------------------
# Just a description of chord diagram
description <- paste0("<i>","A chord diagram is a graphical method of displaying the inter-relationships ",
                      "between data in a matrix. The data is <br> arranged radially around a circle ",
                      "with the relationships between the points typically drawn as arcs connecting ",
                      "the<br>data together - <b>Wikipedia</b>","</i>")

# Coordinates for labels
labels <- data.frame(x = 1.1 * cos(outer$theta - pi/5),
                     y = 1.1 * sin(outer$theta - pi/5),
                     text = paste0("<b>", rownames(df), "</b>"))

# Plot using plot_ly()
chord.plot <- plot_ly(width = 800, height = 800) %>%
  
  # Add labels to sectors
  add_text(data = labels, x = ~x, y = ~y, text = ~text, hoverinfo = "none",
           textfont = list(family = "serif", size = 14, color = "#999999")) %>%
  
  # Layout for shapes, annotations and axis options
  layout(
    xaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F, domain = c(0, 0.9)),
    yaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F, domain = c(0, 0.9)),
    shapes = all.shapes,
    
    annotations = list(
      list(xref = "paper", yref = "paper",
           xanchor = "left", yanchor = "top",
           x = 0, y = 1, showarrow = F,
           text = "<b>Filled Chord Diagram</b>",
           font = list(family = "serif", size = 25, color = "black")),
      
      list(xref = "paper", yref = "paper",
           xanchor = "left", yanchor = "top",
           x = 0, y = 0.95, showarrow = F,
           text = description,
           align = "left",
           font = list(family = "arial", size = 10, color = "black"))
    ))

print(chord.plot)
You should now have something like this:
  
  
  
  I am sure there are more efficient ways of going about this but hopefully you found this post helpful. Here are some additional resources to look at:
  
  Plotly Python documentaion
Chord Diagram Wikipedia
Mike Bostock’s Block
Nice tutorial on Visual Cinnamon
Stack Overflow question
4

RIDDHIMAN
TAGS: chord diagram, filled chord diagram, Plotly, R, svg path
POST NAVIGATION
Previous Post
VISUALIZE TESLA SUPERCHARGING STATIONS WITH MYSQL AND PLOTLY
Next Post
INTERACTIVE VOLCANO PLOTS IN R WITH PLOTLY
SEARCH FOR:
  Enter keyword and hit enter
RECENT POSTS
Hosting Dash Apps on IBM Cloud
Weather maps in Python with Mapbox-gl, xarray, and netcdf4
Plotly December Update
County-Level Choropleth in Plotly and R
7 Interactive Bioinformatics Plots made in Python and R
BUSINESS INTELLIGENCE
Funnel Chart
Lego Mini-Series
Plotly December Update
All OSS used in Plotly Cloud and Plotly On-Premise
News and Updates Surrounding plotly for R
Native support for candlestick charts in Plotly and R
Plotcon May 2017 – Speakers and Topics
BLOG ROLL
R-Bloggers

Share to Facebook
Share to Twitter
Share to Print
Share to Email
More AddThis Share options
SHARES
Sumo
