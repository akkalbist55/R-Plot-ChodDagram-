library(chorddiag)

students = data.frame(Math = c(50, 25, 5, 12),
                      Art = c(10, 55, 5, 20),
                      Science = c(45,12,29, 20),
                      PE = c(24,67,27,15))

students = as.matrix(students)
row.names(students) = c("Section A", "Section B", "Section C", "Section D")

chorddiag(students, type = "bipartite", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)

