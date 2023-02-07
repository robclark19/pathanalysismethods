
library(DiagrammeR)
library(DiagrammeRsvg)
library(xml2)

# Basic digraph with four nodes

grViz("
  digraph flowchart {
    node [shape=circle, fontsize=14, color=lightblue, style=filled]
    a -> b
    b -> c
    c -> d
  }
")

# Linear models (Multiple regression)

grViz("
  digraph flowchart {
    node [shape=circle, fontsize=14, color=lightblue, style=filled]
    a -> d
    b -> d
    c -> d
  }
")

# Multivariate models (MANOVA)

grViz("
  digraph flowchart {
    node [shape=circle, fontsize=14, color=lightblue, style=filled]
    a -> d
    b -> d
    c -> d
  }
")

#### SEMs utility as an extension of GLM and MANOVA

# Indirect effects mean a node can be both a predictor and a response variable
# Here A indirectly affects B via D

grViz("
  digraph flowchart {
    node [shape=circle, fontsize=14, color=lightblue, style=filled]
    a -> c
    a -> d
    d -> b
    {rank=same; a b}
    a -> b [color=red, penwidth=1, style=solid, arrowhead=normal]
  }
")

# Chains of serial direct effects
# A Both directly and indirectly affects C and D through multiplying path coefficients

grViz("
  digraph flowchart {
    node [shape=circle, fontsize=14, color=lightblue, style=filled]
    a -> b
    b -> c
    c -> d
    {rank=same; a b c d}
    a -> c [color=black, penwidth=1, style=dotted, arrowhead=normal]
    a -> d [color=black, penwidth=1, style=dotted, arrowhead=normal]
  }
")

# SEM cannot resolve networks with cyclical parts

grViz("
  digraph flowchart {
    node [shape=circle, fontsize=14, color=lightblue, style=filled]
    a -> b
    b -> c
    c -> a 
    c -> d
    {rank=same; b c}
  }
")

# Reciprocal or bidirectional paths cause the same issue as cyclical diagrams

loop <- grViz("
  digraph flowchart {
    node [shape=circle, fontsize=14, color=lightblue, style=filled]
    a -> b
    b -> a
    a -> c 
    b -> d
    {rank=same; a b}
  }
") 

loop %>%
  export_svg() %>%
  read_xml() %>%
  write_xml("graph.svg")
