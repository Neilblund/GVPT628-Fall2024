library(lsa)

glove <- read.csv(unzip('glove.6B.zip', 'glove.6B.100d.txt'), row.names=1, sep=' ', quote="",
                  header=FALSE
)|>
  as.matrix()

vec<-glove['king',  ] - glove['man',] +  glove['queen',]

nearest<-cosine(vec, t(glove))

sort(nearest, decreasing=T)[1:10]
