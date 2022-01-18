setwd("C:/Users/luisa/Desktop/LM_II_year/Advanced_Social_Networks/EXAM_PROJECT/")

# install.packages("sna")
library("sna")


# 1.1.
# Draw the network using the Fruchterman-Reingold layout with names 
# (but without attributes). 
# Add the R script of how you obtained the drawings.

bullying <- as.matrix(read.table("EXERCISE011_bullying.csv", sep=';', header=T,
                             stringsAsFactors=FALSE, row.names=1))

# dt_TF <- (bullying>0)
dt_TF <- as.network(bullying, directed=TRUE)

net1 <- gplot(dt_TF,
              gmode="graph",
              mode="fruchtermanreingold",
              jitter=F,
              edge.col="grey70", 
              edge.lwd=.1,
              vertex.col="gold3",
              vertex.cex=.75,
              # labels
              displaylabels=T,
              label.pos=1,
              label.cex=.7)

# 1.2.
# Next draw the network again using the Fruchterman-Reingold layout 
# without names, but now with all 3 attributes 
# (gender, grade and ethnicity) included using in some way a combination 
# of color, shape and size of nodes. 
# Add the R script of how you obtained the drawings. 

attributes <- read.table("EXERCISE011_attributes.csv", sep=';', header=T,
                         stringsAsFactors=FALSE, row.names=1)

unique(attributes$Grade) # 4 8 6 5 7 9 3

g <- (attributes$Grade)
colfunc <- colorRampPalette(c("yellow", "green4"))
COLWB <- colfunc(7)

g[g==3]<-COLWB[1]
g[g==4]<-COLWB[2]
g[g==5]<-COLWB[3]
g[g==6]<-COLWB[4]
g[g==7]<-COLWB[5]
g[g==8]<-COLWB[6]
g[g==9]<-COLWB[7]


par(mar=c(0,0,0,0))
gplot(dt_TF, # bullying
      gmode="graph",
      mode="fruchtermanreingold",
      jitter=F,
      edge.col="grey70",
      edge.lwd=.2, 
      # attributes
      vertex.col=g, # color by grade
      vertex.cex=1+.7*(2-attributes$Ethnicity), # size by ethnicity
      vertex.sides=(-attributes$Gender+1)*47+4) # shape by gender


# 1.3.
# Create a legend for the attributes (this does not have to be with R, 
# but needs to clarify how the different attributes were represented in 
# the network). 

legend("topleft",
       legend = c("Colors:", "Lowest Grade: 3", "Grade 4", "Grade 5", "Grade 6", "Grade 7", "Grade 8", "Highest Grade: 9", " ","Shapes:", "Female", "Male", "", "Node size by Ethinicity value"),
       col = c("white", COLWB[1], COLWB[2], COLWB[3], COLWB[4], COLWB[5], COLWB[6], COLWB[7], "white", "white", "black", "black", "white", "white"), 
       bty = "n", 
       pch = c(0, 19, 19, 19, 19, 19, 19, 19, 0, 0, 19, 18, 0, 0), # 18 for the diamond
       pt.cex = c(0, 2.1, 2.1, 2.1, 2.1, 2.1, 2.1, 2.1, 0, 0, 1.7, 1.9, 0, 0), 
       cex = 0.8, 
       text.col = "grey10", 
       horiz = F, 
       inset = c(0.01))

# In the network, the diamond shape represents female students while the circle
# represents males. The nodes are colored with different shades of yellow and 
# green according to their grade. 
# The color goes from yellow, which means the node has the lowest grade among all 
# nodes in the network, namely 3, through different shades of green, until it 
# reaches the darker green color, which highlights the highest mark among those 
# found, which is 9. 
# Even if the possible grades are expressed in a scale from 0 to 10, 
# the extremes are never found in the given network, and the data report 
# grades from 3 to 9. 
# Finally, the vertexes are of different dimensions according to the ethnicity
# value that was given to each individual. The biggest nodes are the one with
# ethnicity value equal to 1, which means they belong to the first generation
# of immigrants (they are born abroad with foreign parents and now live in Italy); 
# the medium size nodes belong to the second generation of immigrants 
# (they are born in Italy with foreign parents), and finally the smallest nodes 
# represent people born in Italy with at least one Italian parent.


# 2.1.
# Calculate indegree and outdegree centrality for each node and 
# interpret your results. 

# Provide the indegree and outdegree centrality score for each node.
# indegree:
degree(dt_TF, g=1, nodes=NULL, gmode="digraph", diag=FALSE,
       tmaxdev=FALSE, cmode="indegree", rescale=FALSE, ignore.eval=FALSE)
# outdegree:
degree(dt_TF, g=1, nodes=NULL, gmode="digraph", diag=FALSE,
       tmaxdev=FALSE, cmode="outdegree", rescale=FALSE, ignore.eval=FALSE)

# Who is the most central when it comes to indegree?
max(degree(dt_TF, g=1, nodes=NULL, gmode="digraph", diag=FALSE,
           tmaxdev=FALSE, cmode="indegree", rescale=FALSE, ignore.eval=FALSE))

colSums(bullying)
colSums(bullying)==max(colSums(bullying)) # Marco

# Provide details of the meaning of the value 
# (taking into account that the network is "who do you bully?").

# This means that Marco is bullied by the greatest number of people,
# namely 9 different people.

# Make sure to discuss the reference points for the indegree measure. 
# How does it compare to the theoretical minimum and maximum value?

# The maximum value for the indegree correspond to n-1, where n is
# the number of nodes in the network. This would mean that any other node
# in the network bullies the node with indegree equal to n-1.
# In this case, the theoretical maximum indegree possible is 25-1=24, 
# but the maximum indegree found is just 9.
# The minimum indegree possible is zero, which would mean that the node 
# is on a separate component on their own, isolated from the rest of
# the network. In the specific case of this network, this situation
# would mean that the considered person is not bullied by anyone.

# Who is the most central when it comes to outdegree?
max(degree(dt_TF, g=1, nodes=NULL, gmode="digraph", diag=FALSE,
           tmaxdev=FALSE, cmode="outdegree", rescale=FALSE, ignore.eval=FALSE))

rowSums(bullying)
rowSums(bullying)==max(rowSums(bullying)) # Francesco

# Provide details of the meaning of the value 
# (taking into account that the network is "who do you bully?").

# This means Francesco is the person who is bullying the higher number
# of people (10). 

# Make sure to discuss the reference points for the outdegree measure. 
# How does it compare to the theoretical minimum and maximum value?

# The maximum value in outdegree is still n-1, which would mean that the node
# having this value bullies all other people in the network. The actual
# highest outdegree found is lower than the theoretical maximum, which in
# the specific case of this network is very unlikely as it is unusual that,
# taken a class of students, one of them bullies all of the others. 
# One of the reason why this is unlikely is because in such a
# situation, the person who bullies everyone  
# would probably be isolated from the rest of the group as they are only 
# impacting negatively everyone else, which would lead to a lack in social 
# capital and social support.
# The minimum outdegree is zero, which would mean a person is not bullying
# anyone. In this specific network, Roberto is the one who is not bullying
# anyone (outdegree equal to zero), and at the same time is not 
# being bullied by anyone (indegree equal to zero).


# 2.2. 
# Let's assume we hypothesize that the grade of students might be 
# impacted by how many others they are being bullied by. Calculate 
# the correlation between the indegree and the grade of the kid. 

cor(degree(dt_TF, g=1, nodes=NULL, gmode="digraph", diag=FALSE,
                tmaxdev=FALSE, cmode="indegree", rescale=FALSE, ignore.eval=FALSE),
         attributes$Grade)

# Pearson correlation with significance
cor.test(degree(dt_TF, g=1, nodes=NULL, gmode="digraph", diag=FALSE,
                tmaxdev=FALSE, cmode="indegree", rescale=FALSE, ignore.eval=FALSE),
         attributes$Grade)

# What is the correlation value? What does it mean substantively?
# Provide the R script of how you obtain the results.

# The correlation is 0.8537182, which is very close to perfect 
# positive correlation. Positive correlation means that when one of the 
# two variables varies (increases or decreases), the other moves in the
# same direction. In this case, for example, this means that the higher
# the number of people you are being bullied by, the higher your grade.
# The p-value is way lower than 5% (p-value=5.767e-08), 
# which means we should reject the null hypothesis of no correlation 
# and assume grade and indegree are highly correlated.


# Finally, perform a significance test using a permutation-based 
# approach for the correlation as discussed in class. 
# What does the significance test do and what do you conclude?
# Add the R script of how you obtained the results.

# The permutation-based approach is based on the idea of
# "alternative worlds" idea. This means that, given a network,
# one would like to reassign nodes randomly, while keeping 
# the same edges (edges stays in the same position).
# This is called permutation test.
# This is performed in order to understand what is the chance, 
# considered all alternative worlds, that one would get the correlation
# value that was actually obtained.
# The permutation-based approach also allows to test the significance.

# Permutation with sampling:
# Random sample without replacement.
# This would lead to different results each time the command "sample" is ran, 
# as no seed is given.
attr_grade <- attributes$Grade
sample(attr_grade, replace=F)

# Set seed for reproducibility
set.seed(42)

# Permutation approach: if we shuffle the data 1000 times,
# how often would we obtain a result as extreme as 
# the one we observed in the original network (or more extreme)?

OUTPUT <- matrix(NA, 1000, 1) # matrix 1000 rows filled with NAs

for (k in c(1:1000)) {
  permutation.test <- sample(attr_grade, replace=F)
  OUTPUT[k, 1] <- cor(degree(dt_TF, g=1, nodes=NULL, gmode="digraph", diag=FALSE,
                             tmaxdev=FALSE, cmode="indegree", rescale=FALSE, ignore.eval=FALSE),
                      permutation.test)
}

OUTPUT # correlation value for each "alternative world" considered

par(mar=c(3,3,3,3))
hist(OUTPUT, main='histogram') 

# original correlation in our data:
(corRealValue <- cor(degree(dt_TF, g=1, nodes=NULL, gmode="digraph", diag=FALSE,
                            tmaxdev=FALSE, cmode="indegree", rescale=FALSE, ignore.eval=FALSE),
                     attributes$Grade))

# results as extreme or more extremes than the actual value 
# (on one and the other tail)
sum(OUTPUT>=abs(corRealValue))/1000 
sum(OUTPUT<=-abs(corRealValue))/1000
# values as/more extreme on both tails
sum(OUTPUT>=abs(corRealValue))/1000 + sum(OUTPUT<=-abs(corRealValue))/1000


# We may assume normality of the distribution and proceed as follows
hist(OUTPUT, nclass=20, prob=T, main='Permutation distribution') 
# Extremes for the 95% confidence interval:
mean(OUTPUT)+sd(OUTPUT)*1.96
mean(OUTPUT)-sd(OUTPUT)*1.96

x <- seq(min(OUTPUT), max(OUTPUT), length = 40)

curve(dnorm(x, mean=mean(OUTPUT), sd=sd(OUTPUT)), 
      col="darkblue", lwd=2, add=TRUE)

# abline(v=cor(degree(dt_TF, g=1, nodes=NULL, gmode="digraph", diag=FALSE,
#                    tmaxdev=FALSE, cmode="indegree", rescale=FALSE, ignore.eval=FALSE),
#             attributes$Grade), lwd=3, col="red")

abline(v=corRealValue, lwd=3, col="red") 

# The real correlation value is around 0.85
# This value is really extreme and outside the range shown 
# on the x axis of the histogram, thus the red vertical line is not visible.
# One could possibly see that max(OUTPUT) returns ~0.67, which is
# lower than the correlation found in the network.

# In the following histogram the red vertical line representing the actual 
# correlation value will be visible, and one can easily notice how extreme this
# value is.

hist(OUTPUT, nclass=20, prob=T, main='Permutation distribution', xlim = c(-1, 1))
abline(v=corRealValue, lwd=3, col="red")

# This means that the our correlation is much higher than the one 
# found in other 1000 configurations created by random sampling. 
# No permuted network had value as extreme or more extreme,
# thus we can assume that the result coming from the original network
# is hardly explained by chance.

# We may also estimate the p-value as follows.
p_value <- (sum(OUTPUT>=corRealValue)+1)/length(OUTPUT)
p_value # 0.001

# Here +1 at the nominator was added to bound the value
# so that the p-value is never zero as there is always some chance that 
# our test statistic can be measured for some permutation.
# Without this addition, the value of p_value would have been zero, as easily
# understandable from the histogram.
# Indeed, in the histogram, there was not even one case of a permutation 
# with a correlation higher than or equal to the one observed, 
# which confirms the low p-value.
# Since the p-value is lower than 5%, we can affirm this result is significant,
# however it would be better to perform the permutation approach with at least
# 10000 permutations. This can be slower, however, in this case,
# attempting to increase the number of permutations would lead to the same 
# conclusion, as it is possible to see by running the code below.

OUTPUT <- matrix(NA, 10000, 1) 

for (k in c(1:10000)) {
  permutation.test <- sample(attr_grade, replace=F)
  OUTPUT[k, 1] <- cor(degree(dt_TF, g=1, nodes=NULL, gmode="digraph", diag=FALSE,
                             tmaxdev=FALSE, cmode="indegree", rescale=FALSE, ignore.eval=FALSE),
                      permutation.test)
}

OUTPUT
hist(OUTPUT, main='Permutation distribution (ran 10.000 times)') 
abline(v=corRealValue, lwd=3, col="red")
# p-value:
p_value_2 <- (sum(OUTPUT>=corRealValue)+1)/length(OUTPUT)
p_value_2 # 1e-04


# 3.1. 
# Calculate the indegree and outdegree centralization for this network 
# using the approach discussed in class and interpret. 
# Make sure to show how you came to these answers. 
# What do you conclude substantively (taking into account that the network 
# is "who do you bully?")?
# Make sure to discuss the reference points for the measure. 
# What is the minimum and maximum value?

# indegree centralization 
centralization(dt_TF, degree, mode="digraph", cmode="indegree", normalize=TRUE)
# outdegree centralization 
centralization(dt_TF, degree, mode="digraph", cmode="outdegree", normalize=TRUE)

# The centralization of a network measures how central its most central node is 
# with respect to how central all other nodes are.
# The indegree centralization identify how much an activity (in this case bullying) 
# is performed by a single entity. A highly centralized network will be one 
# where only one or few nodes perform a certain activity (bullying).
# Instead, the outdegree centralization identify how much a single node is
# subjected to the bullying activity. A high outdegree centralization would
# mean only one or few node are very subjected to bullying by a lot of 
# different other students.

# Thus, a maximally centralized network result in a "star-shaped" graph
# and its centralization value is 1 which indicates that all the nodes in a network
# have ties to only one node and there are no other ties.

# All this considered, the theoretical maximum value of the indegree centralization 
# would be obtained if all nodes in the network are directly connected to
# just one node, meaning this one node has the maximum number of incoming edges 
# possible (indegree=n-1). This would mean that student is bullied by all of their
# classmates.
# Similarly, the maximum value for the outdegree centralization would be obtained
# if one node bullies all the others in the network, namely has outdegree equal 
# to n-1.

# Instead, if all node have the same in-/out-degree, 
# the network is decentralized, and the centralization will be equal to zero,
# which is the minimum value possible.

# From the results above, given that the theoretical maximum is one and the 
# minimum zero, it is possible to conclude the network is not
# very centralized, neither with respect to indegree (~0.262) nor to outdegree (~0.306).
# Both values characterize a weakly centralized and fairly cohesive graph.
# While the values are close to each other, students who bully are slightly 
# more centralized around specific nodes than students who are bullied.
# Note that the configuration of the network visible in the first two plots would have
# suggested the same conclusion.


# 3.2. 
# Calculate the arc-based reciprocity index for this network and interpret. 
# Make sure to show how you came to this answer.
# What do you conclude substantively (taking into account that the network 
# is "who do you bully?")?
# Make sure to discuss the reference points for the measure. 
# What is the minimum, expected and maximum value?

library("igraph")

# graph object to pass to the igraph::reciprocity command
g <- graph_from_adjacency_matrix(
  bullying,
  mode = c("directed"))

reciprocity(g) # 0.02702703

# Alternatively, using the "sna" library, the same result can be obtained by running:
# sna::grecip(dt_TF, measure = "edgewise")

# The arc-based reciprocity index is obtained by applying the following formula:
# 2*M / (2*M+A)
# where M are the mutual ties (e.g., if A bullies B, B bullies A),
# and A are the asymmetric ties (e.g., if A bullies B, A is not bullied by B).
# Thus one may also compute it manually as follow:

sna::dyad.census(dt_TF, g=NULL) # returns number of mutual, asymmetric, and null ties
# mutual = 1, asymmetric = 72, null = 227
reciprocity_index = 2*1 / (2*1 + 72)
reciprocity_index

# The minimum value possible of the reciprocity index
# is zero, which would mean perfect anti-reciprocity, thus if A connect to B, 
# B never connects to A. In the case of the given network, "connects" means
# "bullies". This would mean there is no mutual tie in the whole network
# (edges go only in one direction and never backwards).
# Instead, the maximum value possible is one, which means perfect reciprocity 
# (if A bullies B, then B bullies A, and if A don't bully B, then B don't bully
# A, for all A and B in the network).
# This means all ties are mutual.

# The obtained value of reciprocity is quite close to the theoretical minimum
# as it is around 0.027.
# This means that in the given class of students, if A bullies B, it is unlikely
# that B also bullies A.
