# Social Network Analysis with R
This repository stores the work done for the Advanced Social Networks course, offered by the University of Trento (a.y. 2021/2022).

## Overview
This exercise perform a small social network analysis on a class of Italian student, studying bullying phenomena, and evaluating nodes attributes such as gender, grade, and ethinicity.
We were provided with an Excel file, containing the directed network (Bullying sheet) capturing who bullies whom among a group of students in an Italian school class. 
The network was based on asking every person the question “Who do you bully?”.
You are also given a spreadsheet with attributes for each student (Attributes sheet). This included the attributes:
-	The student’s gender (0=female, 1=male);
-	The student’s grade (which goes from 0 to 10, with 10 being the highest grade);
-	The student’s ethnicity, where 1=born abroad with foreign parents (first generation immigrants), 2=born in Italy with foreign parents (second generation immigrants), 3=born in Italy with at least one Italian parent (which we label “Italian”).

## Data
Both the data for the network and the attributes can be found in the `data` folder.

## Task
### Exercise 1 
Drawing a network using R:

- Draw the network using the Fruchterman-Reingold layout with names (but without attributes). Add the R script of how you obtained the drawings. 
- Next draw the network again using the Fruchterman-Reingold layout without names, but now with all 3 attributes (gender, grade and ethnicity) included using in some way a combination of color, shape and size of nodes. Add the R script of how you obtained the drawings.
-	Create a legend for the attributes (this does not have to be with R, but needs to clarify how the different attributes were represented in the network). 
 
### Exercise 2
Centrality:

Calculate indegree and outdegree centrality for each node and interpret your results.
 - Provide the indegree and outdegree centrality score for each node.
 - Who is the most central when it comes to indegree? Provide details of the meaning of the value (taking into account that the network is “who do you bully?”). Make sure to discuss the reference points for the indegree measure. How does it compare to the theoretical minimum and maximum value?
- Who is the most central when it comes to outdegree? Provide details of the meaning of the value (taking into account that the network is “who do you bully?”). Make sure to discuss the reference points for the outdegree measure. How does it compare to the theoretical minimum and maximum value?

Let’s assume we hypothesize that the grade of students might be impacted by how many others they are being bullied by. Calculate the correlation between the indegree and the grade of the kid. 
- What is the correlation value?
- What does it mean substantively?
- Provide the R script of how you obtain the results.

Finally, perform a significance test using a permutation-based approach for the correlation as discussed in class. 
- What does the significance test do and what do you conclude?
- Add the R script of how you obtained the results.
 
### Exercise 3
Centralization and reciprocity:

Calculate the indegree and outdegree centralization for this network using the approach discussed in class and interpret. 
- Make sure to show how you came to these answers.
-	What do you conclude substantively (taking into account that the network is “who do you bully?”)?
- Make sure to discuss the reference points for the measure. What is the minimum and maximum value?

Calculate the arc-based reciprocity index for this network and interpret. 
- Make sure to show how you came to this answer.
- What do you conclude substantively (taking into account that the network is “who do you bully?”)?
- Make sure to discuss the reference points for the measure. What is the minimum, expected and maximum value?

## Analysis 
All the analysis were performed using R and can be found, along with detailed comments, in the `SNA_project.R` script.
