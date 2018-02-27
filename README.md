### Problem Statement
The Department of Human Services of Arlington County serves multiple clients across different sectors each day. Some of the clients are the elderly. Some are homeless. Some need behavioral health support. Some of these clients intersect across the different services.
The Department would like to know how many unique individuals they are serving in a single day? What is the demographics across all these different functions. Currently, they are unable to answer this question as the data is stored across nine different data systems and there are no underlying linkage between them.

#### The Team
In the Summer of 2017, my fellow Data Science for Public Good Fellow, Sayali Phadke (Penn State Uni) and I along with our mentor Aaron Schroeder of Social Decision Analytics took a crack at linking the different systems starting from an initial 3. 

### What this Repository is about?
This repository is *NOT* a clone of the work we had done. The client data is highly confidential. This repository is a display some of the code work I have contributed along with some of our initial results.

### Our Objectives
Our objective is two fold.

*1*. Within each system, we hope to remove duplicated client data. Due to administrative errors, each system has individuals who have been wrongfully entered several times. We want to remove it first. 

*2*. Across systems, we hope to link individuals by using all available demographic indicators.

### Our Methods

Instead of the existing exact matching method deployed in the data systems, we will be using Probablistic Linkage based on Fellgi & Sunter's method.

### Code Snippets

#### /src/zarni




##### 


### Poster
The Poster in the repository is what we presented at the Summer 2017 Data Science Symposium hosted by the BioComplexity Institute of Virgina Tech in Ballson, Arlington.

### Results
The result sheet displays some of the initial results across the 3 systems we work. As you may notice, we have improved upon the existing deduplication system.   


