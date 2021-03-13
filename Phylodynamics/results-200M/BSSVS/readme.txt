03/13/2020

Three best runs were used ESS values > 200
Run1 and run4 were not used
Burnin 60000000 (30%) 
Run 2 3, 5
An empirical tree set was created of 10500 trees ( subampling every 40000 states) 

--------------------
Discrete trait analysis preformed on empirical set of trees for following schema:
Cor1 - mp1,mp2, mp3
Cor2 - mp1,mp2
EWYK - East, West, Yakima, King
County - counties, counties with less than 10 sequences were combined with adjacent counties. 
Following counties grouped into "Eastern" : 
Jefferson, GraysHarbor, Mason, Clark, Cowlitz, Thurston
Following counties grouped into "Western" :
Asotin, Adams, Benton, Umatilla, Grant, Kittitas

Three independent runs of 10 million chainlenght were preformed for each schema, transition rates were calculated from combined log file created in log combiner, using an in-house python script. Bayes factor support was calculated using SPREAD3. 

