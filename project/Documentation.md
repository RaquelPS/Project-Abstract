
This Shiny App is for searching, visualizating and extracting useful information about the Portuguese Vinho Verde wine variety. Our aim was to create a way for the user to understand this kind of wine and help them choose the correct variant to combine with their food by means of a predictive model (decision tree).

The dataset used is related to red and white variants of the Portuguese "Vinho Verde" wine. The total number of observations is 6497 and from those 6031 had the complete necessary information in order to develop the app. Note that the proportion of white wines is much higher that the red wines one.

In the first tab we can see a table containing the information chosen by the user, as well as a summary that helps us understand the units and ranges of each variable.

In the data visualization tab, we dispayed some plots representing the distribution of each variant of wine, in order for the user to have an idea of the taste and the quality proportions of the wines in general. Regarding the properties of these wines, we ploted a boxplot (which could be seen as a complement to the summary previously done), a bar plot showing the distribution of each property (differentiating between red and white variants), and a correlation plot that shows how usually the amount of each property makes another one's amount grow bigger/smaller or stay the same.

Last but not least, we implemented a predictive model that would give out the best variant of wine that should be chosen according to some important properties. This is essential when deciding for a wine to combine with the food.

Data Source: https://archive.ics.uci.edu/ml/datasets/Wine+Quality

P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis. 
Modeling wine preferences by data mining from physicochemical properties. In Decision Support Systems, Elsevier, 47(4):547-553, 2009.

Note: The dataset is actually loaded from http://halweb.uc3m.es/esp/Personal/personas/imolina/esp/perso.html, since it had additional information regarding the taste of the wine.

Date updated: 29/03/2019

About author: Raquel Parra Suazo and Shenbin Zheng, students of the master in Statistics for Data Science in the University Carlos III of Madrid.
