Trolling identified based on profanity, sentiment, and second person pronoun. The dataset was seperated into those three categories.

Files named Commentsxx.xml are individual communities with information whether the comments has words from profanity list. 

Files named Classifier-xx.csv are individual communities with information of sentiment, the las two coloums are sentiment label and the confidence of the sentiment.

The file named "comment_data.xlxm" contains all communities used for analysis, with all information about profanity, sentiment and second person pronoun.

The file named "trolltable.csv" is the cleaned dataset with all network matrics (ego level as well as community level): density, degree (including outdegree and indegree),betweenness, closeness, eigenvalue centrality, modularity, weak and strong components. 
Also, user activities and community level activities related to trolls are recorded, including: whether the user replyed to a troll before, whether the other community members replyed to trolls before, whether a user's connections replyed to trolls before.
