# deveoping_data_products
Presentation for data app

**Original reference text file has not been inclicluded in this package for confidencial reasons**

This demo app has a simple objective: to expose the user to some simple text mining techniques, while allowing them to interact with the tools for three common Text Mining analyses: Word counting, Tree clustering and Kmeans for word proximity.

As a reference, the input from a survey of over two thousand entries was used, and both the client and participants remain confidential. Participants were asked to rate the service they received from a telecommunications company, for one month period, and their answer were classified as positive (Promoters) and negative (Detractors).

Consequently, to analyse the feedback, this app splits the input in two tabs: Promoters and Detractors (which can be selected from the top menu). Under each tab there are three sliders allowing for the following interaction:

1. Bar chart with word count across all feedback: Choose lower percentage threshold of word usage across all feedback (e.g. words used on at least 50 % of all feedback).

2. Dendogram of feedback’s natural clustering: Choose max allowed sparsity threshold (i.e. resulting cluster contains only terms with a sparse factor of less than sparse value)

3. Verbatim of clusters by word proximity (K means): Choose number of words per cluster, so as to better understand narrative.

**Original reference text file has not been inclicluded in this package for confidencial reasons**