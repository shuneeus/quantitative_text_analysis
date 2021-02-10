# Quantitative Analysis of Political Texts {#qta}

This readme file contains a brief description of an analysis I performed of Twitter data from Chilean deputies during the year 2018. 
The full version of this analysis was published in a chapter of quantitative text analysis in the book [R for Political Data Science](https://www.taylorfrancis.com/chapters/quantitative-analysis-political-texts-sebasti%C3%A1n-huneeus/e/10.1201/9781003010623-13).

I downloaded all the Tweets and then carried a series of descriptive analysis and topic models.  



## Part 1: Hashtag analysis 

In the study of contemporary social movements, hashtags like #metoo, #blacklivesmatter and #niunamenos can tell a rich story on how a protest evolves. Hashtags contain valuable semantic information of the tone, spread or speed of a protest. The protest cycle Ola Feminista (Feminist Wave), occurred in Chile from May to June of 2018, denounced gender inequalities and started as a mobilization of students in Santiago, and gradually grew expanding to broader demands from feminist and worker organizations across Chile.

I was curious about how deputies related or resonated with the protests, so I downloaded all the tweets from May to June of 2018 of all the deputies with an active Twitter account. Then I kept all the tweets containing at least one hashtag. 

For downloading the tweets I used the package 



## Part 2: Wordfish  

In this section I show how to implement a NLP technique commonly used in political science for unsupervised text mining: Wordfish. This text processing models allow us to summarize a lot of different documents in a fast and economical way and can complement other descriptive measures like word and hashtags frequencies. As an unsupervised technique, the classifications will is done without using any previous coding or dictionary. This has the advantage of saving work on manual coding, as well as avoiding the coder's own bias. Another advantage is that is not dependent on the source language, i.e. in principle they can be used in any language. This method use the "bag of words" approach, since the order of the words within a text does not alter the analysis. The parameters estimated by the algorithm can then be plotted with `ggplot2`, which facilitates the visual interpretation of the results. 

Wordfish is an algorithm that allows one-dimensional scaling of a set of texts. That is, to order in a one-dimensional axis the documents from how similar they are to each other in the use of certain keywords. The classification is carried out by establishing the frequency of word use. This modeling assumes that the number of times a word is said in a document follows a Poisson distribution. This model is extremely simple since the number of times a word will appear is estimated from a single parameter λ, which is both the mean and the variance of the Poisson probability distribution.


## Part 3: Structural Topic Models 

Topic modeling is a computational method for automatically identifying relevant word groupings in large volumes of texts. One of the most popular applications in political science is the Latent Dirichlet Allocation (LDA), developed by David Blei and explained in a didactic way at the [Machine Learning Summer School 2009 at Cambridge University](https://www.youtube.com/watch?v=DDq3OVp9dNA).  

Another useful development is the structural topic modeling (STM), a non supervised NLP technique for diving large corpora of texts. The main innovation of the STM is that it incorporates metadata into the topic model, so it allows researchers to discover topics and estimate their relationship to covariates, improving the quality of the inferences and the interpretability of the results. The STM algorithm is available in the `stm` package created by Molly Roberts, Brandon Stewart and Dustin Tingley. For a more detailed review of this method there is a bulk of material in the [official site of the package](http://www.structuraltopicmodel.com/).

In this section, we will analize a subset of our tweets to find the most relevant topics and see how they correlate to the gender and coalition variables. Following [Julia Silge's lead](https://juliasilge.com/blog/evaluating-stm/), we will first do all the preprocessing using tidy tools, to then feed a corrected dataset to `stm`.
