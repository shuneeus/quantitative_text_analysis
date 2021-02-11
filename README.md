# Twitter Analysis

This readme file contains a brief description of an analysis I performed of Twitter data from Chilean deputies during the year 2018. 
The full version of this analysis was published in a chapter of quantitative text analysis in the book [R for Political Data Science](https://www.taylorfrancis.com/chapters/quantitative-analysis-political-texts-sebasti%C3%A1n-huneeus/e/10.1201/9781003010623-13).



## Part 1: Exploratory analysis of hashtag use 

In the study of contemporary social movements, hashtags like #metoo, #blacklivesmatter and #niunamenos can tell a rich story on how a protest evolves. Hashtags contain valuable semantic information of the tone, spread or speed of a protest. The protest cycle Ola Feminista (Feminist Wave), occurred in Chile from May to June of 2018, denounced gender inequalities and started as a mobilization of students in Santiago, and gradually grew expanding to broader demands from feminist and worker organizations across Chile.

I was curious about how deputies related or resonated with the protests, so I downloaded all the tweets from May to June of 2018 of all the deputies with an active Twitter account. Then I kept all the tweets containing at least one hashtag. 


```r
library(tidyverse)
library(tidytext)
library(politicalds)
data("poltweets")
```

After we loaded the dataset and took a quick view to the size and variables included, we must extract the hashtags from the tweets using the `unnest_tokens()` function of `tidytext`, creating a tokenized data frame with one row per hashtag. We then just filter all the rows starting with a hashtag (#), leaving us with a one-hashtag-per-row data frame.

```r
poltweets_hashtags <- poltweets %>% 
  unnest_tokens(output = "hashtag", input = "text", token = "tweets") %>%
  filter(str_starts(hashtag, "#"))
```


We want to see the differences in how representatives, parties and coalitions engage in the gender political debate. To do so, we create a new dummy variable that takes value "1" each time the character string variable matches any of the regular expresions like "femi", "niunamenos", "aborto", "mujer" and "genero":

```r
poltweets_hashtags <- poltweets_hashtags %>%
  mutate(fem_hashtag = case_when(str_detect(hashtag, "femi") ~ 1, 
                                 str_detect(hashtag, "niunamenos") ~ 1, 
                                 str_detect(hashtag, "aborto") ~ 1,
                                 str_detect(hashtag, "mujer") ~ 1,
                                 str_detect(hashtag, "genero")~ 1,
                                 TRUE ~ 0)) %>% 
  mutate(fem_hashtag = as.character(fem_hashtag))
```

### Wordclouds by groups

Using the `facet_wrap()` function, wordclouds can be split by variables of interest. Classifiying by gender and coalition, we immediately see how hashtags such as olafeminista (#feministwave), #agendamujer (#womenagenda) and #educacionnosexista (#sexisteducation) appear only among congresswomen Twitter accounts. When faceting by coalitions, we realize that the tweets from the Frente Amplio (FA) use a high proportion of gender related hashtags, whereas the oficialist coalition Chile Vamos (ChV) uses no hashtag at all. 

```r
library(ggwordcloud)

ggplot(data_hashtags_wordcloud, 
       aes(label = hashtag, size = n, color = fem_hashtag)) + 
  geom_text_wordcloud() +
  scale_size_area(max_size = 8) + # we set a maximum size for the text 
  theme_void()


ggplot(poltweets_hashtags %>% 
         count(hashtag, gender, fem_hashtag) %>% 
         arrange(-n) %>% 
         group_by(gender) %>% 
         slice(1:20), 
       aes(label = hashtag, size = n, color = fem_hashtag)) + 
  geom_text_wordcloud() +
  scale_size_area(max_size = 6) + 
  facet_wrap(~gender)
```

<p align="center">
  <img src="https://github.com/shuneeus/text_mining/blob/master/Images/plot1.jpg" width="500" title="hover text">
</p>



### Barplots


Now we calculate and plot the statistic tf-idf, intended to measure how important a word is to a document in a collection of documents. This statistic is a combination of term frequency (tf) and the term’s inverse document frequency (idf), which decreases the weight for commonly used words and increases the weight for words that are not used very much in the entire collection of documents. We see that, when separating by groups, two hashtags with the highest statistic 
tf-idf in the Frente Amplio are gender related (#leydeidentidaddegeneroahora). 

```r

hash_tf_idf <- poltweets_hashtags %>%
  # calculate tf-idf:
  count(coalition, hashtag, fem_hashtag, sort = T) %>% 
  bind_tf_idf(term = hashtag, document = coalition, n = n) %>% 
  # get 10 most distinctive hashtags per coalition:
  arrange(-tf_idf) %>% 
  group_by(coalition) %>% 
  slice(1:10)



ggplot(data    = hash_tf_idf,
       mapping = aes(x = tf_idf,
                     y = reorder_within(hashtag, tf_idf, coalition), 
                     fill = fem_hashtag)) +
  geom_col() +
  labs(x = "tf_idf", y = "", fill = "Feminist Hashtag") +
  facet_wrap(~coalition, nrow = 3, scales = "free") +
  scale_y_reordered()

```


<p align="center">
  <img src="https://github.com/shuneeus/text_mining/blob/master/Images/plot2.jpg" width="500" title="hover text">
</p>





## Part 2: Natural Language Processing

### Wordfish algorithm  

In this section I show how to implement a NLP technique commonly used in political science for unsupervised text mining: Wordfish. This text processing models allow us to summarize a lot of different documents in a fast and economical way and can complement other descriptive measures like word and hashtags frequencies. As an unsupervised technique, the classifications will is done without using any previous coding or dictionary. This has the advantage of saving work on manual coding, as well as avoiding the coder's own bias. Another advantage is that is not dependent on the source language, i.e. in principle they can be used in any language. This method use the "bag of words" approach, since the order of the words within a text does not alter the analysis. The parameters estimated by the algorithm can then be plotted with `ggplot2`, which facilitates the visual interpretation of the results. 

Wordfish is an algorithm that allows one-dimensional scaling of a set of texts. That is, to order in a one-dimensional axis the documents from how similar they are to each other in the use of certain keywords. The classification is carried out by establishing the frequency of word use. This modeling assumes that the number of times a word is said in a document follows a Poisson distribution. This model is extremely simple since the number of times a word will appear is estimated from a single parameter λ, which is both the mean and the variance of the Poisson probability distribution.



```r

```



### Structural Topic Model algorithm

Topic modeling is a computational method for automatically identifying relevant word groupings in large volumes of texts. One of the most popular applications in political science is the Latent Dirichlet Allocation (LDA), developed by David Blei and explained in a didactic way at the [Machine Learning Summer School 2009 at Cambridge University](https://www.youtube.com/watch?v=DDq3OVp9dNA).  

Another useful development is the structural topic modeling (STM), a non supervised NLP technique for diving large corpora of texts. The main innovation of the STM is that it incorporates metadata into the topic model, so it allows researchers to discover topics and estimate their relationship to covariates, improving the quality of the inferences and the interpretability of the results. The STM algorithm is available in the `stm` package created by Molly Roberts, Brandon Stewart and Dustin Tingley. For a more detailed review of this method there is a bulk of material in the [official site of the package](http://www.structuraltopicmodel.com/).

In this section, we will analize a subset of our tweets to find the most relevant topics and see how they correlate to the gender and coalition variables. Following [Julia Silge's lead](https://juliasilge.com/blog/evaluating-stm/), we will first do all the preprocessing using tidy tools, to then feed a corrected dataset to `stm`.


```r
```
