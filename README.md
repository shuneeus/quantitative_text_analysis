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

hash_tf_idf <- poltweets_hashtags %>%   # calculate tf-idf:
               count(coalition, hashtag, fem_hashtag, sort = T) %>% 
               bind_tf_idf(term = hashtag, document = coalition, n = n) %>% 
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



### Temporal variation in hashtag use

Certain hashtags may increase or decrease in its use through time, depending on the political context. We will explore the weekly frequency of two most frequent hashtags in our example. Using the `lubridate` package, which works with data in a date format, we can look for time trends. In our dataset we have one variables with a date: `created_at`. Using this variable, we can confirm there was a peak in tweets between the 27th of May and the 2nd of June. 

```r
hashtags_weekly <- poltweets_hashtags %>% 
                   mutate(week = floor_date(created_at, "week", week_start = 1)) %>% 
                   filter(hashtag %in% c("#aborto3causales", 
                                         "#leydeidentidaddegeneroahora")) %>% 
                   count(hashtag, week)

ggplot(data    = hashtags_weekly,
       mapping = aes(x = week, y = n, 
       linetype = hashtag, group = hashtag)) +
       geom_point() +
       geom_line() +
       labs(x = "Week", y = "Total weekly use", linetype = "Hashtag")

```


<p align="center">
  <img src="https://github.com/shuneeus/text_mining/blob/master/Images/plot3.jpg" width="500" title="hover text">
</p>




### To sum up

Hashtags can tell you a lot about a political debate. We could verify some evident differences in the use of gender "#". Congresswomen used far more hashtags such as #olafeminista (#feministwave) and #agendamujer (#womenagenda) than their male counterparts. Regarding the coalitions, the left-wing ones (Frente Amplio and La Fuerza de la Mayoría) used them more. Regarding the temporal variation, the greater intensity of mentions on gender issues occurred during the week of the 14-20th of May, the week before the Public Account Speech (21st of May), which also fitted with a manifestation in various cities of the country. We observe that, in relative terms, congresswomen were almost five times more interested in the feminist movement, since they used the hashtag #agendamujer 5 times more than their male counterparts during the week of the 14-20th of May.

What did you learn in this section? We showed you how to use Twitter to analyzing political phenomena. Once you have your own dataset you can follow our step by step analysis. This would be useful as your starting point for explanatory designs that inquire on the causes of political alignment in different agendas.




## Part 2: Natural Language Processing

### Wordfish algorithm  

In this section I show how to implement a NLP technique commonly used in political science for unsupervised text mining: Wordfish. This text processing models allow us to summarize a lot of different documents in a fast and economical way and can complement other descriptive measures like word and hashtags frequencies. As an unsupervised technique, the classifications will is done without using any previous coding or dictionary. This has the advantage of saving work on manual coding, as well as avoiding the coder's own bias. Another advantage is that is not dependent on the source language, i.e. in principle they can be used in any language. This method use the "bag of words" approach, since the order of the words within a text does not alter the analysis. The parameters estimated by the algorithm can then be plotted with `ggplot2`, which facilitates the visual interpretation of the results. 

Wordfish is an algorithm that allows one-dimensional scaling of a set of texts. That is, to order in a one-dimensional axis the documents from how similar they are to each other in the use of certain keywords. The classification is carried out by establishing the frequency of word use. This modeling assumes that the number of times a word is said in a document follows a Poisson distribution. This model is extremely simple since the number of times a word will appear is estimated from a single parameter λ, which is both the mean and the variance of the Poisson probability distribution.


### Inspection and data cleaning 

We load again the `poltweets` dataset and notice now that it contains a set of variables which are necessary for text analysis. Now we will use the entire tweets, not just the tokens. We will also require a variables `status_id` necessary to match each tweet to who tweeted it.  


```r
library(quanteda) # dfm and corpus
library(quanteda.textmodels) # wordfish
library(qdapRegex) # remove non ascii characters

```
### Preprocessing

Before applying the algorithm, we must pre-process the texts. This means using regular expressions to make the text cleaner. We will use regular expressions to remove strange characters, usernames, URLs, emojis and switch everything to lowercase.

```{r}

#this function replaces "accents"
f_remove_accent <- function(x){
                               x %>% 
                                    str_replace_all("á", "a") %>% 
                                    str_replace_all("é", "e") %>% 
                                    str_replace_all("í", "i") %>% 
                                    str_replace_all("ó", "o") %>% 
                                    str_replace_all("ú", "u") %>% 
                                    str_replace_all("ñ", "n") # also replace "ñ", a common letter in Spanish
 }


poltweets <- poltweets %>% 
              mutate(text = text %>%
                       str_remove("\\@[[:alnum:]]+") %>% 
                       str_remove_all("http[\\w[:punct:]]+") %>% 
                       str_to_lower() %>%
                       str_remove_all("[\\d\\.,_\\@]+") %>% 
                       f_remove_accent() %>%
                       rm_non_ascii())
```

Once the text is clean, we want to group it according to the variable for comparison. As we are interested in obtaining the estimates at the coalition level, we group the texts by coalition. Now each coalition is a document in the dataset. When ordering by coalitions, you should place the factor levels in a way that it resembles a left-right axis: 

```{r}
by_coalition <- poltweets %>% 
                group_by(coalition) %>% 
                summarize(text = str_c(text, collapse = " ")) %>%
                ungroup() %>% 
                mutate(coalition = fct_relevel(as.factor(coalition), "FA", "LFM", "ChV"))
```

For modeling with Quanteda we transform the dataset first to Corpus format, and then to Document-feature Matrix (DFM) format. This means transforming each documents in rows and "features" as columns. We make the transformation of the dataset grouped by coalitions to Corpus format and then to DFM. In addition, we take advantage of using a command that will help eliminate numbers, punctuations, symbols and stopwords (conjuctions, articles, etc.):

```{r}
poltweets_corpus <- corpus(by_coalition)

poltweets_dfm <- dfm(poltweets_corpus,
                     remove_numbers = T, remove_punct = T, 
                     remove_symbols = T, remove = stopwords("spa"))
```

Using `dfm_trim()`, we eliminate words with frequency equal to or less than the 5th percentile and those words with a frequency equal or greater than the 95th percentile. In this way, we eliminate unusual words that are located at the extremes of the frequency distribution that can bias the results of the algorithm. 

```{r}
poltweets_dfm_trimmed <- dfm_trim(
                              poltweets_dfm, 
                              min_docfreq = 0.05, max_docfreq = 0.95, 
                              docfreq_type = "quantile" # min 5% / max 95%)
```

### Wordfish


We apply the Wordfish algorithm to the DFM class object, specific to Quanteda.We define the direction of parameter $\theta$ -the equivalent of $\beta$-, in this case that document 3 (FA) is the positive pole and document 1 (CHV) is the negative pole in the estimated ideological dimension. We also use the argument `sparse = T`, which allows working with large amounts of data, saving computational power.

                               
```{r}
wf <- textmodel_wordfish(poltweets_dfm_trimmed,
                         dir = c(3, 1), sparse = T)


df_wf <- tibble(
                coalition = wf[["x"]]@docvars[["coalition"]],
                theta = wf$theta, 
                lower = wf$theta - 1.96 * wf$se.theta, 
                upper = wf$theta + 1.96 * wf$se.theta)



ggplot(data    = df_wf,
       mapping = aes(x = theta, y = fct_reorder(coalition, theta),
                     xmin = lower, xmax = upper)) +
      geom_point() +
      geom_linerange() +
      # add vertical line at x=0:
      geom_vline(xintercept = 0, linetype = "dashed") +
      scale_x_continuous(limits = c(-1.2, 1.2)) +
      labs(y = "")

```

We see that coalitions are grouped along a left-right divide. The interest parameter $\theta$, equivalent to the beta parameter, is the parameter that discriminates the positions of the documents from the word frequencies. We see that this parameter is consistent with how coalitions are grouped politically. The rightmost one, Chile Vamos (ChV), with a $\theta$ of 1.07, is located at one end of the X axis, on the contrary, the leftmost one, Frente Amplio (FA), with a $\theta$ of -0.91, is located at the opposite end. 
                    
<p align="center">
  <img src="https://github.com/shuneeus/text_mining/blob/master/Images/plot4.jpg" width="500" title="hover text">
</p>




### Structural Topic Model algorithm

Topic modeling is a computational method for automatically identifying relevant word groupings in large volumes of texts. One of the most popular applications in political science is the Latent Dirichlet Allocation (LDA), developed by David Blei and explained in a didactic way at the [Machine Learning Summer School 2009 at Cambridge University](https://www.youtube.com/watch?v=DDq3OVp9dNA).  

Another useful development is the structural topic modeling (STM), a non supervised NLP technique for diving large corpora of texts. The main innovation of the STM is that it incorporates metadata into the topic model, so it allows researchers to discover topics and estimate their relationship to covariates, improving the quality of the inferences and the interpretability of the results. The STM algorithm is available in the `stm` package created by Molly Roberts, Brandon Stewart and Dustin Tingley. For a more detailed review of this method there is a bulk of material in the [official site of the package](http://www.structuraltopicmodel.com/).

In this section, we will analize a subset of our tweets to find the most relevant topics and see how they correlate to the gender and coalition variables. Following [Julia Silge's lead](https://juliasilge.com/blog/evaluating-stm/), we will first do all the preprocessing using tidy tools, to then feed a corrected dataset to `stm`.


### Pre-processing

We will only employ tweets from May 2018:

``` r 
library(tidyverse)
library(tidytext)
library(stm)
library(quanteda)
library(qdapRegex)

poltweets_onemonth <- poltweets %>% 
                      filter(created_at >= "2018-05-01" & created_at < "2018-06-01")
```

As mentioned above, we should start by pre-processing the texts. Remember that in the previous subsection we removed strange characters from the text. Next we will create a tokenized version of `poltweets_onemonth`, where every row is a word contained in the original tweet, plus a column with the total number of times that each word is said in the entire dataset (we only keep words that are mentioned ten or more times). Right after doing that, we will we remove stopwords (conjuctions, articles, etc.) using the `stopwords` package. Notice that we will also employ a "custom" dictionary of stopwords, composed by the unique names and surnames of deputies.

``` r 
# obtain unique names and surnames of deputies
names_surnames <- c(poltweets$names, poltweets$lastname) %>% 
                    na.omit() %>% 
                    unique() %>% 
                    str_to_lower() %>% 
                    f_remove_accent() %>% 
                    str_split(" ") %>% 
                    flatten_chr()

poltweets_words <- poltweets_onemonth %>% 
                    unnest_tokens(word, text, "words") %>% 
                    # remove stop words:
                    filter(!word %in% stopwords::stopwords("es", "stopwords-iso")) %>% 
                    # remove names/surnames of deputies:
                    filter(!word %in% names_surnames) %>% 
                    # just keep words that are present ten or more times
                    add_count(word) %>% 
                    filter(n > 10)
```

That's it in term of pre-processing! Next we will transform the tokenized dataset into a stm object using the `cast_dfm()` and `convert()` functions. 

``` r
poltweets_stm <- poltweets_words %>% 
                 cast_dfm(status_id, word, n) %>% 
                 convert(to = "stm")
```

In order to estimate the relation of the topics and the document covariates, we must add the covariate values into the `poltweets_stm$meta` object. The `metadata` object is a dataframe containing the metadata for every document in the stm object thatn can later be used as the document "prevalence"--or metadata. Notice that for creating the stm_meta object, it is necessary to join by the status_id variable, a column containing a unique identifier for every tweet.  


``` r
metadata <- tibble(status_id = names(poltweets_stm$documents)) %>% 
            left_join(distinct(poltweets, status_id, coalition, gender), by = "status_id") %>%
            as.data.frame()     
           
poltweets_stm$meta <- metadata
```

Now we have all the necessary ingredients to estimate our structural topic model, stored in the `poltweets_stm` object:


### Diagnostics

To estimate a `stm`, one needs to define the number of topics ($K$) beforehand. However, there is no "right" number of topics, and the appropiate $K$ should be decided looking at the data itself [@robertsStmPackageStructural2019]. In order to do that, we should train several models and compute diagnostics that will help us decide. What range of $K$ should we consider? In the package manual [@R-stm], the authors offer the following advice: 

> For short corpora focused on very specific subject matter (such as survey experiments) 3-10 topics is a useful starting range. For small corpora (a few hundred to a few thousand) 5-50 topics is a good place to start. Beyond these rough guidelinesit is application specific. Previous applications in political science with medium sized corpora (10kto 100k documents) have found 60-100 topics to work well. For larger corpora 100 topics is a useful default size. (p. 61)

Our dataset has 5,647 documents, and therefore we will try 5-50 topics. We can use the `searchK()` function from the `stm` package to compute the relevant diagnostics, which we will store in the `stm_search` object. This process is computationally expensive, and might take several minutes on a modern computer. If you do not want to wait, you can load the object from the book's package (`data("stm_search")`) and keep going.


``` r
stm_search <- searchK(documents = poltweets_stm$documents,
                      vocab = poltweets_stm$vocab,
                      data = poltweets_stm$meta,
                      # our covariates, mentioned above:
                      prevalence = ~ coalition + gender,
                      # 5-50 topics range:
                      K = seq(5, 50, by = 5), 
                      # use all our available cores (be careful!):
                      cores = parallel::detectCores(),
                      # a seed to reproduce the analysis:
                      heldout.seed = 123)
```


```
