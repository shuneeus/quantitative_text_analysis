# Quantitative Analysis of Political Texts {#qta}


## Part 1: Hashtag analysis 


In the study of contentious politics, #olafeminista, #metoo, #blacklivesmatter and #niunamenos are hashtags that went viral and tell a rich story of the social media activism and protest. This is a case study a protest cycle called Ola Feminista (Feminist Wave), which occurred in Chile from May to June of 2018. The feminist protest cycle denounced structural gender inequalities and started as a mobilization of students in Santiago, and gradually grew expanding to broader demands from feminist and worker organizations across Chile.

### Descriptive statistics

In the first half of this chapter, I show how use basic descriptive statistics to understand how deputies makers use Twitter. We will analyze how deputies in Chile made use of gender related hashtags during the #olafeminista (feminist wave). We will analize simple frequency variations in the usage of hashtags to adrresss different levels of engagement with the online debate around gender and feminist issues.  

In this chapter, I use an original dataset with identification variables for the deputies, such as name and last name, district, political party, age, among others.  The identification variables were extracted from the [official web page of the House of Representatives (*Cámara de Diputados*)](https://www.camara.cl/camara/deputys.aspx#tab). For the data extraction from Twitter I used the `rtweet` package, which allows us to freely access Twitter’s API for downloading information by users, dates and hashtags (see Chapter \@ref(web-mining)).

## Analysis of political hashtags {#sqta1}
