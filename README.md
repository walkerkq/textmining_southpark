# Text Mining South Park    
South Park follows four fourth grade boys (Stan, Kyle, Cartman and Kenny) and an extensive ensemble cast of recurring characters. 
This analysis reviews their speech to determine which words and phrases are distinct for each character. 
Since the series uses a lot of running gags, common phrases should be easy to find.  
  
The programming language R and packages tm, RWeka and stringr were used to read South Park episode transcripts 
from a repository, attribute them to a certain character, break them into ngrams, calculate the log likelihood for 
each ngram/character pair, and rank them to create a list of most characteristic words/phrases for each character. 
The results were visualized using ggplot2, wordcloud and RColorBrewer.  
  
### Data    
Complete transcripts (70,000 lines amounting to 5.5 MB) were downloaded from 
[BobAdamsEE's github repository SouthParkData](https://github.com/BobAdamsEE/SouthParkData) 
from the original source at the [South Park Wikia page](http://southpark.wikia.com/wiki/Portal:Scripts).  

### Log Likelihood  
Each corpus was analyzed to determine the most characteristic words for each speaker. 
Frequent and characteristic words are not the same thing - otherwise words like "I", "school", and "you" would 
rise to the top instead of unique words and phrases like "professor chaos", "hippies" and "you killed kenny." 

Log likelihood was used to measure the unique-ness of the ngrams by character. Log likelihood compares the 
occurrence of a word in a particular corpus (the body of a character's speech) to its occurrence in another corpus 
(all of the remaining South Park text) to determine if it shows up more or less likely that expected. The returned 
value represents the likelihood that the corpora are from the same, larger corpus, similar to a t-test.  

[Read the full report](https://github.com/walkerkq/textmining_southpark/blob/master/southpark_loglikelihood.pdf)
