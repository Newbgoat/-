news <- read_csv("Yoon.csv")

news_raws <-  news  %>% mutate(news=str_replace_all(news,"[^가-힣]"," "),news=str_squish(news))

#단어 형태로 형태소 분석
news_raw<- news_raws  %>% unnest_tokens(input = news,output=word,token=extractNoun)

#한 글자제외 하고 모든 단어들 빈도수 측정
freq <- news_raw %>% count(언론사,word) %>%  filter(str_count(word)>1)

#윤석열 대통령에 관한 내용이기에 윤석열과 대통령이라는 단어를 제외
#KBS에서 주진우라는 단어가 거의 모든 뉴스에 포함되어 제외
#상위 10개 단어 추출
top_10 <- freq %>%filter(!word %in%c("윤석열","대통령","주진우","때문"))%>%  group_by(언론사) %>% 
  slice_max(n,n=10,with_ties = F)

#상위 10개의 단어에 대해서 각각의 언론사 별로 시각화
ggplot(top_10,aes(x=reorder(word,n),y=n,fill=언론사))+geom_col()+coord_flip()+facet_wrap(~언론사,scales ="free_y")+
  scale_x_reordered()+labs(x=NULL)

#tf_idf로 측정
#KBS에서만 주진우라는 단어가 거의 모든 뉴스에 포함되어 제외
tf<- freq %>% filter(word!="주진우") %>%  bind_tf_idf(term = word,document = 언론사,n=n) %>% arrange(-tf_idf)
tf %>% filter(언론사=="KBS")
tf %>% filter(언론사=="mbc")
tf %>% filter(언론사=="경향")
tf %>% filter(언론사=="중앙")


#tf_idf 기준으로 상위 10개 단어 추출
top10 <- tf %>% group_by(언론사) %>% slice_max(tf_idf,n=10,with_ties = F)

#tf_idf 기준으로 상위 10개 단어 시각화
ggplot(top10,aes(x=reorder_within(word,tf_idf,언론사),y=tf_idf,fill=언론사))+geom_col(show.legend = F)+coord_flip()+
  facet_wrap(~언론사,scales = "free",ncol = 2)+scale_x_reordered()+labs(x=NULL)

#감정사전
dic <- read_csv("knu_sentiment_lexicon.csv")

#단어로 토큰화
word_news <- news_raws %>% unnest_tokens(input=news,output = word,token="words",drop = F)

word_news %>% select (word,news)

#감정점수 측정
word_news <- word_news %>% left_join(dic,by="word") %>% mutate(polarity=ifelse(is.na(polarity),0,polarity))

#감정점수 분류
word_news <- word_news %>% mutate(sentiment=ifelse(polarity==2,"pos",ifelse(polarity==-2,"neg","nue")))
word_news %>% count(sentiment)

#감정점수 시각화
top10_sentiment <- word_news %>% filter(sentiment!="nue") %>% count(sentiment,word) %>% group_by(sentiment) %>% slice_max(n,n=10)
ggplot(top10_sentiment,aes(x=reorder(word,n),y=n,fill=sentiment))+geom_col()+coord_flip()+geom_text(aes(label=n),hjust=-0.3)+facet_wrap(~sentiment,scales = "free")+
  scale_y_continuous(expand = expansion(mult=c(0.05,0.15)))+labs(x=NULL)

#기사별 감정점수 측정
score_comment <-  word_news %>% group_by(news) %>% summarise(score=sum(polarity)) %>% ungroup()
score_comment

#긍정적인 기사
score_comment %>% select(score,news) %>% arrange(-score)

#부정적인 기사
score_comment %>% select(score,news) %>% arrange(score)

#긍정 부정 여부
score_comment <- score_comment %>% mutate(sentiment= ifelse(score>=1,"pos",ifelse(score<=-1,"neg","neu")))

#긍/부정 비율
freq_score <- score_comment %>% count(sentiment) %>% mutate(ratio=n/sum(n)*100)
freq_score

#비율 시각화
ggplot(freq_score,aes(x=sentiment,y=n,fill=sentiment))+geom_col()+geom_text(aes(label=n),vjust=-0.3)+scale_x_discrete(limits=c("pos","neu","neg"))
