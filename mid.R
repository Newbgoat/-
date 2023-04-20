sales=data.frame("fruit"=c("사과","딸기","수박"),"price"=c(1800,1500,3000),"volume"=c(24,38,13))
str(sales)
mean(sales$price)
sd(sales$price)
un_4 <- mpg %>% filter(displ<=4)
ov_5 <- mpg %>% filter(displ>=5)
mean(un_4$hwy)
mean(ov_5$hwy)

mpg %>% select(class,cty) %>% head()
mpg %>% filter(manufacturer=="audi") %>% arrange(desc(hwy)) %>% head(5)



mpg <- mpg %>% mutate(total=cty+hwy)
mpg

mpg %>% group_by(class) %>% summarise()

boxplot(mpg$cty)

quantile(mpg$cty,c(0.25,0.5,0.75))

iris
ggplot(iris)+geom_point(aes(x=Petal.Length,y=Petal.Width))

ggplot(iris,aes(x=Petal.Length,y=Petal.Width))+geom_smooth(method = "lm")+geom_point()

ggplot(iris,aes(x=Petal.Length,y=Petal.Width,col=Species))+geom_smooth(method = "lm")+geom_point()

ggplot(iris,aes(x=Species,y=Petal.Width))+geom_bar(stat = "identity")+coord_flip()


gen_otp_url="http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd"
gen_otp_data=list(mktId="STK",
                  trdDd="20230419",
                  money="1",
                  csvxls_isNo="false",
                  name="fileDown",
                  url="dbms/MDC/STAT/standard/MDCSTAT03901")

otp=POST(gen_otp_url,query=gen_otp_data) %>% read_html() %>% html_text()
down_url="http://data.krx.co.kr/comm/fileDn/download_csv/download.cmd"
down=POST(down_url,query=list(code=otp),add_headers(referer=gen_otp_url)) %>% 
  read_html(encoding = 'EUC-KR') %>% html_text() %>% read_csv()
print(down)
