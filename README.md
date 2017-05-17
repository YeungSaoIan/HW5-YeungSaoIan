長庚大學 大數據分析方法 作業五
================

讀入教育程度資料與人口數資料
----------------------------

``` r
#這是R Code Chunk
library(readr)
```

    ## Warning: package 'readr' was built under R version 3.3.3

``` r
data1 <-read_csv("C:/bigdata/opendata105Y020.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
data2 <-read_csv("C:/bigdata/opendata10512M030.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character()
    ## )
    ## See spec(...) for full column specifications.

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.3.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
#結合教育程度與人口兩張表格
mix<-inner_join(data1,data2,by=c("site_id","village"="village"))
```

各縣市村里數
------------

``` r
#這是R Code Chunk
 villagenumanalyze = summarise(group_by(mix,site_id), villagenum = n())
knitr::kable(
head(villagenumanalyze[order(villagenumanalyze$villagenum,decreasing = T),])
)
```

| site\_id     |  villagenum|
|:-------------|-----------:|
| 新北市板橋區 |         126|
| 新北市三重區 |         119|
| 新北市中和區 |          93|
| 桃園市中壢區 |          85|
| 新北市新莊區 |          84|
| 屏東縣屏東市 |          79|

新北市板橋區的村里數最多,有126

各縣市每個村里平均有多少人
--------------------------

``` r
#這是R Code Chunk
#villagepopmeananalyze = summarise(group_by(villagenumanalyze,site_id),villagenum = n(), villagepopmean = as.numeric(villagenum)/as.numeric(data2$people_total))

villagepopmeananalyze = summarise(group_by(mix,site_id),villagenum = n(), villagepopnum = sum(as.numeric(people_total)), villagepopmean = villagepopnum/villagenum)
knitr::kable(
head(villagepopmeananalyze[order(villagepopmeananalyze$villagepopmean,decreasing = T),])
)
```

| site\_id     |  villagenum|  villagepopnum|  villagepopmean|
|:-------------|-----------:|--------------:|---------------:|
| 臺中市大里區 |          27|         210285|        7788.333|
| 臺北市內湖區 |          39|         287733|        7377.769|
| 臺中市潭子區 |          16|         107530|        6720.625|
| 臺中市南屯區 |          25|         166685|        6667.400|
| 臺中市北屯區 |          42|         270547|        6441.595|
| 臺北市文山區 |          43|         275231|        6400.721|

如圖所示: 臺中市大里區的村里平均人數最多 平均人數為7788.33

各縣市的博班畢業人口
--------------------

``` r
#這是R Code Chunk
villagephdanalyze = summarise(group_by(mix,site_id),villagenum = n(), villagephdnum = sum(as.numeric(edu_doctor_graduated_m) +as.numeric(edu_doctor_graduated_f)) )  
knitr::kable(
head(villagephdanalyze[order(villagephdanalyze$villagephdnum,decreasing = T),])
)
```

| site\_id         |      villagenum|  villagephdnum|
|:-----------------|---------------:|--------------:|
| 臺北市大安區     |              53|           5677|
| 臺北市文山區     |              43|           3111|
| 新竹市東　區     |              53|           3051|
| 臺北市中正區     |              31|           2305|
| 臺南市東　區     |              45|           2142|
| 臺北市士林區     |              51|           2046|
| 如圖所示:        |                |               |
| 臺北市大安區的博 |  班畢業人數最多|               |
| 共5677人         |                |               |

各縣市碩畢男女比例的差異
------------------------

``` r
#這是R Code Chunk
villagemasteranalyze = summarise(group_by(mix,site_id),villagenum = n(), villagemalemasterrate =sum(as.numeric(edu_master_graduated_m))/sum(as.numeric(people_total_m)), villagefemalemasterrate =sum(as.numeric(edu_master_graduated_f))/sum(as.numeric(people_total_f)), villagemasterrate = villagemalemasterrate + villagefemalemasterrate)  

knitr::kable(
  head(villagemasteranalyze[order(villagemasteranalyze$villagemasterrate,decreasing = T),])
)
```

| site\_id     |  villagenum|  villagemalemasterrate|  villagefemalemasterrate|  villagemasterrate|
|:-------------|-----------:|----------------------:|------------------------:|------------------:|
| 臺北市大安區 |          53|              0.1363096|                0.0966188|          0.2329285|
| 新竹市東　區 |          53|              0.1315828|                0.0717392|          0.2033220|
| 臺北市中正區 |          31|              0.1171388|                0.0838256|          0.2009644|
| 新竹縣竹北市 |          30|              0.1305302|                0.0641867|          0.1947169|
| 臺北市松山區 |          33|              0.1157290|                0.0749549|          0.1906839|
| 臺北市文山區 |          43|              0.1031229|                0.0681446|          0.1712675|

``` r
#結合教育程度與人口兩張表格
mix<-inner_join(data1,data2,by=c("site_id","village"="village"))
```

如圖所示: 臺北市大安區的碩畢比例最高 男生碩畢比例為13.63% 女生碩畢比例為9.66%

綜合統計資料: 北部的博碩畢業比例最高,以臺北市大安區最為明顯 其次是臺中市

由此推斷城市人口愈多愈繁榮,博碩畢業比例愈高.
