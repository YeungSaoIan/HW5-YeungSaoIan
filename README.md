長庚大學 大數據分析方法 作業五
================

作業說明 （繳交時請直接刪除這個章節）
-------------------------------------

作業目的：延續作業四，練習讀入檔案，並做基本的探索式資料分析

依下列指示，完成探索式資料分析作業：

-   下載並讀入[105各村里教育程度資料](http://data.moi.gov.tw/MoiOD/Data/DataContent.aspx?oid=1F69C3BD-C367-4216-8969-14FDC609B4B0) `5pt`

-   下載並讀入[10512村里戶數、單一年齡人口](http://data.moi.gov.tw/MoiOD/Data/DataContent.aspx?oid=EEC2F7DB-CD5B-4968-AB40-9BD17B86A8C2) `5pt`

-   分析議題1：各縣市各有多少個村里`10pt`？用村里數由大到小排序`5pt` ，討論結果`5pt` 。（group\_by, summarise, n, arrange）

-   分析議題2：各縣市各村里的平均人口數是多少（平均每個村里有多少人）`10pt`？用平均人口數由大到小排序`5pt` ，討論結果`5pt` 。（group\_by, summarise, mean, arrange）

-   分析議題3：哪個**縣市**的博班畢業（不分性別）人口最多`10pt`？用博班畢業人口數由大到小排序`5pt` ，討論結果`5pt` 。（group\_by, summarise, sum, mutate, arrange）

-   分析議題4：各**縣市**碩畢男女比例的差異
    -   利用**村里**資料加總來計算各**縣市**男女人口與碩畢男女人數資料 （group\_by, summarise）`10pt`
    -   使用上述總合後的資料，基於**縣市**欄位，結合教育程度與人口兩張表格 （inner\_join） `10pt`
    -   分別計算各縣市男女碩畢比例，依結果**討論**各縣市男女碩畢比例是否有差異，如有幾個縣市男生碩畢比例大於女生，是否有和地區有關等 `10pt`

讀入教育程度資料與人口數資料
----------------------------

``` r
#這是R Code Chunk
library(readr)
data1 <-read_csv("~/Desktop/HW5-YeungSaoIan/opendata105Y020.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
data2 <-read_csv("~/Desktop/HW5-YeungSaoIan/opendata10512M030.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character()
    ## )
    ## See spec(...) for full column specifications.

``` r
library(dplyr)
```

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

解釋解釋解釋 討論討論討論討論

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

解釋解釋解釋 討論討論討論討論

各縣市的博班畢業人口
--------------------

``` r
#這是R Code Chunk
villagephdanalyze = summarise(group_by(mix,site_id),villagenum = n(), villagephdnum = sum(as.numeric(edu_doctor_graduated_m) +as.numeric(edu_doctor_graduated_f)) )  
knitr::kable(
head(villagephdanalyze[order(villagephdanalyze$villagephdnum,decreasing = T),])
)
```

| site\_id     |  villagenum|  villagephdnum|
|:-------------|-----------:|--------------:|
| 臺北市大安區 |          53|           5677|
| 臺北市文山區 |          43|           3111|
| 新竹市東　區 |          53|           3051|
| 臺北市中正區 |          31|           2305|
| 臺南市東　區 |          45|           2142|
| 臺北市士林區 |          51|           2046|

解釋解釋解釋 討論討論討論討論

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

解釋解釋解釋 討論討論討論討論
