##############################################################
# 期末報告要求
##############################################################
# 
# 每個人都需要繳交一份報告
# 報告需要滿足以下條件：
# 定義一個想用資料回答的問題
# 從開放資料中找到兩份不同來源的資料（不同單位）
# 將資料依照某種方法整合
# 做出簡單的視覺化
# 依據視覺化結果，回答你的問題
#
# 繳交以下的檔案（除了選擇性的之外，請包到一個.zip檔後上傳到ceiba）：
# 1.(選擇性) Proposal、討論成績等，請直接寄到我的信箱
# 2.你抓取的資料
# 3.電子報告(pdf / html，透過ceiba)：
#   報告本體：
#      - 說明你的問題
#      - 呈現你的圖
#      - 說明你的結果
#   備註欄：
#      - 說明你如何收集資料
#      - 說明你所作的加分動作
# 4.你的程式碼們（若用R收集資料，也附上你的程式碼）
#   老師會跑你的程式
# 5.你的程式會用到的資料（請不要讓老師重抓）
# 6. pvm.yml（確認你的程式都不會再改動以後，在R中輸入：pvm::export.packages()即可產生這個檔案。裡面會列出所有你所使用的R套件與版本
# 7. sessionInfo.txt的結果。在R中輸入：saveRDS(sessionInfo(), file = "sessionInfo.Rds")即可產生這個檔案。
#
# 有以下行為會額外加分:
# -用程式抓取、收集資料
# -自學 Rmarkdown 產生 html(附上.Rmd檔案)
# -提供 shiny code讓老師可以互動式的瀏覽你的結果(要會動！)
# -從半結構化、非結構化資料整理出結構化資料
# -空間資料物件的轉換、整合
# -其他你自己學到的技巧
#
##############################################################



##############################################################
# 一、研究方向
##############################################################

# 學生想了解，各鄉鎮市區級的個人(薪資)所得稅多寡
# 是與「當地人民的教育程度」有關，
# 還是是跟「當地的人口年齡結構」有關。

# 因此會抓取五個資料
# 1.103年度綜合所得稅各類所得金額各縣市鄉鎮村里統計表-縣市別
# 2.統計區15歲以上人口五歲年齡組與性別與婚姻狀況統計
# 3.各村里教育程度資料
# 4.二級發布區圖
# 5.各鄉鎮市區人口密度

# 希望利用上述這些資料，
# 來觀察各鄉鎮市區級的個人所得稅多寡，是不是會因為當地人民的教育程度、當地的人口年齡結構而有所不同。
# 前提假設1：男女性別差異不涵蓋在內，假設男女完全平等
# 前提假設2：婚姻狀態差異不涵蓋在內，假設婚姻狀態不影響地方所得稅多寡
# 前提假設3：所有人都沒有輟學的情況，肄業的終究會畢業

##############################################################



##############################################################
# 二、抓取資料
#（有用到部分網路爬蟲技術，屬於學生自學的部分）
#（也有整理非結構化資料）
##############################################################

rm(list= ls())

# (I) 現在要開始抓：103年度綜合所得稅各類所得金額各縣市鄉鎮村里統計表-縣市別

# step1. 在政府資料開放平台上，抓取關鍵字：「綜合所得稅各類所得金額各縣市鄉鎮村里統計表」
library(xml2)
x <- read_html("https://data.gov.tw/datasets/search?qs=ft%3A%E7%B6%9C%E5%90%88%E6%89%80%E5%BE%97%E7%A8%85%E5%90%84%E9%A1%9E%E6%89%80%E5%BE%97%E9%87%91%E9%A1%8D%E5%90%84%E7%B8%A3%E5%B8%82%E9%84%89%E9%8E%AE%E6%9D%91%E9%87%8C%E7%B5%B1%E8%A8%88%E8%A1%A8&order=agency%3Adesc")
y <- xml_find_all(x, "//header/h4/a")
x2 <-  read_html("https://data.gov.tw/datasets/search?qs=ft%3A%E7%B6%9C%E5%90%88%E6%89%80%E5%BE%97%E7%A8%85%E5%90%84%E9%A1%9E%E6%89%80%E5%BE%97%E9%87%91%E9%A1%8D%E5%90%84%E7%B8%A3%E5%B8%82%E9%84%89%E9%8E%AE%E6%9D%91%E9%87%8C%E7%B5%B1%E8%A8%88%E8%A1%A8&order=agency%3Adesc&page=1")
y2 <- xml_find_all(x2, "//header/h4/a")

for(i in 1:length(y2)){
  y[[15+i]] <- y2[[i]]
}

# step2. 將這22個縣市的網頁資料整理到tax.df.main之中
county <- substring(text = as.character(y),first = 52,last = 54)
url_nodeset <- substring(text = as.character(y),first = 10,last = 23)
tax.df.main <- data.frame(county = county,url = paste0("https://data.gov.tw",url_nodeset))

# step3. 將這22個縣市的csv檔下載網址整理好
tax.csv.xmlnode <- c()
for(i in 1:nrow(tax.df.main)){
  tax.csv.xmlnode[i] <- xml_find_all(read_html(as.character(tax.df.main$url[i])),"//*[@id='r2']/div[8]/div[2]/div/a")
}
# 很不巧，學生製作報告的期間，發現6/8這個網站上的資料存放形式有部分更改過，
# 因此至此之後就不能用同一個XPath來抓取資料了....
url <- c()
for(i in 1:length(tax.csv.xmlnode)){
  url[i] <- as.character(xml_attrs(tax.csv.xmlnode[[i]])[1])
}
  
# step4. 下載檔案
for(i in 1:length(url)){
  download.file(url[i], destfile = paste0(as.character(tax.df.main[i,1]),".csv"), mode = "wb")
}


##############################################################

# (II) 現在要開始抓：統計區15歲以上人口五歲年齡組與性別與婚姻狀況統計 & 二級發布區圖
# 已知 https://data.gov.tw/dataset/18642 此網站是下載的目標了

# step1. 將這22個縣市的下載網址整理好
pop.xmlnode <- c()
for(i in 1:22){
  pop.xmlnode[i] <- xml_find_all(read_html("https://data.gov.tw/dataset/18642"),paste0("//*[@id='r",i-1,"']/div[5]/div[2]/div/a"))
}

url2 <- c()
for(i in 1:22){
  url2[i] <- as.character(xml_attrs(pop.xmlnode[[i]])[1])
}

# step2. 下載檔案
pop.main <- c("宜蘭縣","基隆市","嘉義市","臺南市","彰化縣","苗栗縣","新竹市","連江縣","高雄市","嘉義縣","臺中市","雲林縣","屏東縣","新北市","桃園市","南投縣","新竹縣","臺東縣","臺北市","金門縣","花蓮縣","澎湖縣")
for(i in 1:22){
  download.file(url2[i], destfile = paste0(pop.main[i],".xml"), mode = "wb")
}

# 看似server有些問題
# URL 'https://segisws.moi.gov.tw/STATWSSTData/OpenService.asmx/GetStatSTDataForOpenCode?oCode=6E03CA29B955A854D8F52522E38D8C7051A1FBEE829C41DBC09B9B1454506F40784066447C59ACA8B23873BCCAF65274E03D116C7D8EACF3': status was '500 Internal Server Error'
# 改成自行下載資料

# step3. 另外再下載「二級發布區圖」
download.file("http://data.moi.gov.tw/MoiOD/System/DownloadFile.aspx?DATA=CDBF153A-A885-497D-975F-A2996A2E7638",destfile = "area.zip")
# 手動解壓縮

# step4. 讀取SHP檔
library(rgdal)
area <- rgdal::readOGR("G97_TW_U0202_2015.shp")


##############################################################

# (III) 現在要開始抓：各村里教育程度資料
# 已知 https://data.gov.tw/dataset/8409 此網站是下載的目標了

# step1. 下載檔案
edu.xmlnode <- xml_find_all(read_html("https://data.gov.tw/dataset/8409"),"//*[@id='r0']/div[12]/div[2]/div/a")
url3 <- as.character(xml_attrs(edu.xmlnode[[1]])[1])
download.file(url3[1], destfile = paste0("edu",".json"), mode = "wb")

# step2. 讀取JSON檔
library(jsonlite)
edu.main <- fromJSON(txt = "edu.json")


##############################################################



##############################################################
# 三、第一筆資料：綜合所得稅各類所得金額各縣市鄉鎮村里統計表
# 提供機關：財政部財政資訊中心
# 製作各鄉鎮市區個人所得稅收表ias.df
# 部分程式碼與老師在HW4 & 5所示範的一樣
##############################################################

# 讀取資料
filenames <- list.files(getwd(), pattern="*.csv")
ias <- lapply(filenames, read.csv)

# 將header 調成 FALSE，並再重讀一次
ias <- list()
for(i in 1:length(filenames)) {
  ias[[i]] <- read.csv(filenames[i], header = FALSE)
}

# 用「各類所得金額各縣市鄉鎮村里統計表」來篩選第一欄
count <- c()
for(i in 1:length(ias)) {
  . <- ias[[i]]
  .i <- grepl("各類所得金額各縣市鄉鎮村里統計表", .[[1]])
  count[i] <- sum(.i)
}
count

# 整理出的資料，還混雜了「合計」與「其他」
.i <- which(.i)
.dfs <- list()
for(i in 1:(length(.i) - 1)) { 
  .range <- (.i[i] + 2):(.i[i+1] - 1)
  .dfs[[i]] <- .[.range,]
}
i <- length(.i) 
.range <- (.i[i] + 2):(nrow(.))
.dfs[[i]] <- .[.range,]
.df <- do.call(rbind, .dfs)
head(.dfs, 50)

# 合併「鄉鎮」與「村里」
.x <- .df[[1]]
.i <- which(.x != "")
for(i in 1:(length(.i) - 1)) {
  .range <- (.i[i]):(.i[i+1] - 1)
  .x[.range] <- .x[.i[i]]
}
i <- length(.i)
.range <- (.i[i]):(length(.x))
.x[.range] <- .x[.i[i]]
.x

.df[[2]] <- paste(.x, .df[[2]])

# 加上縣市別
.city <- strsplit(as.character(.[1,1]), "縣市別")
.city <- .city[[1]][2]
.city <- strsplit(.city, ",")
.city <- .city[[1]][1]
.city <- substring(.city, 2, nchar(.city))
.df[[2]] <- paste(.city, .df[[2]])

# 將其他的縣市依此類推完成
ias.dfs <- list()
for(j in 1:length(ias)) {
  . <- ias[[j]]
  .i <- grepl("各類所得金額各縣市鄉鎮村里統計表", .[[1]])
  .i <- which(.i)
  .dfs <- list()
  
  for(i in seq_len(length(.i) - 1)) { 
    .range <- (.i[i] + 2):(.i[i+1] - 1)
    .dfs[[i]] <- .[.range,]
  }
  i <- length(.i) 
  .range <- (.i[i] + 2):(nrow(.))
  .dfs[[i]] <- .[.range,]
  .df <- do.call(rbind, .dfs)
  .x <- .df[[1]]
  .i <- which(.x != "")
  for(i in 1:(length(.i) - 1)) {
    .range <- (.i[i]):(.i[i+1] - 1)
    .x[.range] <- .x[.i[i]]
  }
  i <- length(.i)
  .range <- (.i[i]):(length(.x))
  .x[.range] <- .x[.i[i]]
  .x
  .df[[2]] <- paste(.x, .df[[2]])
  .city <- strsplit(as.character(.[1,1]), "縣市別")
  .city <- .city[[1]][2]
  .city <- strsplit(.city, ",")
  .city <- .city[[1]][1]
  .city <- substring(.city, 2, nchar(.city))
  .df[[2]] <- paste(.city, .df[[2]])
  ias.dfs[[j]] <- .df
}
ias.df <- do.call(rbind, ias.dfs)

# 補上標頭
colnames(ias.df) <- as.matrix(ias[[1]][2,])
DT::datatable(head(ias.df, 100))

# 只選取鄉鎮市區級的資料
ias.df[[2]] <- gsub("\\s", "", ias.df[[2]])
ias.df <- ias.df[substring(text = ias.df$村里 ,first = nchar(ias.df$村里)-1,last = nchar(ias.df$村里)) == "合計",]
ias.df[grep(pattern = "桃園縣",x = ias.df$村里),2] <- gsub(pattern = "桃園縣",replacement = "桃園市",x = ias.df[grep(pattern = "桃園縣",x = ias.df$村里),2])
ias.df[grep(pattern = "合計",x = ias.df$村里),2] <- gsub(pattern = "合計",replacement = "",x = ias.df[grep(pattern = "合計",x = ias.df$村里),2])
ias.df <- ias.df[substring(text = ias.df$村里 ,first = nchar(ias.df$村里)-1,last = nchar(ias.df$村里)) != "其他",]


##############################################################



##############################################################
# 四、第二筆資料：統計區15歲以上人口五歲年齡組與性別與婚姻狀況統計
# 提供機關：內政部統計處
# 附加資料：二級發布區圖 （提供機關：內政部統計處）
# 附加資料：各鄉鎮市區人口密度 （提供機關：內政部戶政司）
# 製作各鄉鎮市區人口年齡結構表pop.df 
##############################################################

# 讀取資料，並測試整理
draft <- read_xml("南投縣.xml")
d <- xml_find_all(draft, "//COLUMN_NAME")
d_col_name <- xml_text(d)
d_col_name

draft2 <- read_xml("嘉義市.xml")
dd <- xml_find_all(draft2, "//COLUMN_NAME")
dd_col_name <- xml_text(dd)
dd_col_name

draft3 <- read_xml("基隆市.xml")
ddd <- xml_find_all(draft3, "//COLUMN_NAME")
ddd_col_name <- xml_text(ddd)
ddd_col_name

# 看似每個XML檔，變數數量都相同，變數名稱也一致
# 讀取資料
filenames1 <- list.files(getwd(), pattern="*.xml")
pop <- lapply(filenames1, read_xml)

# 寫迴圈
pop_col_name <- xml_find_all(pop[[1]], "//COLUMN_NAME")
pop_col_name <- xml_text(pop_col_name)

pop_c1 <- list()
for(i in 1:146){
  pop_c1_node <- xml_find_all(pop[[1]], paste0("//",pop_col_name[i]))
  pop_c1[[i]] <- xml_text(pop_c1_node)
}
pop_c2 <- list()
for(i in 1:146){
  pop_c2_node <- xml_find_all(pop[[2]], paste0("//",pop_col_name[i]))
  pop_c2[[i]] <- xml_text(pop_c2_node)
}
pop_c3 <- list()
for(i in 1:146){
  pop_c3_node <- xml_find_all(pop[[3]], paste0("//",pop_col_name[i]))
  pop_c3[[i]] <- xml_text(pop_c3_node)
}
pop_c4 <- list()
for(i in 1:146){
  pop_c4_node <- xml_find_all(pop[[4]], paste0("//",pop_col_name[i]))
  pop_c4[[i]] <- xml_text(pop_c4_node)
}
pop_c5 <- list()
for(i in 1:146){
  pop_c5_node <- xml_find_all(pop[[5]], paste0("//",pop_col_name[i]))
  pop_c5[[i]] <- xml_text(pop_c5_node)
}
pop_c6 <- list()
for(i in 1:146){
  pop_c6_node <- xml_find_all(pop[[6]], paste0("//",pop_col_name[i]))
  pop_c6[[i]] <- xml_text(pop_c6_node)
}
pop_c7 <- list()
for(i in 1:146){
  pop_c7_node <- xml_find_all(pop[[7]], paste0("//",pop_col_name[i]))
  pop_c7[[i]] <- xml_text(pop_c7_node)
}
pop_c8 <- list()
for(i in 1:146){
  pop_c8_node <- xml_find_all(pop[[8]], paste0("//",pop_col_name[i]))
  pop_c8[[i]] <- xml_text(pop_c8_node)
}
pop_c9 <- list()
for(i in 1:146){
  pop_c9_node <- xml_find_all(pop[[9]], paste0("//",pop_col_name[i]))
  pop_c9[[i]] <- xml_text(pop_c9_node)
}
pop_c10 <- list()
for(i in 1:146){
  pop_c10_node <- xml_find_all(pop[[10]], paste0("//",pop_col_name[i]))
  pop_c10[[i]] <- xml_text(pop_c10_node)
}
pop_c11 <- list()
for(i in 1:146){
  pop_c11_node <- xml_find_all(pop[[11]], paste0("//",pop_col_name[i]))
  pop_c11[[i]] <- xml_text(pop_c11_node)
}
pop_c12 <- list()
for(i in 1:146){
  pop_c12_node <- xml_find_all(pop[[12]], paste0("//",pop_col_name[i]))
  pop_c12[[i]] <- xml_text(pop_c12_node)
}
pop_c13 <- list()
for(i in 1:146){
  pop_c13_node <- xml_find_all(pop[[13]], paste0("//",pop_col_name[i]))
  pop_c13[[i]] <- xml_text(pop_c13_node)
}
pop_c14 <- list()
for(i in 1:146){
  pop_c14_node <- xml_find_all(pop[[14]], paste0("//",pop_col_name[i]))
  pop_c14[[i]] <- xml_text(pop_c14_node)
}
pop_c15 <- list()
for(i in 1:146){
  pop_c15_node <- xml_find_all(pop[[15]], paste0("//",pop_col_name[i]))
  pop_c15[[i]] <- xml_text(pop_c15_node)
}
pop_c16 <- list()
for(i in 1:146){
  pop_c16_node <- xml_find_all(pop[[16]], paste0("//",pop_col_name[i]))
  pop_c16[[i]] <- xml_text(pop_c16_node)
}
pop_c17 <- list()
for(i in 1:146){
  pop_c17_node <- xml_find_all(pop[[17]], paste0("//",pop_col_name[i]))
  pop_c17[[i]] <- xml_text(pop_c17_node)
}
pop_c18 <- list()
for(i in 1:146){
  pop_c18_node <- xml_find_all(pop[[18]], paste0("//",pop_col_name[i]))
  pop_c18[[i]] <- xml_text(pop_c18_node)
}
pop_c19 <- list()
for(i in 1:146){
  pop_c19_node <- xml_find_all(pop[[19]], paste0("//",pop_col_name[i]))
  pop_c19[[i]] <- xml_text(pop_c19_node)
}
pop_c20 <- list()
for(i in 1:146){
  pop_c20_node <- xml_find_all(pop[[20]], paste0("//",pop_col_name[i]))
  pop_c20[[i]] <- xml_text(pop_c20_node)
}
pop_c21 <- list()
for(i in 1:146){
  pop_c21_node <- xml_find_all(pop[[21]], paste0("//",pop_col_name[i]))
  pop_c21[[i]] <- xml_text(pop_c21_node)
}
pop_c22 <- list()
for(i in 1:146){
  pop_c22_node <- xml_find_all(pop[[22]], paste0("//",pop_col_name[i]))
  pop_c22[[i]] <- xml_text(pop_c22_node)
}

# 整理每欄資訊
cols <- list()
for(i in 1:146){
  cols[[i]] <- c(pop_c1[[i]],pop_c2[[i]],pop_c3[[i]],pop_c4[[i]],pop_c5[[i]],pop_c6[[i]],pop_c7[[i]],pop_c8[[i]],pop_c9[[i]],pop_c10[[i]],pop_c11[[i]],pop_c12[[i]],pop_c13[[i]],pop_c14[[i]],pop_c15[[i]],pop_c16[[i]],pop_c17[[i]],pop_c18[[i]],pop_c19[[i]],pop_c20[[i]],pop_c21[[i]],pop_c22[[i]])
}

# 建立data.frame
pop.df <- data.frame(matrix(nrow = 8131, ncol = 146))
for(i in 1:146){
  pop.df[i] <- data.frame(cols[[i]])
}

# 補上標頭
pop_df_colname_node <- xml_find_all(pop[[1]], "//COLUMN_DESC")
pop_df_colname <- xml_text(pop_df_colname_node)
names(pop.df) <- pop_df_colname

# 這份資料一共有18個年齡層，每個年齡層差距都是五歲
# 寫迴圈
for(i in 1:nrow(pop.df)){
  pop.df[i,147] <- sum(as.numeric(as.character(pop.df[i,3])),as.numeric(as.character(pop.df[i,4])),as.numeric(as.character(pop.df[i,5])),as.numeric(as.character(pop.df[i,6])),as.numeric(as.character(pop.df[i,75])),as.numeric(as.character(pop.df[i,76])),as.numeric(as.character(pop.df[i,77])),as.numeric(as.character(pop.df[i,78])))
  pop.df[i,148] <- sum(as.numeric(as.character(pop.df[i,7])),as.numeric(as.character(pop.df[i,8])),as.numeric(as.character(pop.df[i,9])),as.numeric(as.character(pop.df[i,10])),as.numeric(as.character(pop.df[i,79])),as.numeric(as.character(pop.df[i,80])),as.numeric(as.character(pop.df[i,81])),as.numeric(as.character(pop.df[i,82])))
  pop.df[i,149] <- sum(as.numeric(as.character(pop.df[i,11])),as.numeric(as.character(pop.df[i,12])),as.numeric(as.character(pop.df[i,13])),as.numeric(as.character(pop.df[i,14])),as.numeric(as.character(pop.df[i,83])),as.numeric(as.character(pop.df[i,84])),as.numeric(as.character(pop.df[i,85])),as.numeric(as.character(pop.df[i,86])))
  pop.df[i,150] <- sum(as.numeric(as.character(pop.df[i,15])),as.numeric(as.character(pop.df[i,16])),as.numeric(as.character(pop.df[i,17])),as.numeric(as.character(pop.df[i,18])),as.numeric(as.character(pop.df[i,87])),as.numeric(as.character(pop.df[i,88])),as.numeric(as.character(pop.df[i,89])),as.numeric(as.character(pop.df[i,90])))
  pop.df[i,151] <- sum(as.numeric(as.character(pop.df[i,19])),as.numeric(as.character(pop.df[i,20])),as.numeric(as.character(pop.df[i,21])),as.numeric(as.character(pop.df[i,22])),as.numeric(as.character(pop.df[i,91])),as.numeric(as.character(pop.df[i,92])),as.numeric(as.character(pop.df[i,93])),as.numeric(as.character(pop.df[i,94])))
  pop.df[i,152] <- sum(as.numeric(as.character(pop.df[i,23])),as.numeric(as.character(pop.df[i,24])),as.numeric(as.character(pop.df[i,25])),as.numeric(as.character(pop.df[i,26])),as.numeric(as.character(pop.df[i,95])),as.numeric(as.character(pop.df[i,96])),as.numeric(as.character(pop.df[i,97])),as.numeric(as.character(pop.df[i,98])))
  pop.df[i,153] <- sum(as.numeric(as.character(pop.df[i,27])),as.numeric(as.character(pop.df[i,28])),as.numeric(as.character(pop.df[i,29])),as.numeric(as.character(pop.df[i,30])),as.numeric(as.character(pop.df[i,99])),as.numeric(as.character(pop.df[i,100])),as.numeric(as.character(pop.df[i,101])),as.numeric(as.character(pop.df[i,102])))
  pop.df[i,154] <- sum(as.numeric(as.character(pop.df[i,31])),as.numeric(as.character(pop.df[i,32])),as.numeric(as.character(pop.df[i,33])),as.numeric(as.character(pop.df[i,34])),as.numeric(as.character(pop.df[i,103])),as.numeric(as.character(pop.df[i,104])),as.numeric(as.character(pop.df[i,105])),as.numeric(as.character(pop.df[i,106])))
  pop.df[i,155] <- sum(as.numeric(as.character(pop.df[i,35])),as.numeric(as.character(pop.df[i,36])),as.numeric(as.character(pop.df[i,37])),as.numeric(as.character(pop.df[i,38])),as.numeric(as.character(pop.df[i,107])),as.numeric(as.character(pop.df[i,108])),as.numeric(as.character(pop.df[i,109])),as.numeric(as.character(pop.df[i,110])))
  pop.df[i,156] <- sum(as.numeric(as.character(pop.df[i,39])),as.numeric(as.character(pop.df[i,40])),as.numeric(as.character(pop.df[i,41])),as.numeric(as.character(pop.df[i,42])),as.numeric(as.character(pop.df[i,111])),as.numeric(as.character(pop.df[i,112])),as.numeric(as.character(pop.df[i,113])),as.numeric(as.character(pop.df[i,114])))
  pop.df[i,157] <- sum(as.numeric(as.character(pop.df[i,43])),as.numeric(as.character(pop.df[i,44])),as.numeric(as.character(pop.df[i,45])),as.numeric(as.character(pop.df[i,46])),as.numeric(as.character(pop.df[i,115])),as.numeric(as.character(pop.df[i,116])),as.numeric(as.character(pop.df[i,117])),as.numeric(as.character(pop.df[i,118])))
  pop.df[i,158] <- sum(as.numeric(as.character(pop.df[i,47])),as.numeric(as.character(pop.df[i,48])),as.numeric(as.character(pop.df[i,49])),as.numeric(as.character(pop.df[i,50])),as.numeric(as.character(pop.df[i,119])),as.numeric(as.character(pop.df[i,120])),as.numeric(as.character(pop.df[i,121])),as.numeric(as.character(pop.df[i,122])))
  pop.df[i,159] <- sum(as.numeric(as.character(pop.df[i,51])),as.numeric(as.character(pop.df[i,52])),as.numeric(as.character(pop.df[i,53])),as.numeric(as.character(pop.df[i,54])),as.numeric(as.character(pop.df[i,123])),as.numeric(as.character(pop.df[i,124])),as.numeric(as.character(pop.df[i,125])),as.numeric(as.character(pop.df[i,126])))
  pop.df[i,160] <- sum(as.numeric(as.character(pop.df[i,55])),as.numeric(as.character(pop.df[i,56])),as.numeric(as.character(pop.df[i,57])),as.numeric(as.character(pop.df[i,58])),as.numeric(as.character(pop.df[i,127])),as.numeric(as.character(pop.df[i,128])),as.numeric(as.character(pop.df[i,129])),as.numeric(as.character(pop.df[i,130])))
  pop.df[i,161] <- sum(as.numeric(as.character(pop.df[i,59])),as.numeric(as.character(pop.df[i,60])),as.numeric(as.character(pop.df[i,61])),as.numeric(as.character(pop.df[i,62])),as.numeric(as.character(pop.df[i,131])),as.numeric(as.character(pop.df[i,132])),as.numeric(as.character(pop.df[i,133])),as.numeric(as.character(pop.df[i,134])))
  pop.df[i,162] <- sum(as.numeric(as.character(pop.df[i,63])),as.numeric(as.character(pop.df[i,64])),as.numeric(as.character(pop.df[i,65])),as.numeric(as.character(pop.df[i,66])),as.numeric(as.character(pop.df[i,135])),as.numeric(as.character(pop.df[i,136])),as.numeric(as.character(pop.df[i,137])),as.numeric(as.character(pop.df[i,138])))
  pop.df[i,163] <- sum(as.numeric(as.character(pop.df[i,67])),as.numeric(as.character(pop.df[i,68])),as.numeric(as.character(pop.df[i,69])),as.numeric(as.character(pop.df[i,70])),as.numeric(as.character(pop.df[i,139])),as.numeric(as.character(pop.df[i,140])),as.numeric(as.character(pop.df[i,141])),as.numeric(as.character(pop.df[i,142])))
  pop.df[i,164] <- sum(as.numeric(as.character(pop.df[i,71])),as.numeric(as.character(pop.df[i,72])),as.numeric(as.character(pop.df[i,73])),as.numeric(as.character(pop.df[i,74])),as.numeric(as.character(pop.df[i,143])),as.numeric(as.character(pop.df[i,144])),as.numeric(as.character(pop.df[i,145])),as.numeric(as.character(pop.df[i,146])))
}
pop.df <- pop.df[-c(3:146)]
names(pop.df) <- c("date","CODE2","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99","100以上")

# 從二級發布區圖拉出縣市名、鄉鎮市區名，及二級發布區的代碼
code2 <- data.frame(area$CODE2,paste0(area$COUNTY,area$TOWN))

# 合併pop.df與code2
library(dplyr)
pop.df <- dplyr::inner_join(pop.df,code2,by = c("CODE2" = "area.CODE2"))

# 查看有幾個鄉鎮市區級
dplyr::distinct(pop.df[21]) %>% nrow

# 資料數看似吻合ias.df的列數，現在將重複的鄉鎮市區進行合併
town <- dplyr::distinct(pop.df[21]) %>% c
town <- town$paste0.area.COUNTY..area.TOWN.

pop.df.list <- list()
for(i in 1:368){
  tempp <- dplyr::filter(pop.df,pop.df[,21] == town[i])
  pop.df.list[[i]] <- data.frame(tempp[1,1],tempp[1,2],sum(tempp[3]),sum(tempp[4]),sum(tempp[5]),sum(tempp[6]),sum(tempp[7]),sum(tempp[8]),sum(tempp[9]),sum(tempp[10]),sum(tempp[11]),sum(tempp[12]),sum(tempp[13]),sum(tempp[14]),sum(tempp[15]),sum(tempp[16]),sum(tempp[17]),sum(tempp[18]),sum(tempp[19]),sum(tempp[20]),tempp[1,21])
  names(pop.df.list[[i]]) <- c("date","CODE2","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99","100以上","TOWN")
}
pop.df <- pop.df.list[[1]]
for(i in 1:367){
  pop.df <- rbind(pop.df,pop.df.list[[i+1]])
}

# 去除空白
pop.df[[21]] <- gsub("\\s", "", pop.df[[21]])

# 下載「各鄉鎮市區人口密度」
# 已知 https://data.gov.tw/dataset/8410 此網站是下載的目標了
den.xmlnode <- xml_find_all(read_html("https://data.gov.tw/dataset/8410"),"//*[@id='r0']/div[8]/div[2]/div/a")
url_den <- as.character(xml_attrs(den.xmlnode[[1]])[1])
download.file(url_den[1], destfile = paste0("den",".csv"), mode = "wb")

# 讀取「各鄉鎮市區人口密度」
den <- read.csv(file = "den.csv",encoding = "UTF-8")

# 去除沒用的列位
den <- den[-c(370:376),]
names(den) <- c("統計年","區域別","年底人口數","土地面積","人口密度")
den <- den[-1,]

# 合併den和pop.df
pop.df <- dplyr::left_join(pop.df,den, by=c("TOWN"="區域別"))

# 觀察到似乎有兩個鄉鎮市區join之後沒有資料
# 觀察到問題出在pop.df中「彰化縣員林鎮」及「苗栗縣頭份鎮」這兩個鎮級單位都已經變成縣轄市
# 應該將這兩個地區分別改為「彰化縣員林市」及「苗栗縣頭份市」

pop.df[345,21] <- "彰化縣員林市"
pop.df[100,21] <- "苗栗縣頭份市"
pop.df <- pop.df[,-c(22:25)]

# 重新再join一次
pop.df <- dplyr::left_join(pop.df,den, by=c("TOWN"="區域別"))

# 算出14歲以下的人口
for(i in 1:368){
  pop.df[i,26] <- as.numeric(as.character(pop.df$年底人口數[i]))-sum(pop.df[i,3:20])
}
dplyr::filter(pop.df, V26 < 0 )

# 看似只有四個地區最後算出來的14歲以下人口變成負值...
# 將這些負值變成0
pop.df[129,26] <- 0
pop.df[225,26] <- 0
pop.df[226,26] <- 0
pop.df[230,26] <- 0

# 整理欄位
pop.df[1] <- pop.df[21]
pop.df[2] <- pop.df[26]
pop.df[21] <- pop.df[23]
pop.df <- pop.df[,-c(22:26)]

# 補上標頭
names(pop.df) <- c("TOWN","14以下","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99","100以上","Total")
DT::datatable(head(pop.df,100))

##############################################################



##############################################################
# 五、第三筆資料：各村里教育程度資料
# 提供機關：內政部戶政司
# 製作各鄉鎮市區教育程度結構表edu.df
##############################################################

View(edu.main)
names(edu.main) <- as.character(edu.main[1,])
edu.main <- edu.main[-1,]

# 整合資料
library(dplyr)
district <- dplyr::distinct(edu.main[3]) %>% c
district <- district$`區域別 `

for(i in 1:7851){
  edu.main[i,52] <- sum(as.numeric(edu.main[6][[1]][i]),as.numeric(edu.main[7][[1]][i]),as.numeric(edu.main[8][[1]][i]),as.numeric(edu.main[9][[1]][i]))
}
for(i in 1:7851){
  edu.main[i,53] <- sum(as.numeric(edu.main[10][[1]][i]),as.numeric(edu.main[11][[1]][i]),as.numeric(edu.main[12][[1]][i]),as.numeric(edu.main[13][[1]][i]))
}
for(i in 1:7851){
  edu.main[i,54] <- sum(as.numeric(edu.main[14][[1]][i]),as.numeric(edu.main[15][[1]][i]),as.numeric(edu.main[16][[1]][i]),as.numeric(edu.main[17][[1]][i]))
}
for(i in 1:7851){
  edu.main[i,55] <- sum(as.numeric(edu.main[18][[1]][i]),as.numeric(edu.main[19][[1]][i]),as.numeric(edu.main[20][[1]][i]),as.numeric(edu.main[21][[1]][i]))
}
for(i in 1:7851){
  edu.main[i,56] <- sum(as.numeric(edu.main[22][[1]][i]),as.numeric(edu.main[23][[1]][i]),as.numeric(edu.main[24][[1]][i]),as.numeric(edu.main[25][[1]][i]))
}
for(i in 1:7851){
  edu.main[i,57] <- sum(as.numeric(edu.main[26][[1]][i]),as.numeric(edu.main[27][[1]][i]),as.numeric(edu.main[28][[1]][i]),as.numeric(edu.main[29][[1]][i]))
}
for(i in 1:7851){
  edu.main[i,58] <- sum(as.numeric(edu.main[30][[1]][i]),as.numeric(edu.main[31][[1]][i]),as.numeric(edu.main[32][[1]][i]),as.numeric(edu.main[33][[1]][i]))
}
for(i in 1:7851){
  edu.main[i,59] <- sum(as.numeric(edu.main[36][[1]][i]),as.numeric(edu.main[37][[1]][i]),as.numeric(edu.main[38][[1]][i]),as.numeric(edu.main[39][[1]][i]))
}
for(i in 1:7851){
  edu.main[i,60] <- sum(as.numeric(edu.main[40][[1]][i]),as.numeric(edu.main[41][[1]][i]),as.numeric(edu.main[42][[1]][i]),as.numeric(edu.main[43][[1]][i]))
}
for(i in 1:7851){
  edu.main[i,61] <- sum(as.numeric(edu.main[44][[1]][i]),as.numeric(edu.main[45][[1]][i]),as.numeric(edu.main[46][[1]][i]),as.numeric(edu.main[47][[1]][i]))
}

edu.df <- edu.main[-c(6:51)]
names(edu.df) <- c("統計年","區域別代碼","區域別","村里","總和","博士","碩士","大學","四技二專","五專","高中","高職","國中","初中","國小")

# 將村里級資料轉變為鄉鎮市區級資料
edu.df.list <- list()
for(i in 1:370){
  temp <- dplyr::filter(edu.df,edu.df[,3] == district[i])
  edu.df.list[[i]] <- data.frame(temp[1,1],temp[1,2],temp[1,3],temp[1,4],sum(as.numeric(temp[5][[1]])),sum(as.numeric(temp[6][[1]])),sum(as.numeric(temp[7][[1]])),sum(as.numeric(temp[8][[1]])),sum(as.numeric(temp[9][[1]])),sum(as.numeric(temp[10][[1]])),sum(as.numeric(temp[11][[1]])),sum(as.numeric(temp[12][[1]])),sum(as.numeric(temp[13][[1]])),sum(as.numeric(temp[14][[1]])),sum(as.numeric(temp[15][[1]])))
  names(edu.df.list[[i]]) <- c("統計年","區域別代碼","區域別","村里","總和","博士","碩士","大學","四技二專","五專","高中","高職","國中","初中","國小")
}
edu.df <- edu.df.list[[1]]
for(i in 1:369){
  edu.df <- rbind(edu.df,edu.df.list[[i+1]])
}

edu.df <- edu.df[,-4]

# 去除空白
edu.df[[3]] <- gsub("\\s", "", edu.df[[3]])

# 查看資料列數
nrow(edu.df)

# 似乎沒有和ias.df及pop.df的列數吻合，
# 發現高雄市三民區與鳳山區被拆分成兩個統計單位
# 需另外將這兩個行政區被拆分的資料進行整合

add1 <- data.frame(edu.df[125,1],edu.df[125,2],"高雄市三民區",sum(edu.df[125:126,4]),sum(edu.df[125:126,5]),sum(edu.df[125:126,6]),sum(edu.df[125:126,7]),sum(edu.df[125:126,8]),sum(edu.df[125:126,9]),sum(edu.df[125:126,10]),sum(edu.df[125:126,11]),sum(edu.df[125:126,12]),sum(edu.df[125:126,13]),sum(edu.df[125:126,14]))
names(add1) <- names(edu.df)
add2 <- data.frame(edu.df[133,1],edu.df[133,2],"高雄市鳳山區",sum(edu.df[133:134,4]),sum(edu.df[133:134,5]),sum(edu.df[133:134,6]),sum(edu.df[133:134,7]),sum(edu.df[133:134,8]),sum(edu.df[133:134,9]),sum(edu.df[133:134,10]),sum(edu.df[133:134,11]),sum(edu.df[133:134,12]),sum(edu.df[133:134,13]),sum(edu.df[133:134,14]))
names(add2) <- names(edu.df)
adds <- rbind(add1,add2)
edu.df <- rbind(edu.df,adds)

# 整理欄位
edu.df <- edu.df[,-c(1,2)]
DT::datatable(head(edu.df,100))

##############################################################



##############################################################
# 六、資料整合
# 整合ias.df / pop.df / edu.df
##############################################################

# 先看看目前整理的結果
View(ias.df)
View(pop.df)
View(edu.df)

# 檢查各自的列位數，是否皆為368列
nrow(ias.df)
nrow(pop.df)
nrow(edu.df)

# 似乎只有edu.df的列數不太一樣，主要是因為高雄市三民區與鳳山區被拆分成兩個統計單位
# 等等用inner_join的時候「三民一」「三民二」「鳳山一」「鳳山二」都會被剔除
# ias.df中似乎太多欄位了，因此只選取「薪資所得」欄位
# 其他與研究方向無關的欄位先刪除
ias.df <- ias.df[,c(2,3,7)]

# join這三份資料
join.df <- dplyr::inner_join(ias.df,pop.df, by=c("村里"="TOWN"))
joinall.df <- dplyr::inner_join(join.df,edu.df, by=c("村里"="區域別"))

# 所有資料都整合在一起了，列位數也全部變成368列了
nrow(joinall.df)


##############################################################



##############################################################
# 七、視覺化
# 視覺化joinall.df
##############################################################

# >> 研究方向
#    學生想了解，各鄉鎮市區級的個人(薪資)所得稅多寡
#    是與「當地人民的教育程度」有關，
#    還是是跟「當地的人口年齡結構」有關。

##############################################################

# (I)先看各鄉鎮市區級「個人(薪資)所得稅多寡」與「當地的人口年齡結構」的關係
# 年齡結構最著名的比例就是撫養比了
# 撫養比：（幼年人口＋老年人口）÷ 青壯年人口 × 100
# 老年人口＝65歲以上人口；青壯年人口＝15-64歲人口；幼年人口＝0-14歲人口
# 撫養比又被稱作：依賴人口指數 Dependency Ratio
for(i in 1:368){
  joinall.df[i,35] <- sum(joinall.df[i,4],joinall.df[i,15],joinall.df[i,16],joinall.df[i,17],joinall.df[i,18],joinall.df[i,19],joinall.df[i,20],joinall.df[i,21],joinall.df[i,22])
}
for(i in 1:368){
  joinall.df[i,36] <- sum(joinall.df[i,5],joinall.df[i,6],joinall.df[i,7],joinall.df[i,8],joinall.df[i,9],joinall.df[i,10],joinall.df[i,11],joinall.df[i,12],joinall.df[i,13],joinall.df[i,14])
}
for(i in 1:368){
  joinall.df[i,37] <- (joinall.df[i,35]*100) / (joinall.df[i,36])
}
range(joinall.df$V37)

# 來看看撫養比與個人(薪資)所得稅多寡是否有相關
library(ggplot2)
ggplot(joinall.df, aes(x = as.numeric(as.character(joinall.df$薪資所得)), y = joinall.df$V37)) +
  geom_point()

# 似乎看不出相關性
cor.test(as.numeric(as.character(joinall.df$薪資所得)),joinall.df$V37)

# 相關係數＝-0.1175454，屬於低度負相關
# 也就是當該地區個人收入如果越多，可能該地區撫養比也不會因此有明顯下降的情形


##############################################################

# (II)再看各鄉鎮市區級「個人(薪資)所得稅多寡」與「當地人民的教育程度」的關係
# 為了不使測量上過於複雜，將依據教育程度標準分類之「等級」向度分類之
# 資料來源：http://stats.moe.gov.tw/files/bcode/105bcode_book.pdf
# 0：學前教育 / 1：國小 / 2：國中 / 3：高中職 / 5：專科 / 6：學士 / 7：碩士 / 8：博士

# 可以觀察到joinall.df中有這些教育程度分類：國小、初中、國中、高職、高中、五專、四技二專、大學、碩士、博士
# 將這些教育程度依照上述方法進行分類：
# 1：國小 / 2：初中、國中 / 3：高職、高中 / 5：五專 / 6：四技二專、大學 / 7：碩士 / 8：博士
# 計算方式：若一人最高學歷為國小，則得1分；若一人最高學歷為五專，則得5分。依此類推，計算出每個地區的education points
for(i in 1:368){
  joinall.df[i,38] <- sum(8*joinall.df[i,25],7*joinall.df[i,26],6*joinall.df[i,27],6*joinall.df[i,28],5*joinall.df[i,29],3*joinall.df[i,30],3*joinall.df[i,31],2*joinall.df[i,32],2*joinall.df[i,33],joinall.df[i,34])
}
range(joinall.df$V38)

# 來看看education points與個人(薪資)所得稅多寡是否有相關
ggplot(joinall.df, aes(x = as.numeric(as.character(joinall.df$薪資所得)), y = joinall.df$V38)) +
  geom_point()

# 似乎有看出相關性
cor.test(as.numeric(as.character(joinall.df$薪資所得)),joinall.df$V38)

# 相關係數＝0.9769521，屬於高度正相關
# 也就是當該地區個人收入如果越多，可能該地區的教育程度也會較高


##############################################################

# 再進一步從空間資料來看相關性
library(RTaiwan)
library(sp)
library(rgdal)
library(leaflet)
data(TaiwanTownArea, package = "RTaiwan")
TTA <- sp::spTransform(TaiwanTownArea, CRS("+init=epsg:4326"))

# 萃取出SpatialPolygonsDataFrame中的欄位，轉變為data.frame，並與joinall.df進行合併
TTA1 <- data.frame(TTA[1],paste0(TTA$COUNTY,TTA$TOWN),coordinates(TTA))
names(TTA1) <- c("TOWN_ID","TOWN","lng","lat")

joinall.TTA1 <- left_join(joinall.df,TTA1, by = c("村里" = "TOWN" ))
joinall.TTA1[98,39] <- 10005050
joinall.TTA1[98,40] <- 120.9189
joinall.TTA1[98,41] <- 24.67640  
joinall.TTA1[347,39] <- 10007100
joinall.TTA1[347,40] <- 120.5930
joinall.TTA1[347,41] <- 23.95663

# 將合併後的data.frame與SpatialPolygonsDataFrame整合
.data1 <- left_join(TTA@data,joinall.TTA1, by = c("TOWN_ID" = "TOWN_ID" ))
nrow(.data1)
TTA@data <- .data1

# 調色盤變數 - 依據撫養比
pal <-colorNumeric(
  palette = "YlOrRd",
  domain = TTA$V37)

# 畫出撫養比高低的分布情況
map <- leaflet(TTA) %>%
  addPolygons(popup = paste0(TTA$COUNTY,TTA$TOWN), fillColor = ~pal(TTA$V37), color = "white", stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1) %>%
  addLegend("bottomright", pal = pal, values =TTA$V37 ,opacity = 1)
map

# 調色盤變數 - 依據薪資所得
pal1 <-colorNumeric(
  palette = "YlOrRd",
  domain = as.numeric(as.character(TTA$薪資所得)))

# 畫出薪資所得的分布情況
map1 <- leaflet(TTA) %>%
  addPolygons(popup = paste0(TTA$COUNTY,TTA$TOWN), fillColor = ~pal1(as.numeric(as.character(TTA$薪資所得))), color = "white", stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1) %>%
  addLegend("bottomright", pal = pal1, values =as.numeric(as.character(TTA$薪資所得)) ,opacity = 1)
map1

# 其實可以發現map和map1畫出來之後，兩張圖的長相其實差蠻多的

# 調色盤變數 - 依據education points
pal2 <-colorNumeric(
  palette = "YlOrRd",
  domain = TTA$V38)

# 畫出education points的分布情況
map2 <- leaflet(TTA) %>%
  addPolygons(popup = paste0(TTA$COUNTY,TTA$TOWN), fillColor = ~pal2(TTA$V38), color = "white", stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1) %>%
  addLegend("bottomright", pal = pal2, values =TTA$V38 ,opacity = 1)
map2

# 其實可以發現map1和map2畫出來之後，兩張圖的長相其實差不了太多


##############################################################

# >> 小結：根據學生欲探討的主題 - 
#    各鄉鎮市區級的個人(薪資)所得稅多寡，是與「當地人民的教育程度」有關，還是是跟「當地的人口年齡結構」有關？
#    針對上述簡易的分析，可以發現
#    各鄉鎮市區級的個人(薪資)所得稅多寡，會與「當地人民的教育程度」有高度正相關性

##############################################################

library("rmarkdown")
library("knitr")

pvm::export.packages()
saveRDS(sessionInfo(), file = "sessionInfo.Rds")

