# 載入讀取xlsx檔案的套件
library(readxl)

####2016 data####

# 讀取檔案，這是2016的raw data
# 可先用getwd查看目前預設路徑
# 使用setwd修改預設路徑後就不用打這麼一長串了（但我懶得改）
raw_2016 <- read_excel("my documents/111-1 傳播導向的大數據分析/期末專案/文化部問卷調查統計數據/2016文化參與及消費調查opendata.xlsx",
                      col_names = TRUE)

# 看看資料狀況
head(raw_2016)

# 用colnames查看欄位名稱
colnames(raw_2016)

# 挑出需要用的欄位，另存成一個資料集
pickVar = c("no", "v01", "v25_1", "v25_2", "v25_3", "v25_4", "v25_5", 
            "v26", "v26_1", "v24_1", "v24_2", "v27_1", "v27_2", "v27_3",
            "v27_4", "v27_5", "v27_6", "v27_7", "v27_8", "v27_9", "v27_10",
            "v27_11", "v27_12", "v27_13", "v29", "v30", "v31", "v32",
            "v33", "v34", "v24_3", "v24_4", "v24_5", "v24_6", "v24_7",
            "v24_8", "v24_9", "v24_10", "v24_11", "v24_12", "v15_1",
            "v15_2", "v15_3", "v15_4", "v15_5", "v15_6", "v15_7", "v15_8",
            "v15_9", "v15_11")

data_2016 <- raw_2016[, pickVar]
colnames(data_2016)　　　#確認欄位沒錯


#------------格式處理------------

# 為了和其他年份的資料合併，要先加入一些空的欄位
emptyCol = rep(NA, nrow(data_2016))　　# 製造空的向量
year = rep(2016, nrow(data_2016))　　# 也會需要加入年份的欄位
data_2016 <- cbind(data_2016[ , 1], year, data_2016[ , 2:6], emptyCol, 
                   data_2016[ , 7], emptyCol, data_2016[ , 8:24], 
                   emptyCol, emptyCol, data_2016[ , 25:49], emptyCol,
                   emptyCol, emptyCol, data_2016[, 50])
ncol(data_2016)



#------------遺漏值處理------------

colnames(data_2016)
# 原始資料中，v25_6, v24_13, v27_14若為1，表示題組拒答
del <- which(raw_2016$v25_6 == 1)　　# 這裡修改變數名稱
data_2016[del, 4:8] = NA　　　　# 這裡修改要放成NA的題組在哪幾欄
# 以下複製
del <- which(raw_2016$v24_13 == 1)　　
data_2016[del, c(13:14, 36:45)] = NA

del <- which(raw_2016$v27_14 == 1)　　
data_2016[del, 15:27] = NA


# v15_11, v24_12, v27_13若為1，表示都沒有（題組其他選項皆應為0）
allNo <- which(raw_2016$v15_11 == 1)　　　　# 這裡修改變數名稱
allNo_sum <- rowSums(data_2016[allNo, 46:54], na.rm = TRUE)　　　　# 這裡修改欄位
length(which(allNo_sum > 0))　　　　# 如果這裡有抓出東西，表示有人填答矛盾，要刪掉
# 上面這個which也會回報出row names，就可以回到data_2016裡面去修改數值
# 以下複製
allNo <- which(raw_2016$v24_12 == 1)　　　　
allNo_sum <- rowSums(data_2016[allNo, c(13:14, 36:44)], na.rm = TRUE)
length(which(allNo_sum > 0))

allNo <- which(raw_2016$v27_13 == 1)　　　　
allNo_sum <- rowSums(data_2016[allNo, c(15:26)], na.rm = TRUE)
length(which(allNo_sum > 0))
# 這邊發現有人選以上皆否，可是其他選項coding選成跳題(9)，要改成0
data_2016["8401", 15:26] <- 0


# v29, v30, v31, v32, v33, v34若為98 or 99, 表示拒答或漏答
miss <- which(data_2016$v29 >= 98)　　　# 手動修改要檢查的欄位
length(miss)
data_2016[miss, "v29"] = NA　　　　# 手動修改要檢查的欄位
# 以下複製
miss <- which(data_2016$v30 >= 98)
length(miss)
data_2016[miss, "v30"] = NA

miss <- which(data_2016$v31 >= 98)
length(miss)
data_2016[miss, "v31"] = NA

miss <- which(data_2016$v32 >= 98)
length(miss)
data_2016[miss, "v32"] = NA

miss <- which(data_2016$v33 >= 98)
length(miss)
data_2016[miss, "v33"] = NA

miss <- which(data_2016$v34 >= 98)
length(miss)
data_2016[miss, "v34"] = NA


# 其他在是非題當中如果出現9的代表有漏答
# v25_1~v25_4, v24_1~v24_13, v27_1~v27_14, v15_1~v15_11
# 迴圈的數值改成4:7, if後面改成4:7
# 迴圈的數值改成13:14, if後面改成c(13:14, 36:45)
# 迴圈的數值改成36:45, if不變
# 迴圈的數值改成15:27, if後面改成15:27
# 迴圈的數值改成46:54, if後面改成c(46:54, 58)
# 迴圈的數值改成58:58, if後面不變
for (x in 58:58) {
  miss <- which(data_2016[ , x] == 9)
  print(length(miss))
  
  if (length(miss) >0) {
    data_2016[miss, c(46:54, 58)] <- NA 
    }
}



#------------變數命名------------
# 最後是把變數名稱統一一下
varNam = c(
   "no","year", "resi", "v22_1", "v22_2", "v22_3", "v22_4", "v22_5", 
   "v22_6", "v22_7", "v23", "v23_1", "v25_0", "v25_1", "v30_1", "v30_2", 
   "v30_3", "v30_4", "v30_5", "v30_6", "v30_7", "v30_8", "v30_9", "v30_10",
   "v30_11", "v30_12", "v30_13", "v32_1", "v32_9", "age", "edu", "fami", 
   "occu", "inco", "sex", "v25_2", "v25_3", "v25_4", "v25_5", "v25_6",
   "v25_7", "v25_8", "v25_9", "v25_10", "v25_11", "v26_1", "v26_2", 
   "v26_3", "v26_4", "v26_5", "v26_6", "v26_7", "v26_8", "v26_9", "v26_10",
   "v26_11", "v26_12", "v26_13")
colnames(data_2016) <- varNam

####END of 2016####





####2017 data####

# 讀取檔案，這是2017的raw data
# 使用setwd修改預設路徑後就不用打這麼一長串了（但我懶得改）
raw_2017 <- read_excel("my documents/111-1 傳播導向的大數據分析/期末專案/文化部問卷調查統計數據/2017文化參與及消費調查opendata.xlsx",
                       col_names = TRUE)

# 看看資料狀況
head(raw_2017)

# 用colnames查看欄位名稱
colnames(raw_2017)

# 挑出需要用的欄位，另存成一個資料集
pickVar = c("no1", "v01", "v25_1", "v25_2", "v25_3", "v25_4", "v25_5",
            "v25_6", "v25_7", "v26", "v26_1", "v28_0", "v28_1", "v29_1", 
            "v29_2", "v29_3", "v29_4", "v29_5", "v29_6", "v29_7", "v29_8",
            "v29_9", "v29_10", "v29_11", "v29_12", "v29_13", "v31_1", 
            "v31_9","v33", "v34", "v35", "v36", "v37", "v38", "v28_2", 
            "v28_3", "v28_4", "v28_5", "v28_6", "v28_7", "v28_8", "v28_9",
            "v28_10", "v28_11", "v16_1", "v16_2", "v16_3", "v16_4", "v16_5",
            "v16_6", "v16_7", "v16_8", "v16_9", "v16_11")

data_2017 <- raw_2017[, pickVar]
colnames(data_2017)　　　#確認欄位沒錯


#------------格式處理------------

# 為了和其他年份的資料合併，要先加入一些空的欄位
emptyCol = rep(NA, nrow(data_2017))　　# 製造空的向量
year = rep(2017, nrow(data_2017))　　# 也會需要加入年份的欄位
data_2017 <- cbind(data_2017[ , 1], year, data_2017[ , 2:53], emptyCol, 
                   emptyCol, emptyCol, data_2017[ , 54])
ncol(data_2017)
names(data_2017)


#------------遺漏值處理------------

# v16_11, v25_7, v28_11, v29_13, v31_12若為1，表示都沒有（題組其他選項皆應為0）
allNo <- which(raw_2017$v16_11 == 1)　　　　# 這裡修改變數名稱
allNo_sum <- rowSums(data_2017[allNo, 46:54], na.rm = TRUE)　　　　# 這裡修改欄位
length(which(allNo_sum > 0))　　　　# 如果這裡有抓出東西，表示有人填答矛盾，要刪掉
# 上面這個which也會回報出row names，就可以回到data_2017裡面去修改數值
# 以下複製
allNo <- which(raw_2017$v25_7 == 1)　　　　
allNo_sum <- rowSums(data_2017[allNo, c(4:9, 13)], na.rm = TRUE)
length(which(allNo_sum > 0))

allNo <- which(raw_2017$v28_11 == 1)　　　　
allNo_sum <- rowSums(data_2017[allNo, c(14, 36:44)], na.rm = TRUE)
length(which(allNo_sum > 0))

allNo <- which(raw_2017$v29_13 == 1)　　　　
allNo_sum <- rowSums(data_2017[allNo, c(15:26)], na.rm = TRUE)
length(which(allNo_sum > 0))

allNo <- which(raw_2017$v31_12 == 1)　　　　
allNo_sum <- rowSums(data_2017[allNo, c(28:29)], na.rm = TRUE)
length(which(allNo_sum > 0))



# v33, v34, v35, v36, v37, v38若為98 or 99, 表示拒答或漏答
miss <- which(data_2017$v33 >= 98)　　　# 手動修改要檢查的欄位
length(miss)
data_2017[miss, "v33"] = NA　　　　# 手動修改要檢查的欄位
# 以下複製
miss <- which(data_2017$v34 >= 98)
length(miss)
data_2017[miss, "v34"] = NA

miss <- which(data_2017$v35 >= 98)
length(miss)
data_2017[miss, "v35"] = NA

miss <- which(data_2017$v36 >= 98)
length(miss)
data_2017[miss, "v36"] = NA

miss <- which(data_2017$v37 >= 98)
length(miss)
data_2017[miss, "v37"] = NA

miss <- which(data_2017$v38 >= 98)
length(miss)
data_2017[miss, "v38"] = NA


# 其他在是非題當中如果出現9的代表有漏答
# v25_1~v25_7, v28_1~v28_11, v29_1~v29_13,v31_1 & v31_9, v16_1~v16_11
# 迴圈的數值改成4:10, if後面改成c(4:10, 13)
# 迴圈的數值改成14:14, if後面改成c(14, 36:45)
# 迴圈的數值改成36:45, if不變
# 迴圈的數值改成15:27, if後面改成15:27
# 迴圈的數值改成28:29, if後面改成28:29
# 迴圈的數值改成46:54, if後面改成c(46:54, 58)
# 迴圈的數值改成58:58, if後面不變
for (x in 58:58) {
  miss <- which(data_2017[ , x] == 9)
  print(length(miss))
  
  if (length(miss) >0) {
    data_2017[miss, c(46:54, 58)] <- NA 
  }
}


#------------變數命名------------
# 最後是把變數名稱統一一下
varNam = c(
  "no","year", "resi", "v22_1", "v22_2", "v22_3", "v22_4", "v22_5", 
  "v22_6", "v22_7", "v23", "v23_1", "v25_0", "v25_1", "v30_1", "v30_2", 
  "v30_3", "v30_4", "v30_5", "v30_6", "v30_7", "v30_8", "v30_9", "v30_10",
  "v30_11", "v30_12", "v30_13", "v32_1", "v32_9", "age", "edu", "fami", 
  "occu", "inco", "sex", "v25_2", "v25_3", "v25_4", "v25_5", "v25_6",
  "v25_7", "v25_8", "v25_9", "v25_10", "v25_11", "v26_1", "v26_2", 
  "v26_3", "v26_4", "v26_5", "v26_6", "v26_7", "v26_8", "v26_9", "v26_10",
  "v26_11", "v26_12", "v26_13")
colnames(data_2017) <- varNam
identical(names(data_2016), names(data_2017))



####END of 2017####







####2018 data####

# 讀取檔案，這是2018的raw data
# 使用setwd修改預設路徑後就不用打這麼一長串了（但我懶得改）
raw_2018 <- read_excel("my documents/111-1 傳播導向的大數據分析/期末專案/文化部問卷調查統計數據/2018文化參與及消費調查opendata.xlsx",
                       col_names = TRUE)

# 看看資料狀況
head(raw_2018)

# 用colnames查看欄位名稱
colnames(raw_2018)

# 挑出需要用的欄位，另存成一個資料集
pickVar = c("no1", "v01", "v22_1", "v22_2", "v22_3", "v22_4", "v22_5",
            "v22_6", "v22_7", "v23", "v23_1", "v25_0", "v25_1", "v30_1", 
            "v30_2", "v30_3", "v30_4", "v30_5", "v30_6", "v30_7", "v30_8",
            "v30_9", "v30_10", "v30_11", "v30_12", "v30_13", "v32_1", 
            "v32_9","v34", "v35", "v36", "v37", "v38", "v39", "v25_2", 
            "v25_3", "v25_4", "v25_5", "v25_6", "v25_7", "v25_8", "v25_9",
            "v25_10", "v25_11", "v26_1", "v26_2", "v26_3", "v26_4", "v26_5",
            "v26_6", "v26_7", "v26_8", "v26_9", "v26_10", "v26_11", "v26_12",
            "v26_13")

data_2018 <- raw_2018[, pickVar]
colnames(data_2018)　　　#確認欄位沒錯


#------------格式處理------------

# 為了和其他年份的資料合併，要先加入一些空的欄位
# emptyCol = rep(NA, nrow(data_2018))　　# 製造空的向量
year = rep(2018, nrow(data_2018))　　# 也會需要加入年份的欄位
data_2018 <- cbind(data_2018[ , 1], year, data_2018[ , 2:57])
ncol(data_2018)
names(data_2018)


#------------遺漏值處理------------

# v26_13, v22_7, v25_11, v30_13, v32_12若為1，表示都沒有（題組其他選項皆應為0）
allNo <- which(raw_2018$v26_13 == 1)　　　　# 這裡修改變數名稱
allNo_sum <- rowSums(data_2018[allNo, 46:57], na.rm = TRUE)　　　　# 這裡修改欄位
length(which(allNo_sum > 0))　　　　# 如果這裡有抓出東西，表示有人填答矛盾，要刪掉
# 上面這個which也會回報出row names，就可以回到data_2017裡面去修改數值
# 以下複製
allNo <- which(raw_2018$v22_7 == 1)　　　　
allNo_sum <- rowSums(data_2018[allNo, c(4:9, 13)], na.rm = TRUE)
length(which(allNo_sum > 0))

allNo <- which(raw_2018$v25_11 == 1)　　　　
allNo_sum <- rowSums(data_2018[allNo, c(14, 36:44)], na.rm = TRUE)
length(which(allNo_sum > 0))

allNo <- which(raw_2018$v30_13 == 1)　　　　
allNo_sum <- rowSums(data_2018[allNo, c(15:26)], na.rm = TRUE)
length(which(allNo_sum > 0))

allNo <- which(raw_2018$v32_12 == 1)　　　　
allNo_sum <- rowSums(data_2018[allNo, c(28:29)], na.rm = TRUE)
length(which(allNo_sum > 0))



# v34, v35, v36, v37, v38, v39若為98 or 99, 表示拒答或漏答
miss <- which(data_2018$v34 >= 98)      # 手動修改要檢查的欄位
length(miss)
data_2018[miss, "v34"] = NA　　　　　　# 手動修改要檢查的欄位

miss <- which(data_2018$v35 >= 98)
length(miss)
data_2018[miss, "v35"] = NA

miss <- which(data_2018$v36 >= 98)
length(miss)
data_2018[miss, "v36"] = NA

miss <- which(data_2018$v37 >= 98)
length(miss)
data_2018[miss, "v37"] = NA

miss <- which(data_2018$v38 >= 98)
length(miss)
data_2018[miss, "v38"] = NA

miss <- which(data_2018$v39 >= 98)
length(miss)
data_2018[miss, "v39"] = NA


# 其他在是非題當中如果出現9的代表有漏答
# v22_1~v22_7, v25_1~v25_11, v30_1~v30_13,v32_1 & v32_9, v26_1~v26_13
# 迴圈的數值改成4:10, if後面改成c(4:10, 13)
# 迴圈的數值改成14:14, if後面改成c(14, 36:45)
# 迴圈的數值改成36:45, if不變
# 迴圈的數值改成15:27, if後面改成15:27
# 迴圈的數值改成28:29, if後面改成28:29
# 迴圈的數值改成46:58, if後面改成c(46:58)

for (x in 46:58) {
  miss <- which(data_2018[ , x] == 9)
  print(length(miss))
  
  if (length(miss) >0) {
    data_2018[miss, c(46:58)] <- NA 
  }
}


#------------變數命名------------
# 最後是把變數名稱統一一下
varNam = c(
  "no","year", "resi", "v22_1", "v22_2", "v22_3", "v22_4", "v22_5", 
  "v22_6", "v22_7", "v23", "v23_1", "v25_0", "v25_1", "v30_1", "v30_2", 
  "v30_3", "v30_4", "v30_5", "v30_6", "v30_7", "v30_8", "v30_9", "v30_10",
  "v30_11", "v30_12", "v30_13", "v32_1", "v32_9", "age", "edu", "fami", 
  "occu", "inco", "sex", "v25_2", "v25_3", "v25_4", "v25_5", "v25_6",
  "v25_7", "v25_8", "v25_9", "v25_10", "v25_11", "v26_1", "v26_2", 
  "v26_3", "v26_4", "v26_5", "v26_6", "v26_7", "v26_8", "v26_9", "v26_10",
  "v26_11", "v26_12", "v26_13")
colnames(data_2018) <- varNam
identical(names(data_2016), names(data_2018))


####END of 2018####




####2019 data####

# 讀取檔案，這是2019的raw data
# 使用setwd修改預設路徑後就不用打這麼一長串了（但我懶得改）
raw_2019 <- read_excel("my documents/111-1 傳播導向的大數據分析/期末專案/文化部問卷調查統計數據/2019文化參與及消費調查opendata.xlsx",
                       col_names = TRUE)

# 看看資料狀況
head(raw_2019)

# 用colnames查看欄位名稱
colnames(raw_2019)

# 挑出需要用的欄位，另存成一個資料集
pickVar = c("no", "v02", "v22_1", "v22_2", "v22_3", "v22_4", "v22_5",
            "v22_6", "v22_7", "v23", "v23_1", "v25_0", "v25_1", "v30_1", 
            "v30_2", "v30_3", "v30_4", "v30_5", "v30_6", "v30_7", "v30_8",
            "v30_9", "v30_10", "v30_11", "v30_12", "v30_13", "v32_1", 
            "v32_9", "v34", "v35", "v36", "v37", "v38", "v39", "v25_2", 
            "v25_3", "v25_4", "v25_5", "v25_6", "v25_7", "v25_8", "v25_9",
            "v25_10", "v25_11", "v26_1", "v26_2", "v26_3", "v26_4", "v26_5",
            "v26_6", "v26_7", "v26_8", "v26_9", "v26_10", "v26_11", "v26_12",
            "v26_13")

data_2019 <- raw_2019[, pickVar]
colnames(data_2019)　　　#確認欄位沒錯


#------------格式處理------------

# 為了和其他年份的資料合併，要先加入一些空的欄位
# emptyCol = rep(NA, nrow(data_2018))　　# 製造空的向量
year = rep(2019, nrow(data_2019))　　# 也會需要加入年份的欄位
data_2019 <- cbind(data_2019[ , 1], year, data_2019[ , 2:57])
ncol(data_2019)
names(data_2019)


#------------遺漏值處理------------

# v26_13, v22_7, v25_11, v30_13, v32_12若為1，表示都沒有（題組其他選項皆應為0）
allNo <- which(raw_2019$v26_13 == 1)　　　　# 這裡修改變數名稱
allNo_sum <- rowSums(data_2019[allNo, 46:57], na.rm = TRUE)　　　　# 這裡修改欄位
length(which(allNo_sum > 0))　　　　# 如果這裡有抓出東西，表示有人填答矛盾，要刪掉
# 上面這個which也會回報出row names，就可以回到data_2017裡面去修改數值
# 以下複製
allNo <- which(raw_2019$v22_7 == 1)　　　　
allNo_sum <- rowSums(data_2019[allNo, c(4:9, 13)], na.rm = TRUE)
length(which(allNo_sum > 0))

allNo <- which(raw_2019$v25_11 == 1)　　　　
allNo_sum <- rowSums(data_2019[allNo, c(14, 36:44)], na.rm = TRUE)
length(which(allNo_sum > 0))

allNo <- which(raw_2019$v30_13 == 1)　　　　
allNo_sum <- rowSums(data_2019[allNo, c(15:26)], na.rm = TRUE)
length(which(allNo_sum > 0))

allNo <- which(raw_2019$v32_12 == 1)　　　　
allNo_sum <- rowSums(data_2019[allNo, c(28:29)], na.rm = TRUE)
length(which(allNo_sum > 0))



# v34, v35, v36, v37, v38, v39若為98 or 99, 表示拒答或漏答
miss <- which(data_2019$v34 >= 98)      # 手動修改要檢查的欄位
length(miss)
data_2019[miss, "v34"] = NA　　　　　　# 手動修改要檢查的欄位

miss <- which(data_2019$v35 >= 98)
length(miss)
data_2019[miss, "v35"] = NA

miss <- which(data_2019$v36 >= 98)
length(miss)
data_2019[miss, "v36"] = NA

miss <- which(data_2019$v37 >= 98)
length(miss)
data_2019[miss, "v37"] = NA

miss <- which(data_2019$v38 >= 98)
length(miss)
data_2019[miss, "v38"] = NA

miss <- which(data_2019$v39 >= 98)
length(miss)
data_2019[miss, "v39"] = NA


# 其他在是非題當中如果出現9的代表有漏答
# v22_1~v22_7, v25_1~v25_11, v30_1~v30_13,v32_1 & v32_9, v26_1~v26_13
# 迴圈的數值改成4:10, if後面改成c(4:10, 13)
# 迴圈的數值改成14:14, if後面改成c(14, 36:45)
# 迴圈的數值改成36:45, if不變
# 迴圈的數值改成15:27, if後面改成15:27
# 迴圈的數值改成28:29, if後面改成28:29
# 迴圈的數值改成46:58, if後面改成c(46:58)

for (x in 46:58) {
  miss <- which(data_2019[ , x] == 9)
  print(length(miss))
  
  if (length(miss) >0) {
    data_2019[miss, c(46:58)] <- NA 
  }
}


#------------變數命名------------
# 最後是把變數名稱統一一下
varNam = c(
  "no","year", "resi", "v22_1", "v22_2", "v22_3", "v22_4", "v22_5", 
  "v22_6", "v22_7", "v23", "v23_1", "v25_0", "v25_1", "v30_1", "v30_2", 
  "v30_3", "v30_4", "v30_5", "v30_6", "v30_7", "v30_8", "v30_9", "v30_10",
  "v30_11", "v30_12", "v30_13", "v32_1", "v32_9", "age", "edu", "fami", 
  "occu", "inco", "sex", "v25_2", "v25_3", "v25_4", "v25_5", "v25_6",
  "v25_7", "v25_8", "v25_9", "v25_10", "v25_11", "v26_1", "v26_2", 
  "v26_3", "v26_4", "v26_5", "v26_6", "v26_7", "v26_8", "v26_9", "v26_10",
  "v26_11", "v26_12", "v26_13")
colnames(data_2019) <- varNam
identical(names(data_2016), names(data_2019))


####END of 2019####



####2020 data####

# 讀取檔案，這是2020的raw data
# 使用setwd修改預設路徑後就不用打這麼一長串了（但我懶得改）
raw_2020 <- read_excel("my documents/111-1 傳播導向的大數據分析/期末專案/文化部問卷調查統計數據/2020文化參與及消費問卷調查opendata.xlsx",
                       col_names = TRUE)

# 看看資料狀況
head(raw_2020)

# 用colnames查看欄位名稱
colnames(raw_2020)

# 挑出需要用的欄位，另存成一個資料集
pickVar = c("no1", "v02", "v22_1", "v22_2", "v22_3", "v22_4", "v22_5",
            "v22_6", "v22_7", "v23", "v23_1", "v25_0", "v25_1", "v30_1", 
            "v30_2", "v30_3", "v30_4", "v30_5", "v30_6", "v30_7", "v30_8",
            "v30_9", "v30_10", "v30_11", "v30_12", "v30_13", "v32_1", 
            "v32_9", "v34", "v35", "v36", "v37", "v38", "v39", "v25_2", 
            "v25_3", "v25_4", "v25_5", "v25_6", "v25_7", "v25_8", "v25_9",
            "v25_10", "v25_11", "v26_1", "v26_2", "v26_3", "v26_4", "v26_5",
            "v26_6", "v26_7", "v26_8", "v26_9", "v26_10", "v26_11", "v26_12",
            "v26_13")

data_2020 <- raw_2020[, pickVar]
colnames(data_2020)　　　#確認欄位沒錯


#------------格式處理------------

# 為了和其他年份的資料合併，要先加入一些空的欄位
# emptyCol = rep(NA, nrow(data_2018))　　# 製造空的向量
year = rep(2020, nrow(data_2020))　　# 也會需要加入年份的欄位
data_2020 <- cbind(data_2020[ , 1], year, data_2020[ , 2:57])
ncol(data_2020)
names(data_2020)


#------------遺漏值處理------------

# v26_13, v22_7, v25_11, v30_13, v32_12若為1，表示都沒有（題組其他選項皆應為0）
allNo <- which(raw_2020$v26_13 == 1)　　　　# 這裡修改變數名稱
allNo_sum <- rowSums(data_2020[allNo, 46:57], na.rm = TRUE)　　　　# 這裡修改欄位
length(which(allNo_sum > 0))　　　　# 如果這裡有抓出東西，表示有人填答矛盾，要刪掉
# 上面這個which也會回報出row names，就可以回到data_2017裡面去修改數值
# 以下複製
allNo <- which(raw_2020$v22_7 == 1)　　　　
allNo_sum <- rowSums(data_2020[allNo, c(4:9, 13)], na.rm = TRUE)
length(which(allNo_sum > 0))

allNo <- which(raw_2020$v25_11 == 1)　　　　
allNo_sum <- rowSums(data_2020[allNo, c(14, 36:44)], na.rm = TRUE)
length(which(allNo_sum > 0))

allNo <- which(raw_2020$v30_13 == 1)　　　　
allNo_sum <- rowSums(data_2020[allNo, c(15:26)], na.rm = TRUE)
length(which(allNo_sum > 0))

allNo <- which(raw_2020$v32_12 == 1)　　　　
allNo_sum <- rowSums(data_2020[allNo, c(28:29)], na.rm = TRUE)
length(which(allNo_sum > 0))



# v34, v35, v36, v37, v38, v39若為98 or 99, 表示拒答或漏答
miss <- which(data_2020$v34 >= 98)      # 手動修改要檢查的欄位
length(miss)
data_2020[miss, "v34"] = NA　　　　　　# 手動修改要檢查的欄位

miss <- which(data_2020$v35 >= 98)
length(miss)
data_2020[miss, "v35"] = NA

miss <- which(data_2020$v36 >= 98)
length(miss)
data_2020[miss, "v36"] = NA

miss <- which(data_2020$v37 >= 98)
length(miss)
data_2020[miss, "v37"] = NA

miss <- which(data_2020$v38 >= 98)
length(miss)
data_2020[miss, "v38"] = NA

miss <- which(data_2020$v39 >= 98)
length(miss)
data_2020[miss, "v39"] = NA


# 其他在是非題當中如果出現9的代表有漏答
# v22_1~v22_7, v25_1~v25_11, v30_1~v30_13,v32_1 & v32_9, v26_1~v26_13
# 迴圈的數值改成4:10, if後面改成c(4:10, 13)
# 迴圈的數值改成14:14, if後面改成c(14, 36:45)
# 迴圈的數值改成36:45, if不變
# 迴圈的數值改成15:27, if後面改成15:27
# 迴圈的數值改成28:29, if後面改成28:29
# 迴圈的數值改成46:58, if後面改成c(46:58)

for (x in 46:58) {
  miss <- which(data_2020[ , x] == 9)
  print(length(miss))
  
  if (length(miss) >0) {
    data_2020[miss, c(46:58)] <- NA 
  }
}


#------------變數命名------------
# 最後是把變數名稱統一一下
varNam = c(
  "no","year", "resi", "v22_1", "v22_2", "v22_3", "v22_4", "v22_5", 
  "v22_6", "v22_7", "v23", "v23_1", "v25_0", "v25_1", "v30_1", "v30_2", 
  "v30_3", "v30_4", "v30_5", "v30_6", "v30_7", "v30_8", "v30_9", "v30_10",
  "v30_11", "v30_12", "v30_13", "v32_1", "v32_9", "age", "edu", "fami", 
  "occu", "inco", "sex", "v25_2", "v25_3", "v25_4", "v25_5", "v25_6",
  "v25_7", "v25_8", "v25_9", "v25_10", "v25_11", "v26_1", "v26_2", 
  "v26_3", "v26_4", "v26_5", "v26_6", "v26_7", "v26_8", "v26_9", "v26_10",
  "v26_11", "v26_12", "v26_13")
colnames(data_2020) <- varNam
identical(names(data_2016), names(data_2020))


####END of 2020####



#------------統一編碼------------
# 每一年的題目選項編碼可能有差異，要比對確認
# resi, v34 (age), v35 (edu), v36 (family), v37 (occu), v38 (income), v39 (sex)

# 發現居住地忘記處理遺漏值了
miss <- which(data_2020$resi >= 98)　　　# 手動修改要檢查的欄位
length(miss)
data_2020[miss, "resi"] = NA

# 希望統一縣市順序：
# 基隆市、新北市、台北市、桃園市、新竹縣、新竹市、苗栗縣、台中市、彰化縣、
# 南投縣、雲林縣、嘉義縣、嘉義市、台南市、高雄市、屏東縣、宜蘭縣、花蓮縣、
# 台東縣、澎湖縣、金門縣、連江縣

resi_16 <- factor(data_2016$resi)
levels(resi_16) = c("新北市","台北市","桃園市","台中市","台南市","高雄市",
                    "宜蘭縣","新竹縣","苗栗縣","彰化縣","南投縣","雲林縣",
                    "嘉義縣","屏東縣","台東縣","花蓮縣","澎湖縣","基隆市",
                    "新竹市","嘉義市","金門縣","連江縣")



#------------整合各年資料集------------