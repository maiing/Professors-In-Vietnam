

#lib-----------------------------------------------------------
library(curl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

#db------------------------------------------------------------
raw <- curl("https://raw.githubusercontent.com/chuvanan/data_projects/master/prof-inflation/raw-profs.csv")
file <- read.csv(raw, header=TRUE)

#helper functions----------------------------------------------

explore <- function(var_name) {
  var_exp <- file %>%
    group_by(file$var_name) %>%
    summarise(n=n())
  names(var_exp)[[1]] <- var_name
  
  return(var_exp)
}

#clean----------------------------------------------------------
##gioi tinh
file$gioitinh_upd <- as.character(file$gioitinh)
file$gioitinh_upd[file$gioitinh_upd==" Nữ"] <- "Nữ"
file$gioitinh_upd[file$gioitinh_upd=="NAM"] <- "Nam"
file$gioitinh_upd[file$gioitinh_upd==""] <- "Chưa rõ"

gender <- explore("gioitinh_upd")

##nganh
file$nganh_upd <- as.character(file$nganh)
file$nganh_upd <- tolower(file$nganh_upd)

file$nganh_upd <- gsub("(\\â|á|à|ã|ả|ạ|ă|á|à|ạ|ã|ả|ắ|ằ|ặ|ẵ|ẳ|ấ|ầ|ẫ|ẩ|ậ)", "a", file$nganh_upd)
file$nganh_upd <- gsub("(ó|ò|õ|ỏ|ọ|ô|ơ|ố|ồ|ộ|ỗ|ổ|ớ|ờ|ợ|ỡ|ở)", "o", file$nganh_upd)
file$nganh_upd <- gsub("(é|è|ẽ|ẻ|ẹ|ê|ế|ề|ệ|ễ|ể)", "e", file$nganh_upd)
file$nganh_upd <- gsub("(ú|ù|ụ|ũ|ủ|ư|ứ|ừ|ự|ữ|ử)", "u", file$nganh_upd)
file$nganh_upd <- gsub("(í|ì|ị|ĩ|ỉ)", "i", file$nganh_upd)
file$nganh_upd <- gsub("(ỳ|ý|ỷ|ỹ|ỵ)", "y", file$nganh_upd)
file$nganh_upd <- gsub("\\đ", "d", file$nganh_upd)
file$nganh_upd <- gsub("(\\(|\\)|\\-)", "", file$nganh_upd)

file$nganh_upd <- as.character(str_trim(file$nganh_upd)) 

file$nganh_upd[grepl("dong luc", file$nganh_upd)] <- "dong luc hoc"
file$nganh_upd[grepl("duoc", file$nganh_upd)] <- "duoc hoc"
file$nganh_upd[grepl("giao thong", file$nganh_upd)] <- "giao thong van tai"
file$nganh_upd[grepl("kh an ninh", file$nganh_upd)] <- "khoa hoc an ninh"
file$nganh_upd[grepl("kinh te", file$nganh_upd)] <- "kinh te hoc"
file$nganh_upd[grepl("luat", file$nganh_upd)] <- "luat hoc"
file$nganh_upd[grepl("mo", file$nganh_upd)] <- "khoa hoc mo"
file$nganh_upd[grepl("(su|lich su)", file$nganh_upd)] <- "su hoc"
file$nganh_upd[grepl("tam li hoc", file$nganh_upd)] <- "tam ly hoc"
file$nganh_upd[grepl("van hoa", file$nganh_upd)] <- "van hoa hoc"
file$nganh_upd[grepl("vat ly", file$nganh_upd)] <- "vat ly hoc"
file$nganh_upd[grepl("the duc  the thao", file$nganh_upd)] <- "the duc the thao"
file$nganh_upd[grepl("triet  hoc", file$nganh_upd)] <- "triet hoc"

major <- explore("nganh_upd")

##decade (Add new var)
file$decade <- as.character(file$nam)
file$decade <- ifelse(substr(file$nam,3,3)=="8", "1980s", 
                      ifelse(substr(file$nam,3,3)=="9", "1990s", 
                             ifelse(substr(file$nam,3,3)=="0", "2000s", 
                                    ifelse(substr(file$nam,3,3)=="1", "2010s", 0))))

decade <- explore("decade")
year <- explore("nam")
no_major <- file %>%
  group_by(decade) %>%
  summarise(n=length(unique(nganh_upd))) 


##ma gs
file$mags_upd <- file$maso_gcn

file$mags_upd <- gsub("[[:punct:]]", "", file$mags_upd)
file$mags_upd <- gsub("\\d", "", file$mags_upd)

file$mags_upd[grepl("NLGS", file$mags_upd)] <- "GS"
file$mags_upd[grepl("NLPGS", file$mags_upd)] <- "PGS"

title <- explore("mags_upd")

#analyse------------------------------------------------------------------------

##gender vs. major==============================================================
gen_major <- file %>%
  group_by(nganh_upd, gioitinh_upd) %>%
  summarise(n=n())

gen_major <- left_join(gen_major, major, by="nganh_upd")
  
# plot1 <- ggplot(gen_major, aes(x=reorder(nganh_upd, sum), y=n, fill=gioitinh_upd)) +
#   geom_bar(stat='identity') +
#   theme_minimal() +
#   labs(y = "Số lượng GS/PGS", x = element_blank()) +
#   theme(axis.title.y = element_text(size = 7)) +
#   theme(axis.text.x = element_text(angle = 75, hjust=1, face = "italic", size = 7)) +
#   theme(axis.text.y = element_text(size = 8)) +
#   guides(fill=guide_legend(title = NULL)) +
#   theme(panel.grid.minor.x = element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         panel.grid.major.y = element_line(color = "gray90")) +
#   scale_fill_manual(values = c("lavenderblush", "lightblue3", "lightcoral")) +
#   scale_y_continuous(breaks = seq(0, 2000, by=250))
# print(plot1)

spread_gen_major <- spread(gen_major, gioitinh_upd, n)
spread_gen_major[is.na(spread_gen_major)] <- 0

spread_gen_major$tyle <- round(spread_gen_major$`Nữ`/spread_gen_major$sum*100, digits=2)

plot2 <- ggplot(spread_gen_major, aes(x=reorder(nganh_upd, tyle), y=tyle)) +
  geom_bar(stat='identity', fill="lightcoral") +
  theme_minimal() +
  labs(y = "% nữ GS/PGS", x = element_blank()) +
  theme(axis.title.y = element_text(size = 7)) +
  theme(axis.text.x = element_text(angle = 75, hjust=1, face = "italic", size = 7)) +
  theme(axis.text.y = element_text(size = 8)) +
  guides(fill=guide_legend(title = NULL)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "gray90")) 
  #scale_y_continuous(breaks = seq(0, 2000, by=250))
print(plot2)

##gender vs. time===============================================================

gen_year <- file %>%
  group_by(nam, gioitinh_upd) %>%
  summarise(n=n())

gen_year <- left_join(gen_year, year, by="nam")

plot3 <- ggplot(gen_year, aes(x=nam, y=n.x, fill=gioitinh_upd)) +
  geom_bar(stat='identity') +
  theme_minimal() +
  labs(y = "Số lượng GS/PGS", x = element_blank()) +
  theme(axis.title.y = element_text(size = 7)) +
  theme(axis.text.x = element_text(angle = 75, hjust=1, face = "italic", size = 7)) +
  theme(axis.text.y = element_text(size = 8)) +
  guides(fill=guide_legend(title = NULL)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "gray90")) +
  scale_fill_manual(values = c("lavenderblush", "lightblue3", "lightcoral")) +
  scale_y_continuous(breaks = seq(0, 2000, by=250))
print(plot3)

spread_gen_year <- spread(gen_year, gioitinh_upd, n.x)
spread_gen_year[is.na(spread_gen_year)] <- 0

spread_gen_year$tyle <- round(spread_gen_year$`Nữ`/spread_gen_year$n.y*100, digits=2)

plot4 <- ggplot(spread_gen_year, aes(x=nam, y=tyle)) +
  geom_bar(stat='identity', fill="lightcoral") +
  theme_minimal() +
  labs(y = "% nữ GS/PGS", x = element_blank()) +
  theme(axis.title.y = element_text(size = 7)) +
  theme(axis.text.x = element_text(angle = 75, hjust=1, face = "italic", size = 7)) +
  theme(axis.text.y = element_text(size = 8)) +
  guides(fill=guide_legend(title = NULL)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "gray90")) +
  scale_y_continuous(breaks = seq(0, 30, by=5))

print(plot4)

##decade vs major ==============================================================

spread_major_decade <- file %>%
  group_by(decade, nganh_upd) %>%
  summarise(n=n()) %>%
  spread(nganh_upd, n)
spread_major_decade[is.na(spread_major_decade)] <- 0
major_decade <- gather(spread_major_decade, "nganh_upd", "n", 2:ncol(spread_major_decade))

  
# plot5 <- ggplot(major_decade, aes(x=reorder(nganh_upd,n), y=n, group=decade)) +
#   geom_bar(position="dodge", stat="identity") +
#   theme_minimal() +
#   labs(y = "Số lượng GS/PGS", x = element_blank()) +
#   theme(axis.title.y = element_text(size = 7)) +
#   theme(axis.text.x = element_text(angle = 75, hjust=1, face = "italic", size = 7)) +
#   theme(axis.text.y = element_text(size = 8)) +
#   guides(fill=guide_legend(title = NULL)) +
#   theme(panel.grid.minor.x = element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         panel.grid.major.y = element_line(color = "gray90")) +
#   guides(fill=FALSE) +
#   facet_grid(.~decade,scales="free")
#   #scale_y_continuous(breaks = seq(0, 30, by=5))
# 
# print(plot5)

plot6 <- ggplot(major_decade, aes(y=n, x=reorder(nganh_upd,n), group=decade)) +
  geom_bar(position="dodge",stat="identity") +
  geom_text(aes(label=n), size=1.8, hjust=-0.2) +
  coord_flip() +
  theme_bw() +
  labs(y = "Số lượng GS/PGS", x = element_blank()) +
  theme(axis.title.x = element_text(size = 7)) +
  theme(axis.text.x = element_text(angle = 75, hjust=1, face = "italic", size = 6)) +
  theme(axis.text.y = element_text(size = 6)) +
  guides(fill=guide_legend(title = NULL)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "gray90")) +
  guides(fill=FALSE) +
  facet_grid(.~decade,scales="free") 
  #scale_y_continuous(breaks = seq(0, 600, by=100))

print(plot6)





