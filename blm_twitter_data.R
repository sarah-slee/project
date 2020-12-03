# blm_twitter_data.R
# November 24, 2020

# load easypackages and various other packages into the library
library(easypackages)
packages("dplyr", "lubridate", "RColorBrewer", "rtweet", "tm", "wordcloud", prompt = T)

# store api keys
api_key <- "9vJajVK1XCsr6e4SFxesKAehe"
api_key_secret <- "Y0knfwIEl1AzHgGmqLNv2fw3bteFUSd6suX3yNPRxPeVpQqKNB"
access_token = "338782445-Bsda9peV2Xs28kkWf4b4WKGNWHU09Tny1wn2krdE"
access_token_secret = "W93VnXAkqqoJ6pCkLdq5a8xieLTS4jSO1FVBHmvo0D6h3"

# check the token
rtweet::get_token()  

# authenticate via web browser
token <- rtweet::create_token(
  app = "blm_twitter_data",
  consumer_key <- api_key,
  consumer_secret <- api_key_secret,
  access_token <- access_token,
  access_secret <- access_token_secret)

# check to see if the token is loaded
get_token()

# anarchist data collection

## individual anarchist data
jam_eli <- get_timeline("jamie_elizabeth", n = 5000, lang = "en")
ana_fed <- get_timeline("AnarchistFed", n = 5000, lang = "en")
ind_ana <- get_timeline("IAF__FAI", n = 5000, lang = "en")
bla_ros <- get_timeline("BRRN_Fed", n = 5000, lang = "en")
soc_rif <- get_timeline("SocialistRA", n = 5000, lang = "en")
bla_soc <- get_timeline("BlackSocialists", n = 5000, lang = "en")
rev_abo <- get_timeline("RevAbolitionNYC", n = 5000, lang = "en")
zoe_sam <- get_timeline("ztsamudzi", n = 5000, lang = "en")

## aggregated anarchist data
anarchist <- rbind(jam_eli, ana_fed, ind_ana, bla_ros, soc_rif, bla_soc, 
                   rev_abo, zoe_sam)
anarchist = as.data.frame(sapply(anarchist, tolower))
anarchist_logical <- grepl("black lives", anarchist$text) | 
  grepl("all lives", anarchist$text) | grepl("blue lives", anarchist$text) | 
  grepl("blm", anarchist$text) | grepl("alm", anarchist$text) | 
  grepl("george floyd", anarchist$text) | grepl("chauvin", anarchist$text) | 
  grepl("police", anarchist$text) | grepl("cops", anarchist$text) | 
  grepl("pigs", anarchist$text) | grepl("qualified immunity", anarchist$text) | 
  grepl("racism", anarchist$text) | grepl("racist", anarchist$text) | 
  grepl("racists", anarchist$text) | grepl("white supremacist", anarchist$text) | 
  grepl("white supremacists", anarchist$text) | grepl("rittenhouse", anarchist$text) | 
  grepl("breonna", anarchist$text) | grepl("mattingly", anarchist$text) | 
  grepl("kenneth walker", anarchist$text) | grepl("elijah", anarchist$text) | 
  grepl("ahmaud", anarchist$text) | grepl("antifa", anarchist$text) | 
  grepl("protest", anarchist$text) | grepl("protests", anarchist$text) | 
  grepl("protester", anarchist$text) | grepl("protesters", anarchist$text) | 
  grepl("protesting", anarchist$text) | grepl("riot", anarchist$text) | 
  grepl("riots", anarchist$text) | grepl("rioter", anarchist$text) | 
  grepl("rioters", anarchist$text) | grepl("rioting", anarchist$text) | 
  grepl("looter", anarchist$text) | grepl("looters", anarchist$text) | 
  grepl("looting", anarchist$text) | grepl("curfew", anarchist$text) | 
  grepl("demonstration", anarchist$text) | grepl("demonstrations", anarchist$text) | 
  grepl("tear gas", anarchist$text) | grepl("baton", anarchist$text) | 
  grepl("rubber bullet", anarchist$text) | grepl("rubber bullets", anarchist$text) | 
  grepl("autonomous zone", anarchist$text) | grepl("unmarked", anarchist$text) | 
  grepl("peaceful", anarchist$text) | grepl("violent", anarchist$text) | 
  grepl("violence", anarchist$text) | grepl("vandalize", anarchist$text) | 
  grepl("vandalized", anarchist$text) | grepl("vandalizing", anarchist$text) | 
  grepl("confederate", anarchist$text) | grepl("monument", anarchist$text) | 
  grepl("statue", anarchist$text) 
anarchist_tweets <- cbind(anarchist, anarchist_logical)
anarchist_blm <- anarchist %>% filter(anarchist_logical == "TRUE")
anarchist_blm$created_at <- as.POSIXct(anarchist_blm$created_at)
anarchist_blm_gf <- anarchist_blm %>% filter(created_at > ymd("2020-05-25"))

# anarchist data visualization 

## time series
ts_plot(anarchist_blm_gf, by = "days") + 
  labs(x = NULL, y = NULL, 
       title = "Frequency of tweets by anarchists on BLM and systemic racism", 
       caption = "Data from the last 3,200 tweets of various anarchist Twitter users since May 25, 2020") + 
  theme_minimal() + geom_line(color = "seagreen3") + 
  theme(text = element_text(size = 16))

## world cloud

## create a vector source for anarchist
anarchist_vector <- VectorSource(anarchist_blm_gf$text)

## check the class of anarchist_vector
class(anarchist_vector)

## create a corpus object
anarchist_corpus <- Corpus(anarchist_vector)

## make the data uniform (no punctuation/number)
anarchist_corpus <- tm_map(anarchist_corpus, removePunctuation)
anarchist_corpus <- tm_map(anarchist_corpus, removeNumbers)

## remove stopwords
anarchist_corpus <- tm_map(anarchist_corpus, removeWords, stopwords("english"))

## create the term document matrix 
anarchist_tdm <- TermDocumentMatrix(anarchist_corpus)
anarchist_matrix = as.matrix(anarchist_tdm)
anarchist_wordcount <- rowSums(anarchist_matrix)
anarchist_wordcount <- sort(anarchist_wordcount, decreasing = TRUE)
head(anarchist_wordcount)

## create a wordcloud
anarchist_cloud <- data.frame(word = names(anarchist_wordcount), 
                              freq = anarchist_wordcount)
set.seed(1234)
wordcloud(names(anarchist_wordcount), anarchist_wordcount, min.freq = 1,
          random.order = FALSE, max.words = 100, scale = c(4, 0.5), 
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

## most tweets in a single day by anarchists
anarchist_blm_gf %>% count(created_at, sort = TRUE) %>% top_n(5)

# far leftist data collection

## individual far leftist data
oli_tho <- get_timeline("PhilosophyTube", n = 5000, lang = "en")
har_bre <- get_timeline("Hbomberguy", n = 5000, lang = "en")
cod_joh <- get_timeline("drmistercody", n = 5000, lang = "en")
ric_wol <- get_timeline("profwolff", n = 5000, lang = "en")
cor_wes <- get_timeline("CornelWest", n = 5000, lang = "en")
ben_dix <- get_timeline("BenjaminPDixon", n = 5000, lang = "en")
mat_bor <- get_timeline("MattBors", n = 5000, lang = "en")
isr_hir <- get_timeline("israhirsi", n = 5000, lang = "en")
kat_hal <- get_timeline("kthalps", n = 5000, lang = "en")
owe_jon <- get_timeline("OwenJones84", n = 5000, lang = "en")
ben_bur <- get_timeline("BenBurgis", n = 5000, lang = "en")
has_pik <- get_timeline("hasanthehun", n = 5000, lang = "en")
vaush_v <- get_timeline("VaushV", n = 5000, lang = "en")
tim_fau <- get_timeline("crulge", n = 5000, lang = "en")
bri_gre <- get_timeline("briebriejoy", n = 5000, lang = "en")
nat_ber <- get_timeline("nathanTbernard", n = 5000, lang = "en")
rob_del <- get_timeline("robdelaney", n = 5000, lang = "en")
phe_cal <- get_timeline("uppitynegress", n = 5000, lang = "en")
bha_sun <- get_timeline("sunraysunray", n = 5000, lang = "en")
dav_kli <- get_timeline("DavidKlion", n = 5000, lang = "en")
vic_ber <- get_timeline("VicBergerIV", n = 5000, lang = "en")
kra_nel <- get_timeline("KrangTNelson", n = 5000, lang = "en")
wil_gee <- get_timeline("classiclib3ral", n = 5000, lang = "en")
rob_rou <- get_timeline("robrousseau", n = 5000, lang = "en")
wil_men <- get_timeline("willmenaker", n = 5000, lang = "en")
mat_chr <- get_timeline("cushbomb", n = 5000, lang = "en")
fel_bie <- get_timeline("ByYourLogic", n = 5000, lang = "en")

## aggregated far leftist data
farleftist <- rbind(oli_tho, har_bre, cod_joh, ric_wol, cor_wes, ben_dix, 
                    mat_bor, isr_hir, kat_hal, owe_jon, ben_bur, has_pik, 
                    vaush_v, tim_fau, bri_gre, nat_ber, rob_del, phe_cal, 
                    bha_sun, dav_kli, vic_ber, kra_nel, wil_gee, rob_rou,
                    wil_men, mat_chr, fel_bie)
farleftist = as.data.frame(sapply(farleftist, tolower))
farleftist_logical <- grepl("black lives", farleftist$text) | 
  grepl("all lives", farleftist$text) | grepl("blue lives", farleftist$text) | 
  grepl("blm", farleftist$text) | grepl("alm", farleftist$text) | 
  grepl("george floyd", farleftist$text) | grepl("chauvin", farleftist$text) | 
  grepl("police", farleftist$text) | grepl("cops", farleftist$text) | 
  grepl("pigs", farleftist$text) | grepl("qualified immunity", farleftist$text) | 
  grepl("racism", farleftist$text) | grepl("racist", farleftist$text) | 
  grepl("racists", farleftist$text) | grepl("white supremacist", farleftist$text) | 
  grepl("white supremacists", farleftist$text) | grepl("rittenhouse", farleftist$text) | 
  grepl("breonna", farleftist$text) | grepl("mattingly", farleftist$text) | 
  grepl("kenneth walker", farleftist$text) | grepl("elijah", farleftist$text) | 
  grepl("ahmaud", farleftist$text) | grepl("antifa", farleftist$text) | 
  grepl("protest", farleftist$text) | grepl("protests", farleftist$text) | 
  grepl("protester", farleftist$text) | grepl("protesters", farleftist$text) | 
  grepl("protesting", farleftist$text) | grepl("riot", farleftist$text) | 
  grepl("riots", farleftist$text) | grepl("rioter", farleftist$text) | 
  grepl("rioters", farleftist$text) | grepl("rioting", farleftist$text) | 
  grepl("looter", farleftist$text) | grepl("looters", farleftist$text) | 
  grepl("looting", farleftist$text) | grepl("curfew", farleftist$text) | 
  grepl("demonstration", farleftist$text) | grepl("demonstrations", farleftist$text) | 
  grepl("tear gas", farleftist$text) | grepl("baton", farleftist$text) | 
  grepl("rubber bullet", farleftist$text) | grepl("rubber bullets", farleftist$text) | 
  grepl("autonomous zone", farleftist$text) | grepl("unmarked", farleftist$text) | 
  grepl("peaceful", farleftist$text) | grepl("violent", farleftist$text) | 
  grepl("violence", farleftist$text) | grepl("vandalize", farleftist$text) | 
  grepl("vandalized", farleftist$text) | grepl("vandalizing", farleftist$text) | 
  grepl("confederate", farleftist$text) | grepl("monument", farleftist$text) | 
  grepl("statue", farleftist$text) 
farleftist_tweets <- cbind(farleftist, farleftist_logical)
farleftist_blm <- farleftist %>% filter(farleftist_logical == "TRUE")
farleftist_blm$created_at <- as.POSIXct(farleftist_blm$created_at)
farleftist_blm_gf <- farleftist_blm %>% filter(created_at > ymd("2020-05-25"))

# far leftist data visualization 

## time series
ts_plot(farleftist_blm_gf, by = "days") + 
  labs(x = NULL, y = NULL, 
       title = "Frequency of tweets by far leftists on BLM and systemic racism", 
       caption = "Data from the last 3,200 tweets of various far leftist Twitter users since May 25, 2020") + 
  theme_minimal() + geom_line(color = "seagreen3") + 
  theme(text = element_text(size = 16))

## most tweets in a single day by far leftists
farleftist_blm_gf %>% count(created_at, sort = TRUE) %>% top_n(5)

## world cloud

## create a vector source for farleftist
farleftist_vector <- VectorSource(farleftist_blm_gf$text)

## check the class of farleftist_vector
class(farleftist_vector)

## create a corpus object
farleftist_corpus <- Corpus(farleftist_vector)

## make the data uniform (no punctuation/number)
farleftist_corpus <- tm_map(farleftist_corpus, removePunctuation)
farleftist_corpus <- tm_map(farleftist_corpus, removeNumbers)

## remove stopwords
farleftist_corpus <- tm_map(farleftist_corpus, removeWords, stopwords("english"))

## create the term document matrix 
farleftist_tdm <- TermDocumentMatrix(farleftist_corpus)
farleftist_matrix = as.matrix(farleftist_tdm)
farleftist_wordcount <- rowSums(farleftist_matrix)
farleftist_wordcount <- sort(farleftist_wordcount, decreasing = TRUE)
head(farleftist_wordcount)

## create a wordcloud
farleftist_cloud <- data.frame(word = names(farleftist_wordcount), 
                              freq = farleftist_wordcount)
set.seed(1234)
wordcloud(names(farleftist_wordcount), farleftist_wordcount, min.freq = 1,
          random.order = FALSE, max.words = 100, scale = c(4, 0.5), 
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

## most tweets in a single day by far leftists
farleftist_blm_gf$created_at = as.Date(farleftist_blm_gf$created_at, format('ymd'))
farleftist_blm_gf %>% count(created_at, sort = TRUE) %>% top_n(5)

# moderate leftist data collection

## individual moderate leftist data
gle_gre <- get_timeline("ggreenwald", n = 5000, lang = "en")
lee_fan <- get_timeline("lhfang", n = 5000, lang = "en")
our_rev <- get_timeline("OurRevolution", n = 5000, lang = "en")
kyl_kul <- get_timeline("KyleKulinski", n = 5000, lang = "en")
nat_wyn <- get_timeline("ContraPoints", n = 5000, lang = "en")
ber_san <- get_timeline("BernieSanders", n = 5000, lang = "en")
ale_oca <- get_timeline("AOC", n = 5000, lang = "en")
ilh_oma <- get_timeline("Ilhan", n = 5000, lang = "en")
ras_tla <- get_timeline("RashidaTlaib", n = 5000, lang = "en")
aya_pre <- get_timeline("AyannaPressley", n = 5000, lang = "en")
kry_bal <- get_timeline("krystalball", n = 5000, lang = "en")
mal_jab <- get_timeline("MalaikaJabali", n = 5000, lang = "en")
jor_uhl <- get_timeline("jordanuhl", n = 5000, lang = "en")
gra_ins <- get_timeline("GravelInstitute", n = 5000, lang = "en")

## aggregated moderate leftist data
modleftist <- rbind(gle_gre, lee_fan, our_rev, kyl_kul, nat_wyn, ber_san, 
                    ale_oca, ilh_oma, ras_tla, aya_pre, kry_bal, mal_jab, 
                    jor_uhl, gra_ins)
modleftist = as.data.frame(sapply(modleftist, tolower))
modleftist_logical <- grepl("black lives", modleftist$text) | 
  grepl("all lives", modleftist$text) | grepl("blue lives", modleftist$text) | 
  grepl("blm", modleftist$text) | grepl("alm", modleftist$text) | 
  grepl("george floyd", modleftist$text) | grepl("chauvin", modleftist$text) | 
  grepl("police", modleftist$text) | grepl("cops", modleftist$text) | 
  grepl("pigs", modleftist$text) | grepl("qualified immunity", modleftist$text) | 
  grepl("racism", modleftist$text) | grepl("racist", modleftist$text) | 
  grepl("racists", modleftist$text) | grepl("white supremacist", modleftist$text) | 
  grepl("white supremacists", modleftist$text) | grepl("rittenhouse", modleftist$text) | 
  grepl("breonna", modleftist$text) | grepl("mattingly", modleftist$text) | 
  grepl("kenneth walker", modleftist$text) | grepl("elijah", modleftist$text) | 
  grepl("ahmaud", modleftist$text) | grepl("antifa", modleftist$text) | 
  grepl("protest", modleftist$text) | grepl("protests", modleftist$text) | 
  grepl("protester", modleftist$text) | grepl("protesters", modleftist$text) | 
  grepl("protesting", modleftist$text) | grepl("riot", modleftist$text) | 
  grepl("riots", modleftist$text) | grepl("rioter", modleftist$text) | 
  grepl("rioters", modleftist$text) | grepl("rioting", modleftist$text) | 
  grepl("looter", modleftist$text) | grepl("looters", modleftist$text) | 
  grepl("looting", modleftist$text) | grepl("curfew", modleftist$text) | 
  grepl("demonstration", modleftist$text) | grepl("demonstrations", modleftist$text) | 
  grepl("tear gas", modleftist$text) | grepl("baton", modleftist$text) | 
  grepl("rubber bullet", modleftist$text) | grepl("rubber bullets", modleftist$text) | 
  grepl("autonomous zone", modleftist$text) | grepl("unmarked", modleftist$text) | 
  grepl("peaceful", modleftist$text) | grepl("violent", modleftist$text) | 
  grepl("violence", modleftist$text) | grepl("vandalize", modleftist$text) | 
  grepl("vandalized", modleftist$text) | grepl("vandalizing", modleftist$text) | 
  grepl("confederate", modleftist$text) | grepl("monument", modleftist$text) | 
  grepl("statue", modleftist$text) 
modleftist_tweets <- cbind(modleftist, modleftist_logical)
modleftist_blm <- modleftist %>% filter(modleftist_logical == "TRUE")
modleftist_blm$created_at <- as.POSIXct(modleftist_blm$created_at)
modleftist_blm_gf <- modleftist_blm %>% filter(created_at > ymd("2020-05-25"))

# moderate leftist data visualization 

## time series
ts_plot(modleftist_blm_gf, by = "days") + 
  labs(x = NULL, y = NULL, 
       title = "Frequency of tweets by moderate leftists on BLM and systemic racism", 
       caption = "Data from the last 3,200 tweets of various moderate leftist Twitter users since May 25, 2020") + 
  theme_minimal() + geom_line(color = "seagreen3") + 
  theme(text = element_text(size = 16))

## world cloud

## create a vector source for modleftist
modleftist_vector <- VectorSource(modleftist_blm_gf$text)

## check the class of modleftist_vector
class(modleftist_vector)

## create a corpus object
modleftist_corpus <- Corpus(modleftist_vector)

## make the data uniform (no punctuation/number)
modleftist_corpus <- tm_map(modleftist_corpus, removePunctuation)
modleftist_corpus <- tm_map(modleftist_corpus, removeNumbers)

## remove stopwords
modleftist_corpus <- tm_map(modleftist_corpus, removeWords, stopwords("english"))

## create the term document matrix 
modleftist_tdm <- TermDocumentMatrix(modleftist_corpus)
modleftist_matrix = as.matrix(modleftist_tdm)
modleftist_wordcount <- rowSums(modleftist_matrix)
modleftist_wordcount <- sort(modleftist_wordcount, decreasing = TRUE)
head(modleftist_wordcount)

## create a wordcloud
modleftist_cloud <- data.frame(word = names(modleftist_wordcount), 
                               freq = modleftist_wordcount)
set.seed(1234)
wordcloud(names(modleftist_wordcount), modleftist_wordcount, min.freq = 1,
          random.order = FALSE, max.words = 100, scale = c(4, 0.5), 
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

## most tweets in a single day by moderate leftists
modleftist_blm_gf$created_at = as.Date(modleftist_blm_gf$created_at, format('ymd'))
modleftist_blm_gf %>% count(created_at, sort = TRUE) %>% top_n(5)

# left-leaning progressive data collection

## individual left-leaning progressive data
cen_uyg <- get_timeline("cenkuygur", n = 5000, lang = "en")
ana_kas <- get_timeline("AnaKasparian", n = 5000, lang = "en")
emm_vig <- get_timeline("EmmaVigeland", n = 5000, lang = "en")
kev_jan <- get_timeline("the1janitor", n = 5000, lang = "en")
pod_sav <- get_timeline("PodSaveAmerica", n = 5000, lang = "en")
jun_lap <- get_timeline("shoe0nhead", n = 5000, lang = "en")
christo <- get_timeline("Halalcoholism", n = 5000, lang = "en")
ant_fan <- get_timeline("theneedledrop", n = 5000, lang = "en")
mar_ruf <- get_timeline("MarkRuffalo", n = 5000, lang = "en")
geo_sor <- get_timeline("georgesoros", n = 5000, lang = "en")
sta_abr <- get_timeline("staceyabrams", n = 5000, lang = "en")
sus_sar <- get_timeline("SusanSarandon", n = 5000, lang = "en")
tul_gab <- get_timeline("TulsiGabbard", n = 5000, lang = "en")
nin_tur <- get_timeline("ninaturner", n = 5000, lang = "en")
ken_kli <- get_timeline("kenklippenstein", n = 5000, lang = "en")
pau_kru <- get_timeline("paulkrugman", n = 5000, lang = "en")
mic_tra <- get_timeline("mtracey", n = 5000, lang = "en")
mic_moo <- get_timeline("MMFlint", n = 5000, lang = "en")
ann_app <- get_timeline("anneapplebaum", n = 5000, lang = "en")
nic_kri <- get_timeline("NickKristof", n = 5000, lang = "en")
joh_fug <- get_timeline("JohnFugelsang", n = 5000, lang = "en")
dun_bla <- get_timeline("Atrios", n = 5000, lang = "en")
eli_war <- get_timeline("ewarren", n = 5000, lang = "en")
ro_kann <- get_timeline("RoKhanna", n = 5000, lang = "en")
joy_rei <- get_timeline("JoyAnnReid", n = 5000, lang = "en")
rac_mad <- get_timeline("maddow", n = 5000, lang = "en")
ezr_kle <- get_timeline("ezraklein", n = 5000, lang = "en")
jua_col <- get_timeline("jricole", n = 5000, lang = "en")
and_yan <- get_timeline("AndrewYang", n = 5000, lang = "en")
cha_boo <- get_timeline("Booker4KY", n = 5000, lang = "en")

## aggregated left-leaning progressive data
leftprog <- rbind(cen_uyg, ana_kas, emm_vig, kev_jan, pod_sav, jun_lap, 
                  christo, ant_fan, mar_ruf, geo_sor, sta_abr, sus_sar, 
                  tul_gab, nin_tur, ken_kli, pau_kru, mic_tra, mic_moo,
                  ann_app, nic_kri, joh_fug, dun_bla, eli_war, ro_kann, 
                  joy_rei, rac_mad, ezr_kle, jua_col, and_yan, cha_boo)
leftprog = as.data.frame(sapply(leftprog, tolower))
leftprog_logical <- grepl("black lives", leftprog$text) | 
  grepl("all lives", leftprog$text) | grepl("blue lives", leftprog$text) | 
  grepl("blm", leftprog$text) | grepl("alm", leftprog$text) | 
  grepl("george floyd", leftprog$text) | grepl("chauvin", leftprog$text) | 
  grepl("police", leftprog$text) | grepl("cops", leftprog$text) | 
  grepl("pigs", leftprog$text) | grepl("qualified immunity", leftprog$text) | 
  grepl("racism", leftprog$text) | grepl("racist", leftprog$text) | 
  grepl("racists", leftprog$text) | grepl("white supremacist", leftprog$text) | 
  grepl("white supremacists", leftprog$text) | grepl("rittenhouse", leftprog$text) | 
  grepl("breonna", leftprog$text) | grepl("mattingly", leftprog$text) | 
  grepl("kenneth walker", leftprog$text) | grepl("elijah", leftprog$text) | 
  grepl("ahmaud", leftprog$text) | grepl("antifa", leftprog$text) | 
  grepl("protest", leftprog$text) | grepl("protests", leftprog$text) | 
  grepl("protester", leftprog$text) | grepl("protesters", leftprog$text) | 
  grepl("protesting", leftprog$text) | grepl("riot", leftprog$text) | 
  grepl("riots", leftprog$text) | grepl("rioter", leftprog$text) | 
  grepl("rioters", leftprog$text) | grepl("rioting", leftprog$text) | 
  grepl("looter", leftprog$text) | grepl("looters", leftprog$text) | 
  grepl("looting", leftprog$text) | grepl("curfew", leftprog$text) | 
  grepl("demonstration", leftprog$text) | grepl("demonstrations", leftprog$text) | 
  grepl("tear gas", leftprog$text) | grepl("baton", leftprog$text) | 
  grepl("rubber bullet", leftprog$text) | grepl("rubber bullets", leftprog$text) | 
  grepl("autonomous zone", leftprog$text) | grepl("unmarked", leftprog$text) | 
  grepl("peaceful", leftprog$text) | grepl("violent", leftprog$text) | 
  grepl("violence", leftprog$text) | grepl("vandalize", leftprog$text) | 
  grepl("vandalized", leftprog$text) | grepl("vandalizing", leftprog$text) | 
  grepl("confederate", leftprog$text) | grepl("monument", leftprog$text) | 
  grepl("statue", leftprog$text) 
leftprog_tweets <- cbind(leftprog, leftprog_logical)
leftprog_blm <- leftprog %>% filter(leftprog_logical == "TRUE")
leftprog_blm$created_at <- as.POSIXct(leftprog_blm$created_at)
leftprog_blm_gf <- leftprog_blm %>% filter(created_at > ymd("2020-05-25"))

# left-leaning progressive data visualization 

## time series
ts_plot(leftprog_blm_gf, by = "days") + 
  labs(x = NULL, y = NULL, 
       title = "Frequency of tweets by left-leaning progressives on BLM and systemic racism", 
       caption = "Data from the last 3,200 tweets of various left-leaning progressive Twitter users since May 25, 2020") + 
  theme_minimal() + geom_line(color = "seagreen3") + 
  theme(text = element_text(size = 16))

## world cloud

## create a vector source for leftprog
leftprog_vector <- VectorSource(leftprog_blm_gf$text)

## check the class of leftprog_vector
class(leftprog_vector)

## create a corpus object
leftprog_corpus <- Corpus(leftprog_vector)

## make the data uniform (no punctuation/number)
leftprog_corpus <- tm_map(leftprog_corpus, removePunctuation)
leftprog_corpus <- tm_map(leftprog_corpus, removeNumbers)

## remove stopwords
leftprog_corpus <- tm_map(leftprog_corpus, removeWords, stopwords("english"))

## create the term document matrix 
leftprog_tdm <- TermDocumentMatrix(leftprog_corpus)
leftprog_matrix = as.matrix(leftprog_tdm)
leftprog_wordcount <- rowSums(leftprog_matrix)
leftprog_wordcount <- sort(leftprog_wordcount, decreasing = TRUE)
head(leftprog_wordcount)

## create a wordcloud
leftprog_cloud <- data.frame(word = names(leftprog_wordcount), 
                             freq = leftprog_wordcount)
set.seed(1234)
wordcloud(names(leftprog_wordcount), leftprog_wordcount, min.freq = 1,
          random.order = FALSE, max.words = 100, scale = c(4, 0.5), 
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

## most tweets in a single day by left-leaning progressives
leftprog_blm_gf$created_at = as.Date(leftprog_blm_gf$created_at, format('ymd'))
leftprog_blm_gf %>% count(created_at, sort = TRUE) %>% top_n(5)

# social justice progressive data collection

## individual social justice progressive data
nat_lgb <- get_timeline("TheTaskForce", n = 5000, lang = "en")
lgb_gla <- get_timeline("glaad", n = 5000, lang = "en")
ass_ame <- get_timeline("IndianAffairs", n = 5000, lang = "en")
ame_civ <- get_timeline("ACLU", n = 5000, lang = "en")
spl_cen <- get_timeline("splcenter", n = 5000, lang = "en")
cli_smi <- get_timeline("ClintSmithIII", n = 5000, lang = "en")
rox_gay <- get_timeline("rgay", n = 5000, lang = "en")
kat_gri <- get_timeline("kathygriffin", n = 5000, lang = "en")
jel_cob <- get_timeline("jelani9", n = 5000, lang = "en")
joh_lew <- get_timeline("repjohnlewis", n = 5000, lang = "en")
art_chu <- get_timeline("arthur_affect", n = 5000, lang = "en")
bria_wu <- get_timeline("BriannaWu", n = 5000, lang = "en")
mic_dys <- get_timeline("MichaelEDyson", n = 5000, lang = "en")
sam_sin <- get_timeline("samswey", n = 5000, lang = "en")
dan_sav <- get_timeline("fakedansavage", n = 5000, lang = "en")
phi_lew <- get_timeline("Phil_Lewis_", n = 5000, lang = "en")
mic_sko <- get_timeline("MichaelSkolnik", n = 5000, lang = "en")
emm_gon <- get_timeline("Emma4Change", n = 5000, lang = "en")
dav_hog <- get_timeline("davidhogg111", n = 5000, lang = "en")
ije_olu <- get_timeline("ijeomaoluo", n = 5000, lang = "en")
taq_nas <- get_timeline("tariqnasheed", n = 5000, lang = "en")
tar_bur <- get_timeline("TaranaBurke", n = 5000, lang = "en")
aly_mil <- get_timeline("Alyssa_Milano", n = 5000, lang = "en")
ali_gar <- get_timeline("aliciagarza", n = 5000, lang = "en")
opa_tom <- get_timeline("opalayo", n = 5000, lang = "en")
jam_smi <- get_timeline("JamilSmith", n = 5000, lang = "en")
ber_kin <- get_timeline("BerniceKing", n = 5000, lang = "en")
der_mck <- get_timeline("deray", n = 5000, lang = "en")
sha_kin <- get_timeline("shaunking", n = 5000, lang = "en")
ash_for <- get_timeline("iSmashFizzle", n = 5000, lang = "en")
car_and <- get_timeline("ProfAnderson", n = 5000, lang = "en")
cam_kas <- get_timeline("cameron_kasky", n = 5000, lang = "en")
lau_duc <- get_timeline("laurenduca", n = 5000, lang = "en")
mic_dys <- get_timeline("MichaelEDyson", n = 5000, lang = "en")
par_mol <- get_timeline("ParkerMolloy", n = 5000, lang = "en")
sal_koh <- get_timeline("sallykohn", n = 5000, lang = "en")
sha_wat <- get_timeline("shannonrwatts", n = 5000, lang = "en")
col_kae <- get_timeline("Kaepernick7", n = 5000, lang = "en")
kat_bla <- get_timeline("kat_blaque", n = 5000, lang = "en")
al_shar <- get_timeline("TheRevAl", n = 5000, lang = "en")
ima_gan <- get_timeline("AngryBlackLady", n = 5000, lang = "en")
ril_den <- get_timeline("RileyJayDennis", n = 5000, lang= "en")
nik_jon <- get_timeline("nhannahjones", n = 5000, lang = "en")
jam_boo <- get_timeline("jbouie", n = 5000, lang = "en")
tim_wis <- get_timeline("timjacobwise", n = 5000, lang = "en")
fra_leo <- get_timeline("franklinleonard", n = 5000, lang = "en")
jes_jac <- get_timeline("RevJJackson", n = 5000, lang = "en")
bri_pac <- get_timeline("MsPackyetti", n = 5000, lang = "en")
edd_gla <- get_timeline("esglaude", n = 5000, lang = "en")
lin_sar <- get_timeline("lsarsour", n = 5000, lang = "en")
jor_pee <- get_timeline("JordanPeele", n = 5000, lang = "en")
lau_pen <- get_timeline("PennyRed", n = 5000, lang = "en")

## aggregated social justice progressive data
socjusprog <- rbind(nat_lgb, lgb_gla, ass_ame, ame_civ, spl_cen, cli_smi,
                    rox_gay, kat_gri, jel_cob, joh_lew, art_chu, bria_wu, 
                    mic_dys, sam_sin, dan_sav, phi_lew, mic_sko, emm_gon, 
                    dav_hog, ije_olu, taq_nas, tar_bur, aly_mil, ali_gar, 
                    opa_tom, jam_smi, ber_kin, der_mck, sha_kin, ash_for,
                    car_and, cam_kas, lau_duc, mic_dys, par_mol, sal_koh,
                    sha_wat, col_kae, kat_bla, al_shar, ima_gan, ril_den,
                    nik_jon, jam_boo, tim_wis, fra_leo, jes_jac, bri_pac,
                    edd_gla, lin_sar, jor_pee, lau_pen)
socjusprog = as.data.frame(sapply(socjusprog, tolower))
socjusprog_logical <- grepl("black lives", socjusprog$text) | 
  grepl("all lives", socjusprog$text) | grepl("blue lives", socjusprog$text) | 
  grepl("blm", socjusprog$text) | grepl("alm", socjusprog$text) | 
  grepl("george floyd", socjusprog$text) | grepl("chauvin", socjusprog$text) | 
  grepl("police", socjusprog$text) | grepl("cops", socjusprog$text) | 
  grepl("pigs", socjusprog$text) | grepl("qualified immunity", socjusprog$text) | 
  grepl("racism", socjusprog$text) | grepl("racist", socjusprog$text) | 
  grepl("racists", socjusprog$text) | grepl("white supremacist", socjusprog$text) | 
  grepl("white supremacists", socjusprog$text) | grepl("rittenhouse", socjusprog$text) | 
  grepl("breonna", socjusprog$text) | grepl("mattingly", socjusprog$text) | 
  grepl("kenneth walker", socjusprog$text) | grepl("elijah", socjusprog$text) | 
  grepl("ahmaud", socjusprog$text) | grepl("antifa", socjusprog$text) | 
  grepl("protest", socjusprog$text) | grepl("protests", socjusprog$text) | 
  grepl("protester", socjusprog$text) | grepl("protesters", socjusprog$text) | 
  grepl("protesting", socjusprog$text) | grepl("riot", socjusprog$text) | 
  grepl("riots", socjusprog$text) | grepl("rioter", socjusprog$text) | 
  grepl("rioters", socjusprog$text) | grepl("rioting", socjusprog$text) | 
  grepl("looter", socjusprog$text) | grepl("looters", socjusprog$text) | 
  grepl("looting", socjusprog$text) | grepl("curfew", socjusprog$text) | 
  grepl("demonstration", socjusprog$text) | grepl("demonstrations", socjusprog$text) | 
  grepl("tear gas", socjusprog$text) | grepl("baton", socjusprog$text) | 
  grepl("rubber bullet", socjusprog$text) | grepl("rubber bullets", socjusprog$text) | 
  grepl("autonomous zone", socjusprog$text) | grepl("unmarked", socjusprog$text) | 
  grepl("peaceful", socjusprog$text) | grepl("violent", socjusprog$text) | 
  grepl("violence", socjusprog$text) | grepl("vandalize", socjusprog$text) | 
  grepl("vandalized", socjusprog$text) | grepl("vandalizing", socjusprog$text) | 
  grepl("confederate", socjusprog$text) | grepl("monument", socjusprog$text) | 
  grepl("statue", socjusprog$text) 
socjusprog_tweets <- cbind(socjusprog, socjusprog_logical)
socjusprog_blm <- socjusprog %>% filter(socjusprog_logical == "TRUE")
socjusprog_blm$created_at <- as.POSIXct(socjusprog_blm$created_at)
socjusprog_blm_gf <- socjusprog_blm %>% filter(created_at > ymd("2020-05-25"))

# social justice progressive data visualization 

## time series
ts_plot(socjusprog_blm_gf, by = "days") + 
  labs(x = NULL, y = NULL, 
       title = "Frequency of tweets by social justice progressives on BLM and systemic racism", 
       caption = "Data from the last 3,200 tweets of various social justice progressive Twitter users since May 25, 2020") + 
  theme_minimal() + geom_line(color = "seagreen3") + 
  theme(text = element_text(size = 16))

## world cloud

## create a vector source for socjusprog
socjusprog_vector <- VectorSource(socjusprog_blm_gf$text)

## check the class of socjusprog_vector
class(socjusprog_vector)

## create a corpus object
socjusprog_corpus <- Corpus(socjusprog_vector)

## make the data uniform (no punctuation/number)
socjusprog_corpus <- tm_map(socjusprog_corpus, removePunctuation)
socjusprog_corpus <- tm_map(socjusprog_corpus, removeNumbers)

## remove stopwords
socjusprog_corpus <- tm_map(socjusprog_corpus, removeWords, stopwords("english"))

## create the term document matrix 
socjusprog_tdm <- TermDocumentMatrix(socjusprog_corpus)
socjusprog_matrix = as.matrix(socjusprog_tdm)
socjusprog_wordcount <- rowSums(socjusprog_matrix)
socjusprog_wordcount <- sort(socjusprog_wordcount, decreasing = TRUE)
head(socjusprog_wordcount)

## create a wordcloud
socjusprog_cloud <- data.frame(word = names(socjusprog_wordcount), 
                               freq = socjusprog_wordcount)
set.seed(1234)
wordcloud(names(socjusprog_wordcount), socjusprog_wordcount, min.freq = 1,
          random.order = FALSE, max.words = 100, scale = c(4, 0.5), 
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

## most tweets in a single day by social justice progressives
socjusprog_blm_gf$created_at = as.Date(socjusprog_blm_gf$created_at, format('ymd'))
socjusprog_blm_gf %>% count(created_at, sort = TRUE) %>% top_n(5)

# establishment democrat data collection

## individual establishment democrat data
hil_cli <- get_timeline("HillaryClinton", n = 5000, lang = "en")
bar_oba <- get_timeline("BarackObama", n = 5000, lang = "en")
mic_oba <- get_timeline("MichelleObama", n = 5000, lang = "en")
joe_bid <- get_timeline("JoeBiden", n = 5000, lang = "en")
kam_har <- get_timeline("KamalaHarris", n = 5000, lang = "en")
cor_boo <- get_timeline("CoryBooker", n = 5000, lang = "en")
amy_klo <- get_timeline("amyklobuchar", n = 5000, lang = "en")
nan_pel <- get_timeline("SpeakerPelosi", n = 5000, lang = "en")
bet_oro <- get_timeline("BetoORourke", n = 5000, lang = "en")
max_wat <- get_timeline("RepMaxineWaters", n = 5000, lang = "en")
ted_lie <- get_timeline("tedlieu", n = 5000, lang = "en")
chu_sch <- get_timeline("SenSchumer", n = 5000, lang = "en")
and_cuo <- get_timeline("NYGovCuomo", n = 5000, lang = "en")
tim_kai <- get_timeline("timkaine", n = 5000, lang = "en")
bob_cas <- get_timeline("SenBobCasey", n = 5000, lang = "en")
kir_gil <- get_timeline("SenGillibrand", n = 5000, lang = "en")
ada_sch <- get_timeline("RepAdamSchiff", n = 5000, lang = "en")
dia_fei <- get_timeline("SenFeinstein", n = 5000, lang = "en")
maz_hir <- get_timeline("maziehirono", n = 5000, lang = "en")
dic_dur <- get_timeline("SenatorDurbin", n = 5000, lang = "en")
chr_mur <- get_timeline("ChrisMurphyCT", n = 5000, lang = "en")
ric_blu <- get_timeline("SenBlumenthal", n =  5000, lang = "en")

## aggregated establishment democrat data
estabdem <- rbind(hil_cli, bar_oba, mic_oba, joe_bid, kam_har, cor_boo, 
                  amy_klo, nan_pel, bet_oro, max_wat, ted_lie, chu_sch,
                  and_cuo, tim_kai, bob_cas, kir_gil, ada_sch, dia_fei,
                  maz_hir, dic_dur, chr_mur, ric_blu)
estabdem = as.data.frame(sapply(estabdem, tolower))
estabdem_logical <- grepl("black lives", estabdem$text) | 
  grepl("all lives", estabdem$text) | grepl("blue lives", estabdem$text) | 
  grepl("blm", estabdem$text) | grepl("alm", estabdem$text) | 
  grepl("george floyd", estabdem$text) | grepl("chauvin", estabdem$text) | 
  grepl("police", estabdem$text) | grepl("cops", estabdem$text) | 
  grepl("pigs", estabdem$text) | grepl("qualified immunity", estabdem$text) | 
  grepl("racism", estabdem$text) | grepl("racist", estabdem$text) | 
  grepl("racists", estabdem$text) | grepl("white supremacist", estabdem$text) | 
  grepl("white supremacists", estabdem$text) | grepl("rittenhouse", estabdem$text) | 
  grepl("breonna", estabdem$text) | grepl("mattingly", estabdem$text) | 
  grepl("kenneth walker", estabdem$text) | grepl("elijah", estabdem$text) | 
  grepl("ahmaud", estabdem$text) | grepl("antifa", estabdem$text) | 
  grepl("protest", estabdem$text) | grepl("protests", estabdem$text) | 
  grepl("protester", estabdem$text) | grepl("protesters", estabdem$text) | 
  grepl("protesting", estabdem$text) | grepl("riot", estabdem$text) | 
  grepl("riots", estabdem$text) | grepl("rioter", estabdem$text) | 
  grepl("rioters", estabdem$text) | grepl("rioting", estabdem$text) | 
  grepl("looter", estabdem$text) | grepl("looters", estabdem$text) | 
  grepl("looting", estabdem$text) | grepl("curfew", estabdem$text) | 
  grepl("demonstration", estabdem$text) | grepl("demonstrations", estabdem$text) | 
  grepl("tear gas", estabdem$text) | grepl("baton", estabdem$text) | 
  grepl("rubber bullet", estabdem$text) | grepl("rubber bullets", estabdem$text) | 
  grepl("autonomous zone", estabdem$text) | grepl("unmarked", estabdem$text) | 
  grepl("peaceful", estabdem$text) | grepl("violent", estabdem$text) | 
  grepl("violence", estabdem$text) | grepl("vandalize", estabdem$text) | 
  grepl("vandalized", estabdem$text) | grepl("vandalizing", estabdem$text) | 
  grepl("confederate", estabdem$text) | grepl("monument", estabdem$text) | 
  grepl("statue", estabdem$text) 
estabdem_tweets <- cbind(estabdem, estabdem_logical)
estabdem_blm <- estabdem %>% filter(estabdem_logical == "TRUE")
estabdem_blm$created_at <- as.POSIXct(estabdem_blm$created_at)
estabdem_blm_gf <- estabdem_blm %>% filter(created_at > ymd("2020-05-25"))

# establishment democrat data visualization 

## time series
ts_plot(estabdem_blm_gf, by = "days") + 
  labs(x = NULL, y = NULL, 
       title = "Frequency of tweets by establishment Democrats on BLM and systemic racism", 
       caption = "Data from the last 3,200 tweets of various establishment Democrat Twitter users since May 25, 2020") + 
  theme_minimal() + geom_line(color = "seagreen3") + 
  theme(text = element_text(size = 16))

## world cloud

## create a vector source for estabdem
estabdem_vector <- VectorSource(estabdem_blm_gf$text)

## check the class of estabdem_vector
class(estabdem_vector)

## create a corpus object
estabdem_corpus <- Corpus(estabdem_vector)

## make the data uniform (no punctuation/number)
estabdem_corpus <- tm_map(estabdem_corpus, removePunctuation)
estabdem_corpus <- tm_map(estabdem_corpus, removeNumbers)

## remove stopwords
estabdem_corpus <- tm_map(estabdem_corpus, removeWords, stopwords("english"))

## create the term document matrix 
estabdem_tdm <- TermDocumentMatrix(estabdem_corpus)
estabdem_matrix = as.matrix(estabdem_tdm)
estabdem_wordcount <- rowSums(estabdem_matrix)
estabdem_wordcount <- sort(estabdem_wordcount, decreasing = TRUE)
head(estabdem_wordcount)

## create a wordcloud
estabdem_cloud <- data.frame(word = names(estabdem_wordcount), 
                             freq = estabdem_wordcount)
set.seed(1234)
wordcloud(names(estabdem_wordcount), estabdem_wordcount, min.freq = 1,
          random.order = FALSE, max.words = 100, scale = c(4, 0.5), 
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

## most tweets in a single day by establishment democrats
estabdem_blm_gf$created_at = as.Date(estabdem_blm_gf$created_at, format('ymd'))
estabdem_blm_gf %>% count(created_at, sort = TRUE) %>% top_n(5)

# general liberal data collection

## individual general liberal data
nat_sil <- get_timeline("NateSilver538", n = 5000, lang = "en")
jam_fal <- get_timeline("JamesFallows", n = 5000, lang = "en")
and_mit <- get_timeline("mitchellreports", n = 5000, lang = "en")
bil_mah <- get_timeline("billmaher", n = 5000, lang = "en")
dem_coa <- get_timeline("TheDemCoaalition", n = 5000, lang = "en")
cen_ame <- get_timeline("amprog", n = 5000, lang = "en")
max_blu <- get_timeline("MaxBlumenthal", n = 5000, lang = "en")
bri_coh <- get_timeline("briantylercohen", n = 5000, lang = "en")
jak_tap <- get_timeline("jaketapper", n = 5000, lang = "en")
jim_aco <- get_timeline("Acosta", n = 5000, lang = "en")
jud_leg <- get_timeline("JuddLegum", n = 5000, lang = "en")
geo_tak <- get_timeline("GeorgeTakei", n = 5000, lang = "en")
van_jon <- get_timeline("VanJones68", n = 5000, lang = "en")
opr_win <- get_timeline("Oprah", n = 5000, lang = "en")
joh_leg <- get_timeline("johnlegend", n = 5000, lang = "en")
ste_kin <- get_timeline("StephenKing", n = 5000, lang = "en")
rob_rei <- get_timeline("robreiner", n = 5000, lang = "en")
dai_sho <- get_timeline("TheDailyShow", n = 5000, lang = "en")
jim_kim <- get_timeline("jimmykimmel", n = 5000, lang = "en")

## aggregated general liberal data
genliberal <- rbind(nat_sil, jam_fal, and_mit, bil_mah, dem_coa, cen_ame,
                    max_blu, bri_coh, jak_tap, jim_aco, jud_leg, geo_tak,
                    van_jon, opr_win, joh_leg, ste_kin, rob_rei, dai_sho, 
                    jim_kim)
genliberal = as.data.frame(sapply(genliberal, tolower))
genliberal_logical <- grepl("black lives", genliberal$text) | 
  grepl("all lives", genliberal$text) | grepl("blue lives", genliberal$text) | 
  grepl("blm", genliberal$text) | grepl("alm", genliberal$text) | 
  grepl("george floyd", genliberal$text) | grepl("chauvin", genliberal$text) | 
  grepl("police", genliberal$text) | grepl("cops", genliberal$text) | 
  grepl("pigs", genliberal$text) | grepl("qualified immunity", genliberal$text) | 
  grepl("racism", genliberal$text) | grepl("racist", genliberal$text) | 
  grepl("racists", genliberal$text) | grepl("white supremacist", genliberal$text) | 
  grepl("white supremacists", genliberal$text) | grepl("rittenhouse", genliberal$text) | 
  grepl("breonna", genliberal$text) | grepl("mattingly", genliberal$text) | 
  grepl("kenneth walker", genliberal$text) | grepl("elijah", genliberal$text) | 
  grepl("ahmaud", genliberal$text) | grepl("antifa", genliberal$text) | 
  grepl("protest", genliberal$text) | grepl("protests", genliberal$text) | 
  grepl("protester", genliberal$text) | grepl("protesters", genliberal$text) | 
  grepl("protesting", genliberal$text) | grepl("riot", genliberal$text) | 
  grepl("riots", genliberal$text) | grepl("rioter", genliberal$text) | 
  grepl("rioters", genliberal$text) | grepl("rioting", genliberal$text) | 
  grepl("looter", genliberal$text) | grepl("looters", genliberal$text) | 
  grepl("looting", genliberal$text) | grepl("curfew", genliberal$text) | 
  grepl("demonstration", genliberal$text) | grepl("demonstrations", genliberal$text) | 
  grepl("tear gas", genliberal$text) | grepl("baton", genliberal$text) | 
  grepl("rubber bullet", genliberal$text) | grepl("rubber bullets", genliberal$text) | 
  grepl("autonomous zone", genliberal$text) | grepl("unmarked", genliberal$text) | 
  grepl("peaceful", genliberal$text) | grepl("violent", genliberal$text) | 
  grepl("violence", genliberal$text) | grepl("vandalize", genliberal$text) | 
  grepl("vandalized", genliberal$text) | grepl("vandalizing", genliberal$text) | 
  grepl("confederate", genliberal$text) | grepl("monument", genliberal$text) | 
  grepl("statue", genliberal$text) 
genliberal_tweets <- cbind(genliberal, genliberal_logical)
genliberal_blm <- genliberal %>% filter(genliberal_logical == "TRUE")
genliberal_blm$created_at <- as.POSIXct(genliberal_blm$created_at)
genliberal_blm_gf <- genliberal_blm %>% filter(created_at > ymd("2020-05-25"))

# general liberal data visualization 

## time series
ts_plot(genliberal_blm_gf, by = "days") + 
  labs(x = NULL, y = NULL, 
       title = "Frequency of tweets by general liberals on BLM and systemic racism", 
       caption = "Data from the last 3,200 tweets of various generally liberal Twitter users since May 25, 2020") + 
  theme_minimal() + geom_line(color = "seagreen3") + 
  theme(text = element_text(size = 16))

## world cloud

## create a vector source for genliberal
genliberal_vector <- VectorSource(genliberal_blm_gf$text)

## check the class of genliberal_vector
class(genliberal_vector)

## create a corpus object
genliberal_corpus <- Corpus(genliberal_vector)

## make the data uniform (no punctuation/number)
genliberal_corpus <- tm_map(genliberal_corpus, removePunctuation)
genliberal_corpus <- tm_map(genliberal_corpus, removeNumbers)

## remove stopwords
genliberal_corpus <- tm_map(genliberal_corpus, removeWords, stopwords("english"))

## create the term document matrix 
genliberal_tdm <- TermDocumentMatrix(genliberal_corpus)
genliberal_matrix = as.matrix(genliberal_tdm)
genliberal_wordcount <- rowSums(genliberal_matrix)
genliberal_wordcount <- sort(genliberal_wordcount, decreasing = TRUE)
head(genliberal_wordcount)

## create a wordcloud
genliberal_cloud <- data.frame(word = names(genliberal_wordcount), 
                               freq = genliberal_wordcount)
set.seed(1234)
wordcloud(names(genliberal_wordcount), genliberal_wordcount, min.freq = 1,
          random.order = FALSE, max.words = 100, scale = c(4, 0.5), 
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

## most tweets in a single day by social justice progressives
genliberal_blm_gf$created_at = as.Date(genliberal_blm_gf$created_at, format('ymd'))
genliberal_blm_gf %>% count(created_at, sort = TRUE) %>% top_n(5)

# centrist liberal data collection

## individual centrist liberal data
bro_ins <- get_timeline("BrookingsInst", n = 5000, lang = "en")
sam_har <- get_timeline("SamHarrisOrg", n = 5000, lang = "en")
dan_car <- get_timeline("dccommonsense", n = 5000, lang = "en")
ste_pin <- get_timeline("sapinker", n = 5000, lang = "en")
sar_hai <- get_timeline("SarahTheHaider", n = 5000, lang = "en")
far_zak <- get_timeline("FareedZakaria", n = 5000, lang = "en")
h3h3_pr <- get_timeline("h3h3productions", n = 5000, lang = "en")
phi_def <- get_timeline("PhillyD", n = 5000, lang = "en")
ian_bre <- get_timeline("ianbremmer", n = 5000, lang = "en")
eri_wei <- get_timeline("EricRWeinstein", n = 5000, lang = "en")
bre_wei <- get_timeline("BretWeinstein", n = 5000, lang = "en")
noa_smi <- get_timeline("Noahpinion", n = 5000, lang = "en")
hea_hey <- get_timeline("HeatherEHeying", n = 5000, lang = "en")

## aggregated centrist liberal data
centristlib <- rbind(bro_ins, sam_har, dan_car, ste_pin, sar_hai, far_zak,
                     h3h3_pr, phi_def, ian_bre, eri_wei, bre_wei, noa_smi, 
                     hea_hey)
centristlib = as.data.frame(sapply(centristlib, tolower))
centristlib_logical <- grepl("black lives", centristlib$text) | 
  grepl("all lives", centristlib$text) | grepl("blue lives", centristlib$text) | 
  grepl("blm", centristlib$text) | grepl("alm", centristlib$text) | 
  grepl("george floyd", centristlib$text) | grepl("chauvin", centristlib$text) | 
  grepl("police", centristlib$text) | grepl("cops", centristlib$text) | 
  grepl("pigs", centristlib$text) | grepl("qualified immunity", centristlib$text) | 
  grepl("racism", centristlib$text) | grepl("racist", centristlib$text) | 
  grepl("racists", centristlib$text) | grepl("white supremacist", centristlib$text) | 
  grepl("white supremacists", centristlib$text) | grepl("rittenhouse", centristlib$text) | 
  grepl("breonna", centristlib$text) | grepl("mattingly", centristlib$text) | 
  grepl("kenneth walker", centristlib$text) | grepl("elijah", centristlib$text) | 
  grepl("ahmaud", centristlib$text) | grepl("antifa", centristlib$text) | 
  grepl("protest", centristlib$text) | grepl("protests", centristlib$text) | 
  grepl("protester", centristlib$text) | grepl("protesters", centristlib$text) | 
  grepl("protesting", centristlib$text) | grepl("riot", centristlib$text) | 
  grepl("riots", centristlib$text) | grepl("rioter", centristlib$text) | 
  grepl("rioters", centristlib$text) | grepl("rioting", centristlib$text) | 
  grepl("looter", centristlib$text) | grepl("looters", centristlib$text) | 
  grepl("looting", centristlib$text) | grepl("curfew", centristlib$text) | 
  grepl("demonstration", centristlib$text) | grepl("demonstrations", centristlib$text) | 
  grepl("tear gas", centristlib$text) | grepl("baton", centristlib$text) | 
  grepl("rubber bullet", centristlib$text) | grepl("rubber bullets", centristlib$text) | 
  grepl("autonomous zone", centristlib$text) | grepl("unmarked", centristlib$text) | 
  grepl("peaceful", centristlib$text) | grepl("violent", centristlib$text) | 
  grepl("violence", centristlib$text) | grepl("vandalize", centristlib$text) | 
  grepl("vandalized", centristlib$text) | grepl("vandalizing", centristlib$text) | 
  grepl("confederate", centristlib$text) | grepl("monument", centristlib$text) | 
  grepl("statue", centristlib$text) 
centristlib_tweets <- cbind(centristlib, centristlib_logical)
centristlib_blm <- centristlib %>% filter(centristlib_logical == "TRUE")
centristlib_blm$created_at <- as.POSIXct(centristlib_blm$created_at)
centristlib_blm_gf <- centristlib_blm %>% filter(created_at > ymd("2020-05-25"))

# centrist liberal data visualization 

## time series
ts_plot(centristlib_blm_gf, by = "days") + 
  labs(x = NULL, y = NULL, 
       title = "Frequency of tweets by centrist liberals on BLM and systemic racism", 
       caption = "Data from the last 3,200 tweets of various centrist liberal users since May 25, 2020") + 
  theme_minimal() + geom_line(color = "seagreen3") + 
  theme(text = element_text(size = 16))

## world cloud

## create a vector source for centristlib
centristlib_vector <- VectorSource(centristlib_blm_gf$text)

## check the class of centristlib_vector
class(centristlib_vector)

## create a corpus object
centristlib_corpus <- Corpus(centristlib_vector)

## make the data uniform (no punctuation/number)
centristlib_corpus <- tm_map(centristlib_corpus, removePunctuation)
centristlib_corpus <- tm_map(centristlib_corpus, removeNumbers)

## remove stopwords
centristlib_corpus <- tm_map(centristlib_corpus, removeWords, stopwords("english"))

## create the term document matrix 
centristlib_tdm <- TermDocumentMatrix(centristlib_corpus)
centristlib_matrix = as.matrix(centristlib_tdm)
centristlib_wordcount <- rowSums(centristlib_matrix)
centristlib_wordcount <- sort(centristlib_wordcount, decreasing = TRUE)
head(centristlib_wordcount)

## create a wordcloud
centristlib_cloud <- data.frame(word = names(centristlib_wordcount), 
                                freq = centristlib_wordcount)
set.seed(1234)
wordcloud(names(centristlib_wordcount), centristlib_wordcount, min.freq = 1,
          random.order = FALSE, max.words = 100, scale = c(4, 0.5), 
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

## most tweets in a single day by centrist liberals
centristlib_blm_gf$created_at = as.Date(centristlib_blm_gf$created_at, format('ymd'))
centristlib_blm_gf %>% count(created_at, sort = TRUE) %>% top_n(5)

# centrist conservative data collection

## individual centrist conservative data
amy_alk <- get_timeline("amyalkin", n = 5000, lang = "en")
hoo_ins <- get_timeline("HooverInst", n = 5000, lang = "en")
gad_saa <- get_timeline("GadSaad", n = 5000, lang = "en")
mel_che <- get_timeline("MsMelChen", n = 5000, lang = "en")
chr_som <- get_timeline("CHSommers", n = 5000, lang = "en")
tim_poo <- get_timeline("TimCast", n = 5000, lang = "en")
nic_dip <- get_timeline("NickDiPaolo", n = 5000, lang = "en")
ric_low <- get_timeline("RichLowry", n = 5000, lang = "en")
con_fri <- get_timeline("conor64", n = 5000, lang = "en")
jos_bar <- get_timeline("jbarro", n = 5000, lang = "en")
tom_nic <- get_timeline("RadioFreeTom", n = 5000, lang = "en")
ale_ber <- get_timeline("AlexBerenson", n = 5000, lang = "en")
lau_che <- get_timeline("TheLaurenChen", n = 5000, lang = "en")
noa_blu <- get_timeline("neontaser", n = 5000, lang = "en")
yeyo_za <- get_timeline("RealYeyoZa", n = 5000, lang = "en")

## aggregated centrist conservative data
centristcons <- rbind(amy_alk, hoo_ins, gad_saa, mel_che, chr_som, tim_poo,
                      nic_dip, ric_low, con_fri, jos_bar, tom_nic, ale_ber, 
                      lau_che, noa_blu, yeyo_za)
centristcons = as.data.frame(sapply(centristcons, tolower))
centristcons_logical <- grepl("black lives", centristcons$text) | 
  grepl("all lives", centristcons$text) | grepl("blue lives", centristcons$text) | 
  grepl("blm", centristcons$text) | grepl("alm", centristcons$text) | 
  grepl("george floyd", centristcons$text) | grepl("chauvin", centristcons$text) | 
  grepl("police", centristcons$text) | grepl("cops", centristcons$text) | 
  grepl("pigs", centristcons$text) | grepl("qualified immunity", centristcons$text) | 
  grepl("racism", centristcons$text) | grepl("racist", centristcons$text) | 
  grepl("racists", centristcons$text) | grepl("white supremacist", centristcons$text) | 
  grepl("white supremacists", centristcons$text) | grepl("rittenhouse", centristcons$text) | 
  grepl("breonna", centristcons$text) | grepl("mattingly", centristcons$text) | 
  grepl("kenneth walker", centristcons$text) | grepl("elijah", centristcons$text) | 
  grepl("ahmaud", centristcons$text) | grepl("antifa", centristcons$text) | 
  grepl("protest", centristcons$text) | grepl("protests", centristcons$text) | 
  grepl("protester", centristcons$text) | grepl("protesters", centristcons$text) | 
  grepl("protesting", centristcons$text) | grepl("riot", centristcons$text) | 
  grepl("riots", centristcons$text) | grepl("rioter", centristcons$text) | 
  grepl("rioters", centristcons$text) | grepl("rioting", centristcons$text) | 
  grepl("looter", centristcons$text) | grepl("looters", centristcons$text) | 
  grepl("looting", centristcons$text) | grepl("curfew", centristcons$text) | 
  grepl("demonstration", centristcons$text) | grepl("demonstrations", centristcons$text) | 
  grepl("tear gas", centristcons$text) | grepl("baton", centristcons$text) | 
  grepl("rubber bullet", centristcons$text) | grepl("rubber bullets", centristcons$text) | 
  grepl("autonomous zone", centristcons$text) | grepl("unmarked", centristcons$text) | 
  grepl("peaceful", centristcons$text) | grepl("violent", centristcons$text) | 
  grepl("violence", centristcons$text) | grepl("vandalize", centristcons$text) | 
  grepl("vandalized", centristcons$text) | grepl("vandalizing", centristcons$text) | 
  grepl("confederate", centristcons$text) | grepl("monument", centristcons$text) | 
  grepl("statue", centristcons$text) 
centristcons_tweets <- cbind(centristcons, centristcons_logical)
centristcons_blm <- centristcons %>% filter(centristcons_logical == "TRUE")
centristcons_blm$created_at <- as.POSIXct(centristcons_blm$created_at)
centristcons_blm_gf <- centristcons_blm %>% filter(created_at > ymd("2020-05-25"))

# centrist conservative data visualization 

## time series
ts_plot(centristcons_blm_gf, by = "days") + 
  labs(x = NULL, y = NULL, 
       title = "Frequency of tweets by centrist conservatives on BLM and systemic racism", 
       caption = "Data from the last 3,200 tweets of various centrist conservative users since May 25, 2020") + 
  theme_minimal() + geom_line(color = "seagreen3") + 
  theme(text = element_text(size = 16))

## world cloud

## create a vector source for centristcons
centristcons_vector <- VectorSource(centristcons_blm_gf$text)

## check the class of centristcons_vector
class(centristcons_vector)

## create a corpus object
centristcons_corpus <- Corpus(centristcons_vector)

## make the data uniform (no punctuation/number)
centristcons_corpus <- tm_map(centristcons_corpus, removePunctuation)
centristcons_corpus <- tm_map(centristcons_corpus, removeNumbers)

## remove stopwords
centristcons_corpus <- tm_map(centristcons_corpus, removeWords, stopwords("english"))

## create the term document matrix 
centristcons_tdm <- TermDocumentMatrix(centristcons_corpus)
centristcons_matrix = as.matrix(centristcons_tdm)
centristcons_wordcount <- rowSums(centristcons_matrix)
centristcons_wordcount <- sort(centristcons_wordcount, decreasing = TRUE)
head(centristcons_wordcount)

## create a wordcloud
centristcons_cloud <- data.frame(word = names(centristcons_wordcount), 
                                 freq = centristcons_wordcount)
set.seed(1234)
wordcloud(names(centristcons_wordcount), centristcons_wordcount, min.freq = 1,
          random.order = FALSE, max.words = 100, scale = c(4, 0.5), 
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

## most tweets in a single day by centrist conservatives
centristcons_blm_gf$created_at = as.Date(centristcons_blm_gf$created_at, format('ymd'))
centristcons_blm_gf %>% count(created_at, sort = TRUE) %>% top_n(5)

# libertarian data collection

## individual libertarian data
nic_gil <- get_timeline("nickgillespie", n = 5000, lang = "en")
mat_wel <- get_timeline("MattWelch", n = 5000, lang = "en")
eli_bro <- get_timeline("ENBrown", n = 5000, lang = "en")
cat_ins <- get_timeline("CatoInstitute", n = 5000, lang = "en")
dav_rub <- get_timeline("RubinReport", n = 5000, lang = "en")
kme_fos <- get_timeline("kmele", n = 5000, lang = "en")
ran_pau <- get_timeline("RandPaul", n = 5000, lang = "en")
mic_she <- get_timeline("michaelshermer", n = 5000, lang = "en")
cat_you <- get_timeline("CathyYoung63", n = 5000, lang = "en")
jes_ven <- get_timeline("GovJVentura", n = 5000, lang = "en")
lar_eld <- get_timeline("larryelder", n = 5000, lang = "en")
gle_bec <- get_timeline("glennbeck", n = 5000, lang = "en")
gre_gut <- get_timeline("greggutfeld", n = 5000, lang = "en")
dav_wei <- get_timeline("daveweigel", n = 5000, lang = "en")
tar_war <- get_timeline("Styx666Official", n = 5000, lang = "en")


## aggregated libertarian data
libertarian <- rbind(nic_gil, mat_wel, eli_bro, cat_ins, dav_rub, kme_fos, 
                     ran_pau, mic_she, cat_you, jes_ven, lar_eld, gle_bec,
                     gre_gut, dav_wei, tar_war)
libertarian = as.data.frame(sapply(libertarian, tolower))
libertarian_logical <- grepl("black lives", libertarian$text) | 
  grepl("all lives", libertarian$text) | grepl("blue lives", libertarian$text) | 
  grepl("blm", libertarian$text) | grepl("alm", libertarian$text) | 
  grepl("george floyd", libertarian$text) | grepl("chauvin", libertarian$text) | 
  grepl("police", libertarian$text) | grepl("cops", libertarian$text) | 
  grepl("pigs", libertarian$text) | grepl("qualified immunity", libertarian$text) | 
  grepl("racism", libertarian$text) | grepl("racist", libertarian$text) | 
  grepl("racists", libertarian$text) | grepl("white supremacist", libertarian$text) | 
  grepl("white supremacists", libertarian$text) | grepl("rittenhouse", libertarian$text) | 
  grepl("breonna", libertarian$text) | grepl("mattingly", libertarian$text) | 
  grepl("kenneth walker", libertarian$text) | grepl("elijah", libertarian$text) | 
  grepl("ahmaud", libertarian$text) | grepl("antifa", libertarian$text) | 
  grepl("protest", libertarian$text) | grepl("protests", libertarian$text) | 
  grepl("protester", libertarian$text) | grepl("protesters", libertarian$text) | 
  grepl("protesting", libertarian$text) | grepl("riot", libertarian$text) | 
  grepl("riots", libertarian$text) | grepl("rioter", libertarian$text) | 
  grepl("rioters", libertarian$text) | grepl("rioting", libertarian$text) | 
  grepl("looter", libertarian$text) | grepl("looters", libertarian$text) | 
  grepl("looting", libertarian$text) | grepl("curfew", libertarian$text) | 
  grepl("demonstration", libertarian$text) | grepl("demonstrations", libertarian$text) | 
  grepl("tear gas", libertarian$text) | grepl("baton", libertarian$text) | 
  grepl("rubber bullet", libertarian$text) | grepl("rubber bullets", libertarian$text) | 
  grepl("autonomous zone", libertarian$text) | grepl("unmarked", libertarian$text) | 
  grepl("peaceful", libertarian$text) | grepl("violent", libertarian$text) | 
  grepl("violence", libertarian$text) | grepl("vandalize", libertarian$text) | 
  grepl("vandalized", libertarian$text) | grepl("vandalizing", libertarian$text) | 
  grepl("confederate", libertarian$text) | grepl("monument", libertarian$text) | 
  grepl("statue", libertarian$text) 
libertarian_tweets <- cbind(libertarian, libertarian_logical)
libertarian_blm <- libertarian %>% filter(libertarian_logical == "TRUE")
libertarian_blm$created_at <- as.POSIXct(libertarian_blm$created_at)
libertarian_blm_gf <- libertarian_blm %>% filter(created_at > ymd("2020-05-25"))

# libertarian data visualization 

## time series
ts_plot(libertarian_blm_gf, by = "days") + 
  labs(x = NULL, y = NULL, 
       title = "Frequency of tweets by libertarians on BLM and systemic racism", 
       caption = "Data from the last 3,200 tweets of various libertarian users since May 25, 2020") + 
  theme_minimal() + geom_line(color = "seagreen3") + 
  theme(text = element_text(size = 16))

## world cloud

## create a vector source for libertarian
libertarian_vector <- VectorSource(libertarian_blm_gf$text)

## check the class of libertarian_vector
class(libertarian_vector)

## create a corpus object
libertarian_corpus <- Corpus(libertarian_vector)

## make the data uniform (no punctuation/number)
libertarian_corpus <- tm_map(libertarian_corpus, removePunctuation)
libertarian_corpus <- tm_map(libertarian_corpus, removeNumbers)

## remove stopwords
libertarian_corpus <- tm_map(libertarian_corpus, removeWords, stopwords("english"))

## create the term document matrix 
libertarian_tdm <- TermDocumentMatrix(libertarian_corpus)
libertarian_matrix = as.matrix(libertarian_tdm)
libertarian_wordcount <- rowSums(libertarian_matrix)
libertarian_wordcount <- sort(libertarian_wordcount, decreasing = TRUE)
head(libertarian_wordcount)

## create a wordcloud
libertarian_cloud <- data.frame(word = names(libertarian_wordcount), 
                                freq = libertarian_wordcount)
set.seed(1234)
wordcloud(names(libertarian_wordcount), libertarian_wordcount, min.freq = 1,
          random.order = FALSE, max.words = 100, scale = c(4, 0.5), 
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

## most tweets in a single day by libertarians
libertarian_blm_gf$created_at = as.Date(libertarian_blm_gf$created_at, format('ymd'))
libertarian_blm_gf %>% count(created_at, sort = TRUE) %>% top_n(5)

# general conservative data collection

## individual general conservative data
wes_jor <- get_timeline("WestJournalism", n = 5000, lang = "en")
ben_sha <- get_timeline("benshapiro", n = 5000, lang = "en")
prage_u <- get_timeline("prageru", n = 5000, lang = "en")
cam_ref <- get_timeline("campusreform", n = 5000, lang = "en")
mac_ins <- get_timeline("MacIverWisc", n = 5000, lang = "en")
med_res <- get_timeline("theMRC", n = 5000, lang = "en")
mic_mal <- get_timeline("michellemalkin", n = 5000, lang = "en")
bil_ore <- get_timeline("BillOReilly", n = 5000, lang = "en")
sea_han <- get_timeline("seanhannity", n = 5000, lang = "en")
tuc_car <- get_timeline("TuckerCarlson", n = 5000, lang = "en")
mar_lev <- get_timeline("marklevinshow", n = 5000, lang = "en")
seb_gor <- get_timeline("SebGorka", n = 5000, lang = "en")
ana_nav <- get_timeline("ananavarro", n = 5000, lang = "en")
ric_wio <- get_timeline("TheRickWilson", n = 5000, lang = "en")
hug_hew <- get_timeline("hughhewitt", n = 5000, lang = "en")
dan_loe <- get_timeline("DLoesch", n = 5000, lang = "en")
bil_kri <- get_timeline("BillKristol", n = 5000, lang = "en")
joh_car <- get_timeline("johncardillo", n = 5000, lang = "en")
amy_kre <- get_timeline("AmyKremer", n = 5000, lang = "en")
all_stu <- get_timeline("conservmillen", n = 5000, lang = "en")
joe_wal <- get_timeline("WalshFreedom", n = 5000, lang = "en")
mic_kno <- get_timeline("michaeljknowles", n = 5000, lang = "en")
kee_sta <- get_timeline("KEEMSTAR", n = 5000, lang = "en")
tur_poi <- get_timeline("TPUSA", n = 5000, lang = "en")
cha_kir <- get_timeline("CharlieKirk11", n = 5000, lang = "en")
rya_fou <- get_timeline("RyanAFournier", n = 5000, lang = "en")
sco_pre <- get_timeline("ScottPresler", n = 5000, lang = "en")
can_owe <- get_timeline("RealCandaceO", n = 5000, lang = "en")
ste_cro <- get_timeline("scrowder", n = 5000, lang = "en")
tom_lah <- get_timeline("TomiLahren", n = 5000, lang = "en")
ant_log <- get_timeline("ANTHONYBLOGAN", n = 5000, lang = "en")
kai_ben <- get_timeline("KaitMarieox", n = 5000, lang = "en")
cha_gre <- get_timeline("chadfelixg", n = 5000, lang = "en")
max_boo <- get_timeline("MaxBoot", n = 5000, lang = "en")
dav_web <- get_timeline("davidwebbshow", n = 5000, lang = "en")
ben_joh <- get_timeline("bennyjohnson", n = 5000, lang = "en")
and_kla <- get_timeline("andrewklavan", n = 5000, lang = "en")
jam_woo <- get_timeline("RealJamesWoods", n = 5000, lang = "en")
mol_hem <- get_timeline("MZHemingway", n = 5000, lang = "en")
rod_dre <- get_timeline("roddreher", n = 5000, lang = "en")
ste_sch <- get_timeline("SteveSchmidtSES", n = 5000, lang = "en")
kat_pav <- get_timeline("KatiePavlich", n = 5000, lang = "en")
ezr_lev <- get_timeline("ezralevant", n = 5000, lang = "en")
buc_sex <- get_timeline("BuckSexton", n = 5000, lang = "en")
rya_saa <- get_timeline("RealSaavedra", n = 5000, lang = "en")
nea_boo <- get_timeline("Talkmaster", n = 5000, lang = "en")
sha_tri <- get_timeline("ComfortablySmug", n = 5000, lang = "en")

## aggregated general conservative data
genconservative <- rbind(wes_jor, ben_sha, prage_u, cam_ref, mac_ins, 
                         med_res, mic_mal, bil_ore, sea_han, tuc_car,
                         mar_lev, seb_gor, ana_nav, ric_wio, hug_hew,
                         dan_loe, bil_kri, joh_car, amy_kre, all_stu, 
                         joe_wal, mic_kno, kee_sta, tur_poi, cha_kir, 
                         rya_fou, sco_pre, can_owe, ste_cro, tom_lah,
                         ant_log, kai_ben, cha_gre, max_boo, dav_web,
                         ben_joh, and_kla, jam_woo, mol_hem, rod_dre,
                         ste_sch, kat_pav, ezr_lev, buc_sex, rya_saa,
                         nea_boo, sha_tri)
genconservative = as.data.frame(sapply(genconservative, tolower))
genconservative_logical <- grepl("black lives", genconservative$text) | 
  grepl("all lives", genconservative$text) | grepl("blue lives", genconservative$text) | 
  grepl("blm", genconservative$text) | grepl("alm", genconservative$text) | 
  grepl("george floyd", genconservative$text) | grepl("chauvin", genconservative$text) | 
  grepl("police", genconservative$text) | grepl("cops", genconservative$text) | 
  grepl("pigs", genconservative$text) | grepl("qualified immunity", genconservative$text) | 
  grepl("racism", genconservative$text) | grepl("racist", genconservative$text) | 
  grepl("racists", genconservative$text) | grepl("white supremacist", genconservative$text) | 
  grepl("white supremacists", genconservative$text) | grepl("rittenhouse", genconservative$text) | 
  grepl("breonna", genconservative$text) | grepl("mattingly", genconservative$text) | 
  grepl("kenneth walker", genconservative$text) | grepl("elijah", genconservative$text) | 
  grepl("ahmaud", genconservative$text) | grepl("antifa", genconservative$text) | 
  grepl("protest", genconservative$text) | grepl("protests", genconservative$text) | 
  grepl("protester", genconservative$text) | grepl("protesters", genconservative$text) | 
  grepl("protesting", genconservative$text) | grepl("riot", genconservative$text) | 
  grepl("riots", genconservative$text) | grepl("rioter", genconservative$text) | 
  grepl("rioters", genconservative$text) | grepl("rioting", genconservative$text) | 
  grepl("looter", genconservative$text) | grepl("looters", genconservative$text) | 
  grepl("looting", genconservative$text) | grepl("curfew", genconservative$text) | 
  grepl("demonstration", genconservative$text) | grepl("demonstrations", genconservative$text) | 
  grepl("tear gas", genconservative$text) | grepl("baton", genconservative$text) | 
  grepl("rubber bullet", genconservative$text) | grepl("rubber bullets", genconservative$text) | 
  grepl("autonomous zone", genconservative$text) | grepl("unmarked", genconservative$text) | 
  grepl("peaceful", genconservative$text) | grepl("violent", genconservative$text) | 
  grepl("violence", genconservative$text) | grepl("vandalize", genconservative$text) | 
  grepl("vandalized", genconservative$text) | grepl("vandalizing", genconservative$text) | 
  grepl("confederate", genconservative$text) | grepl("monument", genconservative$text) | 
  grepl("statue", genconservative$text) 
genconservative_tweets <- cbind(genconservative, genconservative_logical)
genconservative_blm <- genconservative %>% filter(genconservative_logical == "TRUE")
genconservative_blm$created_at <- as.POSIXct(genconservative_blm$created_at)
genconservative_blm_gf <- genconservative_blm %>% filter(created_at > ymd("2020-05-25"))

# general conservative data visualization 

## time series
ts_plot(genconservative_blm_gf, by = "days") + 
  labs(x = NULL, y = NULL, 
       title = "Frequency of tweets by general conservatives on BLM and systemic racism", 
       caption = "Data from the last 3,200 tweets of various general conservative users since May 25, 2020") + 
  theme_minimal() + geom_line(color = "seagreen3") + 
  theme(text = element_text(size = 16))

## world cloud

## create a vector source for genconservative
genconservative_vector <- VectorSource(genconservative_blm_gf$text)

## check the class of genconservative_vector
class(genconservative_vector)

## create a corpus object
genconservative_corpus <- Corpus(genconservative_vector)

## make the data uniform (no punctuation/number)
genconservative_corpus <- tm_map(genconservative_corpus, removePunctuation)
genconservative_corpus <- tm_map(genconservative_corpus, removeNumbers)

## remove stopwords
genconservative_corpus <- tm_map(genconservative_corpus, removeWords, stopwords("english"))

## create the term document matrix 
genconservative_tdm <- TermDocumentMatrix(genconservative_corpus)
genconservative_matrix = as.matrix(genconservative_tdm)
genconservative_wordcount <- rowSums(genconservative_matrix)
genconservative_wordcount <- sort(genconservative_wordcount, decreasing = TRUE)
head(genconservative_wordcount)

## create a wordcloud
genconservative_cloud <- data.frame(word = names(genconservative_wordcount), 
                                    freq = genconservative_wordcount)
set.seed(1234)
wordcloud(names(genconservative_wordcount), genconservative_wordcount, min.freq = 1,
          random.order = FALSE, max.words = 100, scale = c(4, 0.5), 
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

## most tweets in a single day by general conservatives
genconservative_blm_gf$created_at = as.Date(genconservative_blm_gf$created_at, format('ymd'))
genconservative_blm_gf %>% count(created_at, sort = TRUE) %>% top_n(5)

# establishment republican data collection

## individual establishment republican data
don_tru <- get_timeline("realDonaldTrump", n = 5000, lang = "en")
lau_ing <- get_timeline("IngrahamAngle", n = 5000, lang = "en")
meg_mcc <- get_timeline("MeghanMcCain", n = 5000, lang = "en")
mik_huc <- get_timeline("GovMikeHuckabee", n = 5000, lang = "en")
mit_rom <- get_timeline("MittRomney", n = 5000, lang = "en")
ted_cru <- get_timeline("tedcruz", n = 5000, lang = "en")
mit_mcc <- get_timeline("senatemajleader", n = 5000, lang = "en")
mar_rub <- get_timeline("marcorubio", n = 5000, lang = "en")
jea_pir <- get_timeline("JudgeJeanine", n = 5000, lang = "en")
new_gin <- get_timeline("newtgingrich", n = 5000, lang = "en")
eva_mcm <- get_timeline("EvanMcMullin", n = 5000, lang = "en")
lin_gra <- get_timeline("LindseyGrahamSC", n = 5000, lang = "en")

## aggregated establishment republican data
estabrep <- rbind(don_tru, lau_ing, meg_mcc, mik_huc, mit_rom, ted_cru, 
                  mit_mcc, mar_rub, jea_pir, new_gin, eva_mcm, lin_gra)
estabrep = as.data.frame(sapply(estabrep, tolower))
estabrep_logical <- grepl("black lives", estabrep$text) | 
  grepl("all lives", estabrep$text) | grepl("blue lives", estabrep$text) | 
  grepl("blm", estabrep$text) | grepl("alm", estabrep$text) | 
  grepl("george floyd", estabrep$text) | grepl("chauvin", estabrep$text) | 
  grepl("police", estabrep$text) | grepl("cops", estabrep$text) | 
  grepl("pigs", estabrep$text) | grepl("qualified immunity", estabrep$text) | 
  grepl("racism", estabrep$text) | grepl("racist", estabrep$text) | 
  grepl("racists", estabrep$text) | grepl("white supremacist", estabrep$text) | 
  grepl("white supremacists", estabrep$text) | grepl("rittenhouse", estabrep$text) | 
  grepl("breonna", estabrep$text) | grepl("mattingly", estabrep$text) | 
  grepl("kenneth walker", estabrep$text) | grepl("elijah", estabrep$text) | 
  grepl("ahmaud", estabrep$text) | grepl("antifa", estabrep$text) | 
  grepl("protest", estabrep$text) | grepl("protests", estabrep$text) | 
  grepl("protester", estabrep$text) | grepl("protesters", estabrep$text) | 
  grepl("protesting", estabrep$text) | grepl("riot", estabrep$text) | 
  grepl("riots", estabrep$text) | grepl("rioter", estabrep$text) | 
  grepl("rioters", estabrep$text) | grepl("rioting", estabrep$text) | 
  grepl("looter", estabrep$text) | grepl("looters", estabrep$text) | 
  grepl("looting", estabrep$text) | grepl("curfew", estabrep$text) | 
  grepl("demonstration", estabrep$text) | grepl("demonstrations", estabrep$text) | 
  grepl("tear gas", estabrep$text) | grepl("baton", estabrep$text) | 
  grepl("rubber bullet", estabrep$text) | grepl("rubber bullets", estabrep$text) | 
  grepl("autonomous zone", estabrep$text) | grepl("unmarked", estabrep$text) | 
  grepl("peaceful", estabrep$text) | grepl("violent", estabrep$text) | 
  grepl("violence", estabrep$text) | grepl("vandalize", estabrep$text) | 
  grepl("vandalized", estabrep$text) | grepl("vandalizing", estabrep$text) | 
  grepl("confederate", estabrep$text) | grepl("monument", estabrep$text) | 
  grepl("statue", estabrep$text) 
estabrep_tweets <- cbind(estabrep, estabrep_logical)
estabrep_blm <- estabrep %>% filter(estabrep_logical == "TRUE")
estabrep_blm$created_at <- as.POSIXct(estabrep_blm$created_at)
estabrep_blm_gf <- estabrep_blm %>% filter(created_at > ymd("2020-05-25"))

# establishment republican data visualization 

## time series
ts_plot(estabrep_blm_gf, by = "days") + 
  labs(x = NULL, y = NULL, 
       title = "Frequency of tweets by establishment republicans on BLM and systemic racism", 
       caption = "Data from the last 3,200 tweets of various establishment republican users since May 25, 2020") + 
  theme_minimal() + geom_line(color = "seagreen3") + 
  theme(text = element_text(size = 16))

## world cloud

## create a vector source for estabrep
estabrep_vector <- VectorSource(estabrep_blm_gf$text)

## check the class of estabrep_vector
class(estabrep_vector)

## create a corpus object
estabrep_corpus <- Corpus(estabrep_vector)

## make the data uniform (no punctuation/number)
estabrep_corpus <- tm_map(estabrep_corpus, removePunctuation)
estabrep_corpus <- tm_map(estabrep_corpus, removeNumbers)

## remove stopwords
estabrep_corpus <- tm_map(estabrep_corpus, removeWords, stopwords("english"))

## create the term document matrix 
estabrep_tdm <- TermDocumentMatrix(estabrep_corpus)
estabrep_matrix = as.matrix(estabrep_tdm)
estabrep_wordcount <- rowSums(estabrep_matrix)
estabrep_wordcount <- sort(estabrep_wordcount, decreasing = TRUE)
head(estabrep_wordcount)

## create a wordcloud
estabrep_cloud <- data.frame(word = names(estabrep_wordcount), 
                             freq = estabrep_wordcount)
set.seed(1234)
wordcloud(names(estabrep_wordcount), estabrep_wordcount, min.freq = 1,
          random.order = FALSE, max.words = 100, scale = c(4, 0.5), 
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

## most tweets in a single day by establishment republicans
estabrep_blm_gf$created_at = as.Date(estabrep_blm_gf$created_at, format('ymd'))
estabrep_blm_gf %>% count(created_at, sort = TRUE) %>% top_n(5)

# far-right conservative data collection

## individual far-right conservative data
joh_bir <- get_timeline("The_JBS", n = 5000, lang = "en")
pam_gel <- get_timeline("PamelaGeller", n = 5000, lang = "en")
rob_spe <- get_timeline("jihadwatchRS", n = 5000, lang = "en")
kev_shi <- get_timeline("Kevin_Shipp", n = 5000, lang = "en")
ste_mil <- get_timeline("redsteeze", n = 5000, lang = "en")
ann_cou <- get_timeline("AnnCoulter", n = 5000, lang = "en")
jes_pet <- get_timeline("JLPtalk", n = 5000, lang = "en")
act_ame <- get_timeline("ACTforAmerica", n = 5000, lang = "en")
bri_gab <- get_timeline("ACTBrigitte", n = 5000, lang = "en")
din_dso <- get_timeline("DineshDSouza", n = 5000, lang = "en")
bil_cun <- get_timeline("Willie700WLW", n = 5000, lang = "en")

## aggregated far-right conservative data
farrightconservative <- rbind(joh_bir, pam_gel, rob_spe, kev_shi, ste_mil,
                              ann_cou, jes_pet, act_ame, bri_gab, din_dso,
                              bil_cun)
farrightconservative = as.data.frame(sapply(farrightconservative, tolower))
farrightconservative_logical <- grepl("black lives", farrightconservative$text) | 
  grepl("all lives", farrightconservative$text) | grepl("blue lives", farrightconservative$text) | 
  grepl("blm", farrightconservative$text) | grepl("alm", farrightconservative$text) | 
  grepl("george floyd", farrightconservative$text) | grepl("chauvin", farrightconservative$text) | 
  grepl("police", farrightconservative$text) | grepl("cops", farrightconservative$text) | 
  grepl("pigs", farrightconservative$text) | grepl("qualified immunity", farrightconservative$text) | 
  grepl("racism", farrightconservative$text) | grepl("racist", farrightconservative$text) | 
  grepl("racists", farrightconservative$text) | grepl("white supremacist", farrightconservative$text) | 
  grepl("white supremacists", farrightconservative$text) | grepl("rittenhouse", farrightconservative$text) | 
  grepl("breonna", farrightconservative$text) | grepl("mattingly", farrightconservative$text) | 
  grepl("kenneth walker", farrightconservative$text) | grepl("elijah", farrightconservative$text) | 
  grepl("ahmaud", farrightconservative$text) | grepl("antifa", farrightconservative$text) | 
  grepl("protest", farrightconservative$text) | grepl("protests", farrightconservative$text) | 
  grepl("protester", farrightconservative$text) | grepl("protesters", farrightconservative$text) | 
  grepl("protesting", farrightconservative$text) | grepl("riot", farrightconservative$text) | 
  grepl("riots", farrightconservative$text) | grepl("rioter", farrightconservative$text) | 
  grepl("rioters", farrightconservative$text) | grepl("rioting", farrightconservative$text) | 
  grepl("looter", farrightconservative$text) | grepl("looters", farrightconservative$text) | 
  grepl("looting", farrightconservative$text) | grepl("curfew", farrightconservative$text) | 
  grepl("demonstration", farrightconservative$text) | grepl("demonstrations", farrightconservative$text) | 
  grepl("tear gas", farrightconservative$text) | grepl("baton", farrightconservative$text) | 
  grepl("rubber bullet", farrightconservative$text) | grepl("rubber bullets", farrightconservative$text) | 
  grepl("autonomous zone", farrightconservative$text) | grepl("unmarked", farrightconservative$text) | 
  grepl("peaceful", farrightconservative$text) | grepl("violent", farrightconservative$text) | 
  grepl("violence", farrightconservative$text) | grepl("vandalize", farrightconservative$text) | 
  grepl("vandalized", farrightconservative$text) | grepl("vandalizing", farrightconservative$text) | 
  grepl("confederate", farrightconservative$text) | grepl("monument", farrightconservative$text) | 
  grepl("statue", farrightconservative$text) 
farrightconservative_tweets <- cbind(farrightconservative, farrightconservative_logical)
farrightconservative_blm <- farrightconservative %>% filter(farrightconservative_logical == "TRUE")
farrightconservative_blm$created_at <- as.POSIXct(farrightconservative_blm$created_at)
farrightconservative_blm_gf <- farrightconservative_blm %>% filter(created_at > ymd("2020-05-25"))

# far-right conservative data visualization 

## time series
ts_plot(farrightconservative_blm_gf, by = "days") + 
  labs(x = NULL, y = NULL, 
       title = "Frequency of tweets by far-right conservatives on BLM and systemic racism", 
       caption = "Data from the last 3,200 tweets of various far-right conservative users since May 25, 2020") + 
  theme_minimal() + geom_line(color = "seagreen3") + 
  theme(text = element_text(size = 16))

## world cloud

## create a vector source for farrightconservative
farrightconservative_vector <- VectorSource(farrightconservative_blm_gf$text)

## check the class of farrightconservative_vector
class(farrightconservative_vector)

## create a corpus object
farrightconservative_corpus <- Corpus(farrightconservative_vector)

## make the data uniform (no punctuation/number)
farrightconservative_corpus <- tm_map(farrightconservative_corpus, removePunctuation)
farrightconservative_corpus <- tm_map(farrightconservative_corpus, removeNumbers)

## remove stopwords
farrightconservative_corpus <- tm_map(farrightconservative_corpus, removeWords, stopwords("english"))

## create the term document matrix 
farrightconservative_tdm <- TermDocumentMatrix(farrightconservative_corpus)
farrightconservative_matrix = as.matrix(farrightconservative_tdm)
farrightconservative_wordcount <- rowSums(farrightconservative_matrix)
farrightconservative_wordcount <- sort(farrightconservative_wordcount, decreasing = TRUE)
head(farrightconservative_wordcount)

## create a wordcloud
farrightconservative_cloud <- data.frame(word = names(farrightconservative_wordcount), 
                                         freq = farrightconservative_wordcount)
set.seed(1234)
wordcloud(names(farrightconservative_wordcount), farrightconservative_wordcount, min.freq = 1,
          random.order = FALSE, max.words = 100, scale = c(4, 0.5), 
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

## most tweets in a single day by far-right conservatives
farrightconservative_blm_gf$created_at = as.Date(farrightconservative_blm_gf$created_at, format('ymd'))
farrightconservative_blm_gf %>% count(created_at, sort = TRUE) %>% top_n(5)

# far-right conspiracy theorist data collection

## individual far-right conspiracy theorist data
pau_wat <- get_timeline("PrisonPlanet", n = 5000, lang = "en")
q_anon_ <- get_timeline("Qanon76", n = 5000, lang = "en")
mar_dic <- get_timeline("MarkDice", n = 5000, lang = "en")
cur_sch <- get_timeline("gehrig38", n = 5000, lang = "en")
crux_qa <- get_timeline("Crux41507251", n = 5000, lang = "en")
gra_eli <- get_timeline("reallygraceful", n = 5000, lang = "en")
pat_177 <- get_timeline("John_F_Kennnedy", n = 5000, lang = "en")
pra_med <- get_timeline("@prayingmedic", n = 5000, lang = "en")
elenoch <- get_timeline("elenochle", n = 5000, lang = "en")
mar_ged <- get_timeline("martingeddes", n = 5000, lang = "en")
mic_sav <- get_timeline("ASavageNation", n = 5000, lang = "en")
dav_kni <- get_timeline("@libertytarian", n = 5000, lang = "en")
hot_jes <- get_timeline("HotepJesus", n = 5000, lang = "en")
tra_bea <- get_timeline("tracybeanz", n = 5000, lang = "en")
hen_mak <- get_timeline("HenryMakow", n = 5000, lang = "en")

## aggregated far-right conspiracy theorist data
farrighttheorist <- rbind(pau_wat, q_anon_, mar_dic, cur_sch, crux_qa, gra_eli,
                          pat_177, pra_med, elenoch, mar_ged, mic_sav, dav_kni,
                          hot_jes, tra_bea, hen_mak)
farrighttheorist = as.data.frame(sapply(farrighttheorist, tolower))
farrighttheorist_logical <- grepl("black lives", farrighttheorist$text) | 
  grepl("all lives", farrighttheorist$text) | grepl("blue lives", farrighttheorist$text) | 
  grepl("blm", farrighttheorist$text) | grepl("alm", farrighttheorist$text) | 
  grepl("george floyd", farrighttheorist$text) | grepl("chauvin", farrighttheorist$text) | 
  grepl("police", farrighttheorist$text) | grepl("cops", farrighttheorist$text) | 
  grepl("pigs", farrighttheorist$text) | grepl("qualified immunity", farrighttheorist$text) | 
  grepl("racism", farrighttheorist$text) | grepl("racist", farrighttheorist$text) | 
  grepl("racists", farrighttheorist$text) | grepl("white supremacist", farrighttheorist$text) | 
  grepl("white supremacists", farrighttheorist$text) | grepl("rittenhouse", farrighttheorist$text) | 
  grepl("breonna", farrighttheorist$text) | grepl("mattingly", farrighttheorist$text) | 
  grepl("kenneth walker", farrighttheorist$text) | grepl("elijah", farrighttheorist$text) | 
  grepl("ahmaud", farrighttheorist$text) | grepl("antifa", farrighttheorist$text) | 
  grepl("protest", farrighttheorist$text) | grepl("protests", farrighttheorist$text) | 
  grepl("protester", farrighttheorist$text) | grepl("protesters", farrighttheorist$text) | 
  grepl("protesting", farrighttheorist$text) | grepl("riot", farrighttheorist$text) | 
  grepl("riots", farrighttheorist$text) | grepl("rioter", farrighttheorist$text) | 
  grepl("rioters", farrighttheorist$text) | grepl("rioting", farrighttheorist$text) | 
  grepl("looter", farrighttheorist$text) | grepl("looters", farrighttheorist$text) | 
  grepl("looting", farrighttheorist$text) | grepl("curfew", farrighttheorist$text) | 
  grepl("demonstration", farrighttheorist$text) | grepl("demonstrations", farrighttheorist$text) | 
  grepl("tear gas", farrighttheorist$text) | grepl("baton", farrighttheorist$text) | 
  grepl("rubber bullet", farrighttheorist$text) | grepl("rubber bullets", farrighttheorist$text) | 
  grepl("autonomous zone", farrighttheorist$text) | grepl("unmarked", farrighttheorist$text) | 
  grepl("peaceful", farrighttheorist$text) | grepl("violent", farrighttheorist$text) | 
  grepl("violence", farrighttheorist$text) | grepl("vandalize", farrighttheorist$text) | 
  grepl("vandalized", farrighttheorist$text) | grepl("vandalizing", farrighttheorist$text) | 
  grepl("confederate", farrighttheorist$text) | grepl("monument", farrighttheorist$text) | 
  grepl("statue", farrighttheorist$text) 
farrighttheorist_tweets <- cbind(farrighttheorist, farrighttheorist_logical)
farrighttheorist_blm <- farrighttheorist %>% filter(farrighttheorist_logical == "TRUE")
farrighttheorist_blm$created_at <- as.POSIXct(farrighttheorist_blm$created_at)
farrighttheorist_blm_gf <- farrighttheorist_blm %>% filter(created_at > ymd("2020-05-25"))

# far-right conspiracy theorist data visualization 

## time series
ts_plot(farrighttheorist_blm_gf, by = "days") + 
  labs(x = NULL, y = NULL, 
       title = "Frequency of tweets by far-right conspiracy theorists on BLM and systemic racism", 
       caption = "Data from the last 3,200 tweets of various far-right conspiracy theorist users since May 25, 2020") + 
  theme_minimal() + geom_line(color = "seagreen3") + 
  theme(text = element_text(size = 16))

## world cloud

## create a vector source for farrighttheorist
farrighttheorist_vector <- VectorSource(farrighttheorist_blm_gf$text)

## check the class of farrighttheorist_vector
class(farrighttheorist_vector)

## create a corpus object
farrighttheorist_corpus <- Corpus(farrighttheorist_vector)

## make the data uniform (no punctuation/number)
farrighttheorist_corpus <- tm_map(farrighttheorist_corpus, removePunctuation)
farrighttheorist_corpus <- tm_map(farrighttheorist_corpus, removeNumbers)

## remove stopwords
farrighttheorist_corpus <- tm_map(farrighttheorist_corpus, removeWords, stopwords("english"))

## create the term document matrix 
farrighttheorist_tdm <- TermDocumentMatrix(farrighttheorist_corpus)
farrighttheorist_matrix = as.matrix(farrighttheorist_tdm)
farrighttheorist_wordcount <- rowSums(farrighttheorist_matrix)
farrighttheorist_wordcount <- sort(farrighttheorist_wordcount, decreasing = TRUE)
head(farrighttheorist_wordcount)

## create a wordcloud
farrighttheorist_cloud <- data.frame(word = names(farrighttheorist_wordcount), 
                                     freq = farrighttheorist_wordcount)
set.seed(1234)
wordcloud(names(farrighttheorist_wordcount), farrighttheorist_wordcount, min.freq = 1,
          random.order = FALSE, max.words = 100, scale = c(4, 0.5), 
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

## most tweets in a single day by far-right conspiracy theorists
farrighttheorist_blm_gf$created_at = as.Date(farrighttheorist_blm_gf$created_at, format('ymd'))
farrighttheorist_blm_gf %>% count(created_at, sort = TRUE) %>% top_n(5)

# alt-right data collection

## individual alt-right data
mik_cer <- get_timeline("Cernovich", n = 5000, lang = "en")
bri_sel <- get_timeline("BrittPettibone", n = 5000, lang = "en")
jac_pos <- get_timeline("JackPosobiec", n = 5000, lang = "en")
and_ngo <- get_timeline("MrAndyNgo", n = 5000, lang = "en")
ian_che <- get_timeline("stillgray", n = 5000, lang = "en")

## aggregated alt-right data
altright <- rbind(mik_cer, bri_sel, jac_pos, and_ngo, ian_che)
altright_logical <- grepl("black lives", altright$text) | 
  grepl("all lives", altright$text) | grepl("blue lives", altright$text) | 
  grepl("blm", altright$text) | grepl("alm", altright$text) | 
  grepl("george floyd", altright$text) | grepl("chauvin", altright$text) | 
  grepl("police", altright$text) | grepl("cops", altright$text) | 
  grepl("pigs", altright$text) | grepl("qualified immunity", altright$text) | 
  grepl("racism", altright$text) | grepl("racist", altright$text) | 
  grepl("racists", altright$text) | grepl("white supremacist", altright$text) | 
  grepl("white supremacists", altright$text) | grepl("rittenhouse", altright$text) | 
  grepl("breonna", altright$text) | grepl("mattingly", altright$text) | 
  grepl("kenneth walker", altright$text) | grepl("elijah", altright$text) | 
  grepl("ahmaud", altright$text) | grepl("antifa", altright$text) | 
  grepl("protest", altright$text) | grepl("protests", altright$text) | 
  grepl("protester", altright$text) | grepl("protesters", altright$text) | 
  grepl("protesting", altright$text) | grepl("riot", altright$text) | 
  grepl("riots", altright$text) | grepl("rioter", altright$text) | 
  grepl("rioters", altright$text) | grepl("rioting", altright$text) | 
  grepl("looter", altright$text) | grepl("looters", altright$text) | 
  grepl("looting", altright$text) | grepl("curfew", altright$text) | 
  grepl("demonstration", altright$text) | grepl("demonstrations", altright$text) | 
  grepl("tear gas", altright$text) | grepl("baton", altright$text) | 
  grepl("rubber bullet", altright$text) | grepl("rubber bullets", altright$text) | 
  grepl("autonomous zone", altright$text) | grepl("unmarked", altright$text) | 
  grepl("peaceful", altright$text) | grepl("violent", altright$text) | 
  grepl("violence", altright$text) | grepl("vandalize", altright$text) | 
  grepl("vandalized", altright$text) | grepl("vandalizing", altright$text) | 
  grepl("confederate", altright$text) | grepl("monument", altright$text) | 
  grepl("statue", altright$text) 
altright_tweets <- cbind(altright, altright_logical)
altright_blm <- altright %>% filter(altright_logical == "TRUE")
altright_blm$created_at <- as.POSIXct(altright_blm$created_at)
altright_blm_gf <- altright_blm %>% filter(created_at > ymd("2020-05-25"))

# alt-right data visualization 

## time series
ts_plot(altright_blm_gf, by = "days") + 
  labs(x = NULL, y = NULL, 
       title = "Frequency of tweets by alt-rights on BLM and systemic racism", 
       caption = "Data from the last 3,200 tweets of various alt-right users since May 25, 2020") + 
  theme_minimal() + geom_line(color = "seagreen3") + 
  theme(text = element_text(size = 16))

## world cloud

## create a vector source for altright
altright_vector <- VectorSource(altright_blm_gf$text)

## check the class of altright_vector
class(altright_vector)

## create a corpus object
altright_corpus <- Corpus(altright_vector)

## make the data uniform (no punctuation/number)
altright_corpus <- tm_map(altright_corpus, removePunctuation)
altright_corpus <- tm_map(altright_corpus, removeNumbers)

## remove stopwords
altright_corpus <- tm_map(altright_corpus, removeWords, stopwords("english"))

## create the term document matrix 
altright_tdm <- TermDocumentMatrix(altright_corpus)
altright_matrix = as.matrix(altright_tdm)
altright_wordcount <- rowSums(altright_matrix)
altright_wordcount <- sort(altright_wordcount, decreasing = TRUE)
head(altright_wordcount)

## create a wordcloud
altright_cloud <- data.frame(word = names(altright_wordcount), 
                             freq = altright_wordcount)
set.seed(1234)
wordcloud(names(altright_wordcount), altright_wordcount, min.freq = 1,
          random.order = FALSE, max.words = 100, scale = c(4, 0.5), 
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

## most tweets in a single day by alt-rights
altright_blm_gf$created_at = as.Date(altright_blm_gf$created_at, format('ymd'))
altright_blm_gf %>% count(created_at, sort = TRUE) %>% top_n(5)

# white supremacist/nationalist data collection

## individual white supremacist/nationalist data
pet_bri <- get_timeline("peterbrimelow", n = 5000, lang = "en")
nic_fue <- get_timeline("NickJFuentes", n = 5000, lang = "en")
lan_lok <- get_timeline("LanaLokteff", n = 5000, lang = "en")
cou_cur <- get_timeline("NewRightAmerica", n = 5000, lang = "en")
fai_gol <- get_timeline("FaithGoldy", n = 5000, lang = "en")
ric_spe <- get_timeline("RichardBSpencer", n = 5000, lang = "en")

## aggregated white supremacist/nationalist data
whitesupnat <- rbind(pet_bri, nic_fue, lan_lok, cou_cur, fai_gol, ric_spe)
whitesupnat_logical <- grepl("black lives", whitesupnat$text) | 
  grepl("all lives", whitesupnat$text) | grepl("blue lives", whitesupnat$text) | 
  grepl("blm", whitesupnat$text) | grepl("alm", whitesupnat$text) | 
  grepl("george floyd", whitesupnat$text) | grepl("chauvin", whitesupnat$text) | 
  grepl("police", whitesupnat$text) | grepl("cops", whitesupnat$text) | 
  grepl("pigs", whitesupnat$text) | grepl("qualified immunity", whitesupnat$text) | 
  grepl("racism", whitesupnat$text) | grepl("racist", whitesupnat$text) | 
  grepl("racists", whitesupnat$text) | grepl("white supremacist", whitesupnat$text) | 
  grepl("white supremacists", whitesupnat$text) | grepl("rittenhouse", whitesupnat$text) | 
  grepl("breonna", whitesupnat$text) | grepl("mattingly", whitesupnat$text) | 
  grepl("kenneth walker", whitesupnat$text) | grepl("elijah", whitesupnat$text) | 
  grepl("ahmaud", whitesupnat$text) | grepl("antifa", whitesupnat$text) | 
  grepl("protest", whitesupnat$text) | grepl("protests", whitesupnat$text) | 
  grepl("protester", whitesupnat$text) | grepl("protesters", whitesupnat$text) | 
  grepl("protesting", whitesupnat$text) | grepl("riot", whitesupnat$text) | 
  grepl("riots", whitesupnat$text) | grepl("rioter", whitesupnat$text) | 
  grepl("rioters", whitesupnat$text) | grepl("rioting", whitesupnat$text) | 
  grepl("looter", whitesupnat$text) | grepl("looters", whitesupnat$text) | 
  grepl("looting", whitesupnat$text) | grepl("curfew", whitesupnat$text) | 
  grepl("demonstration", whitesupnat$text) | grepl("demonstrations", whitesupnat$text) | 
  grepl("tear gas", whitesupnat$text) | grepl("baton", whitesupnat$text) | 
  grepl("rubber bullet", whitesupnat$text) | grepl("rubber bullets", whitesupnat$text) | 
  grepl("autonomous zone", whitesupnat$text) | grepl("unmarked", whitesupnat$text) | 
  grepl("peaceful", whitesupnat$text) | grepl("violent", whitesupnat$text) | 
  grepl("violence", whitesupnat$text) | grepl("vandalize", whitesupnat$text) | 
  grepl("vandalized", whitesupnat$text) | grepl("vandalizing", whitesupnat$text) | 
  grepl("confederate", whitesupnat$text) | grepl("monument", whitesupnat$text) | 
  grepl("statue", whitesupnat$text) 
whitesupnat_tweets <- cbind(whitesupnat, whitesupnat_logical)
whitesupnat_blm <- whitesupnat %>% filter(whitesupnat_logical == "TRUE")
whitesupnat_blm$created_at <- as.POSIXct(whitesupnat_blm$created_at)
whitesupnat_blm_gf <- whitesupnat_blm %>% filter(created_at > ymd("2020-05-25"))

# white supremacist/nationalist data visualization 

## time series
ts_plot(whitesupnat_blm_gf, by = "days") + 
  labs(x = NULL, y = NULL, 
       title = "Frequency of tweets by white supremacists/nationalists on BLM and systemic racism", 
       caption = "Data from the last 3,200 tweets of various white supremacists/nationalist users since May 25, 2020") + 
  theme_minimal() + geom_line(color = "seagreen3") + 
  theme(text = element_text(size = 16))

## world cloud

## create a vector source for whitesupnat
whitesupnat_vector <- VectorSource(whitesupnat_blm_gf$text)

## check the class of whitesupnat_vector
class(whitesupnat_vector)

## create a corpus object
whitesupnat_corpus <- Corpus(whitesupnat_vector)

## make the data uniform (no punctuation/number)
whitesupnat_corpus <- tm_map(whitesupnat_corpus, removePunctuation)
whitesupnat_corpus <- tm_map(whitesupnat_corpus, removeNumbers)

## remove stopwords
whitesupnat_corpus <- tm_map(whitesupnat_corpus, removeWords, stopwords("english"))

## create the term document matrix 
whitesupnat_tdm <- TermDocumentMatrix(whitesupnat_corpus)
whitesupnat_matrix = as.matrix(whitesupnat_tdm)
whitesupnat_wordcount <- rowSums(whitesupnat_matrix)
whitesupnat_wordcount <- sort(whitesupnat_wordcount, decreasing = TRUE)
head(whitesupnat_wordcount)

## create a wordcloud
whitesupnat_cloud <- data.frame(word = names(whitesupnat_wordcount), 
                                freq = whitesupnat_wordcount)
set.seed(1234)
wordcloud(names(whitesupnat_wordcount), whitesupnat_wordcount, min.freq = 1,
          random.order = FALSE, max.words = 100, scale = c(4, 0.5), 
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

## most tweets in a single day by white supremacists/nationalists
whitesupnat_blm_gf$created_at = as.Date(whitesupnat_blm_gf$created_at, format('ymd'))
whitesupnat_blm_gf %>% count(created_at, sort = TRUE) %>% top_n(5)
