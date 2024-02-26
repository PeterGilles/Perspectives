library(tidyverse)
library(qs)
library(corregp)

###############################
### Correspondence analysis ###
###############################

# function to extract and recode relevant data from raw_tables, attach Sozialdaten
make_long <- function(table, variable, selection, French_origin = "", domain = ""){
    
  longer_google_df <- qread(paste0("/Users/peter.gilles/Documents/_Daten/Schnessen-App/MappingDialects/pure_shiny/Variatiounsatlas/raw_tables/", table, ".qs")) %>%
    filter(Geschlecht != "Aner") %>%
    mutate(Geschlecht = recode_factor(Geschlecht, "Männlech" = "male",
                               "Weiblech" = "female")) %>%
    filter(Mammesprooch == "Jo") %>%
    filter(Dialektgebiet != "") %>%
    filter(!is.na(Dialektgebiet)) %>%
    mutate(age6 = Alter) %>%
    mutate(Alter = recode_factor(Alter, `≤ 24` = "young",
                          `25 bis 34` = "young",
                          `35 bis 44` = "middle-aged",
                          `45 bis 54` = "middle-aged",
                          `55 bis 64` = "old",
                          `65+` = "old")) %>%
    mutate(Ausbildung = recode_factor(Ausbildung,
                               #`Primaire/Grondschoul` = "just\nPrimaire",
                               `just\nPrimaire` = "Technical\nschool",
                               "Lycée\ntechnique" = "Technical\nschool",
                               "Lycée\nclassique" = "Classical\nsecondary\nschool",
                               "Fachhéichschoul/\nUniversitéit" = "University")) %>%
    mutate(Ausbildung = factor(Ausbildung, levels = c("other", "Technical\nschool", "Classical\nsecondary\nschool", "University"), ordered = TRUE)) %>%
    mutate(langEduIndex_raw = as.numeric(langEduIndex_raw)) %>%
    mutate(Kompetenz_Französisch = recode_factor(`Kompetenz am Franséischen`, "1" = "French\nlow",
                                          "2" = "French\nlow",
                                          "3" = "French\nlow",
                                          "4" = "French\naverage",
                                          "5" = "French\naverage",
                                          "6" = "French\nhigh",
                                          "7" = "French\nhigh")) %>%
    mutate(Kompetenz_Deutsch = recode_factor(`Kompetenz am Däitschen`, "1" = "German low",
                                          "2" = "German low",
                                          "3" = "German low",
                                          "4" = "German average",
                                          "5" = "German average",
                                          "6" = "German high",
                                          "7" = "German high")) %>%
    mutate(urbanity = recode_factor(`Urbanisatioun`, "1\n(= Stad Lëtzebuerg)" = "Capital",
                                      "2\nkleng Stied" = "smaller towns",
                                      "3\nkleng Dierfer" = "rural areas")) %>% 
    pivot_longer(cols = all_of(variable),
                 names_to = "variable",
                 values_to = "variants") %>%
    # take only variants in selection into account
    filter(variants %in% selection)
    
    # mutate all column as factor except langEduIndex_raw
    longer_google_df <- longer_google_df %>%
      mutate_if(is.character, as.factor) 
    
  longer_google_df <- longer_google_df %>%
    pivot_longer(cols = c("Dialektgebiet"),
                 names_to = "geo_type",
                 values_to = "geo_name")
  
  longer_google_df <- longer_google_df %>%
    pivot_longer(cols = c("Geschlecht"),
                 names_to = "geschlecht_type",
                 values_to = "geschlecht_name")
  
  longer_google_df <- longer_google_df %>%
    pivot_longer(cols = c("Ausbildung"),
                 names_to = "Ausbildung_type",
                 values_to = "Ausbildung_name")
  
  longer_google_df <- longer_google_df %>%
    pivot_longer(cols = c("Kompetenz_Französisch"),
                 names_to = "French_type",
                 values_to = "French_name")
  
  longer_google_df <- longer_google_df %>%
    pivot_longer(cols = c("Kompetenz_Deutsch"),
                 names_to = "German_type",
                 values_to = "German_name")
  
  longer_google_df <- longer_google_df %>%
    pivot_longer(cols = c("Sprooch & Educatioun-Index"),
                 names_to = "Index_type",
                 values_to = "Index_name")

  longer_google_df <- longer_google_df %>%
    pivot_longer(cols = c("langEduIndex_raw"),
                 names_to = "Index_raw_type",
                 values_to = "Index_raw_name")
  
  longer_google_df <- longer_google_df %>%
    pivot_longer(cols = c("Alter"),
                 names_to = "Alter_type",
                 values_to = "Alter_name") %>%
  
    # assign 'French' to the words in the column 'French_origin', this singles out all French words
    mutate(French_origin = if_else({variants} %in% {French_origin}, "French", "non-French")) %>%
    
    # add column for domain
    mutate(domain = domain) %>%
    
    dplyr::select(id = surveyID1, variant = variants, variable, French_origin, domain, urbanity, socio_index = `Indice socio-économique`, socio_index_raw = `Indice socio-économique_raw`, region = geo_name, first_language = Mammesprooch, age6, age = Alter_name, gender = geschlecht_name, education = Ausbildung_name, competence_french = French_name, competence_german = German_name)
    
  return(longer_google_df)
}

#test <- make_long("raw_table_111", "Variant_Arbitter", c("Arbitter", "Schiidsrichter", "Schiri"), French_origin = c("Arbitter"), domain= "lexicon")
#test <-  make_long("raw_table_22", "Variant_Pharen", c("Pharen", "Grouss_Luuchten"), French_origin = c("Pharen"))

# call the data collecting function per variable and then bind all rows to one df
# only lexical variables containing a clear French loan

input_data <- bind_rows(
#  make_long("raw_table_2", "Variant_Päiperlek", c("Päiperlek", "Pimpampel", "Schmetterling"), domain= "lexicon"),
  make_long("raw_table_5", "Variant_Ënn", c("Ënn", "Zwiwwel", "Zwibbel"), c("Ënn"), domain= "lexicon") %>%
    mutate(variant = recode_factor(variant, `Zwibbel` = "Zwiwwel")),
#  make_long("raw_table_7", "Variant_Kéiseker", c("Kéiseker", "Igel"), c(""), domain= "lexicon"),
  make_long("raw_table_9", "Variant_Schallimo", c("Schallimo", "Stréihallem"), c("Schallimo"), domain= "lexicon"),
  make_long("raw_table_13", "Variant_Toilettëpabeier", c("Toilettëpabeier", "Cabinetspabeier,"), c("Cabinetspabeier"), domain= "lexicon"),
#  make_long("raw_table_18", "Variant_Märel", c("Märel", "Amsel"), c(""), domain= "lexicon"),
#  make_long("raw_table_15", "Variant_Seejomes", c("[zeːʑoːməs]", "[ʑeːʑoːməs]", "Ameis"), c(""), domain= "lexicon"),
  make_long("raw_table_17", "Variant_Poubelle", c("Poubelle", "Dreckskëscht", "Drecksbac"), c("Poubelle", "Drecksbac"), domain= "lexicon"),
#  make_long("raw_table_19", "Variant_Psycho", c("Ps[ik]o", "Ps[yk]o", "Ps[iɕ]o", "Ps[yɕ]o"), c("Ps[ik]o", "Ps[yk]o"), domain= "phonology"),
  make_long("raw_table_20", "Variant_Chantier", c("Chantier", "Schantjen", "Baustell"), c("Chantier", "Schantjen"), domain= "lexicon"),
  make_long("raw_table_22", "Variant_Pharen", c("Pharen", "Grouss_Luuchten"), c("Pharen"), domain= "lexicon"),
#  make_long("raw_table_24", "Variant_Kartron", c("Cartron", "Carton", "Kartong", "Kartrong"), c("Carton"), domain= "phonology"),
  make_long("raw_table_26", "Variant_Kürbis", c("Kürbis", "Kalbass", "Potiron"), c("Kalbass", "Potiron"), domain= "lexicon"),
  make_long("raw_table_28", "Variant_Plage", c("Plage", "Strand"), c("Plage"), domain= "lexicon"),
  make_long("raw_table_31", "Variant_Wartesall", c("Wartesall", "Salle_d'attente"), c("Salle_d'attente"), domain= "lexicon"),
  make_long("raw_table_33", "Variant_Schaarschtech", c("Schaarschtech", "Kamäin", "Schminni/Schminnee/Cheminée"), c("Schminni/Schminnee/Cheminée"), domain= "lexicon"),
  make_long("raw_table_41", "Variant_Bréifkëscht", c("Bréifkëscht", "Bréifboîte"), c("Bréifboîte"), domain= "lexicon"),
  make_long("raw_table_46", "Variant_Jong1", c("Jong", "Bouf", "Fils"), c("Fils"), domain= "lexicon"),
  make_long("raw_table_49", "Variant_Läffelsgeschier", c("Läffelsgeschier", "Besteck", "Couverten"), c("Couverten"), domain= "lexicon"),
  make_long("raw_table_53", "Variant_Wallis", c("Wallis", "Koffer"), c("Wallis"), domain= "lexicon"),
  make_long("raw_table_57", "Variant_Fernsee", c("Fernsee", "Fernseeër", "Tëlee", "Televisioun"), c("Tëlee", "Televisioun"), domain= "lexicon") %>%
  mutate(variant = recode_factor(variant, `Fernseeër` = "Fernsee")),
  make_long("raw_table_55", "Variant_Pech", c("Pech", "Pëch", "Koll", "Superkleber"), c("Koll"), domain= "lexicon") %>%
  mutate(variant = recode_factor(variant, `Pëch` = "Pech")),
  make_long("raw_table_63", "Variant_H", c("Ha", "Hasch"), c("Hasch"), domain= "lexicon"),
  make_long("raw_table_66", "Variant_Lautsprecher", c("Lautsprecheren", "Haut-Parleuren", "Boxen", "Musek(s)boxen"), c("Haut-Parleuren"), domain= "lexicon") %>%
  mutate(variant = recode_factor(variant, `Lautsprecheren` = "Lautsprecher",
                          `Musek(s)boxen` = "Boxen")),
  make_long("raw_table_68", "Variant_Bëbee", c("Bëbee", "Beebee", "Puppelchen"), c("Bëbee", "Beebee"), domain= "lexicon") %>%
  mutate(variant = recode_factor(variant, `Bëbee` = "Beebee")),
  make_long("raw_table_70", "Variant_Y", c("[iː'gʀæk]", "Ypsilon"), c("[iː'gʀæk]"), domain= "lexicon"),
  make_long("raw_table_72", "Variant_Kannapee", c("Kusch", "Kannapee", "Fotell"), c("Kannapee", "Fotell"), domain= "lexicon"),
  make_long("raw_table_75", "Variant_Déierendoktesch", c("Déierendoktesch", "Déierendokter", "Véidoktesch", "Véidokter", "Veterinairin", "Veterinaire"), c("Veterinaire"), domain= "lexicon") %>%
  mutate(variant = recode_factor(variant, `Veterinairin` = "Veterinaire",
                           `Déierendoktesch` = "Déierendokter",
                           `Véidoktesch` = "Véidokter")),
  make_long("raw_table_78", "Variant_Telecommande", c("[teːleː]commande","Fern(seh)steierung/Fernsteuerung", "Fernbedienung"), c("[teːleː]commande"), domain= "lexicon") %>%
  mutate(variant = recode_factor(variant, `Fern(seh)steierung/Fernsteuerung` = "Fernbedienung")),
  make_long("raw_table_80", "Variant_Eisekuch", c("Eisekuch(en)", "Wafel(en)", "Waffel(en)", "Gaufre(n)", "Gaufer(en)"), c("Gaufer(en)", "Gaufre(n)"), domain= "lexicon") %>%
  mutate(variant= recode_factor(variant, 
                         `Gaufre(n)` = "Gaufer(en)",
                         `Waffel(en)` = "Wafel(en)")),
  make_long("raw_table_82", "Variant_Paprika", c("Paprika", "Poivron"), c("Poivron"), domain= "lexicon"),
  make_long("raw_table_83", "Variant_Police", c("Police", "Polizei"), c("Police"), domain= "lexicon"),
  make_long("raw_table_86", "Variant_Drucker", c("Drucker", "Imprimante",  "Printer"), c("Imprimante"), domain= "lexicon"),
  make_long("raw_table_88", "Variant_Këssen", c("Këssen", "Coussin"), c("Coussin"), domain= "lexicon"),
  make_long("raw_table_111", "Variant_Arbitter", c("Arbitter", "Schiidsrichter"), c("Arbitter"), domain= "lexicon"),
#  make_long("raw_table_13", "Variant_Toilettëpabeier", c("Toilettëpabeier", "Cabinetspabeier"), c("Cabinetspabeier"), domain= "lexicon"),
#  make_long("raw_table_166", "Variant_Demokratie", c("Demokra[t]ie", "Demokra[s]ie"), c("Demokra[s]ie"), domain= "phonology"),
#  make_long("raw_table_172", "Variant_Diplomatie", c("Diploma[t]ie", "Diploma[s]ie"), c("Diploma[s]ie"), domain= "phonology"),
  make_long("raw_table_145", "Variant_Fan_Wuertschatz", c("Fan", "Supporter"), c("Supporter"), domain= "lexicon"),
  make_long("raw_table_154", "Variant_Dëschelduch", c("Dëschelduch", "Napp", "Dëschdecken", "Dëschnapp", "Toile_cirée"), c("Napp", "Dëschnapp", "Toile_cirée"), domain= "lexicon") %>%
  mutate(variant = recode_factor(variant, `Dëschdecken` = "Dëschelduch")),
  make_long("raw_table_156", "Variant_Gefaangen", c("Gefaangen", "Prisonéier"), c("Prisonéier"), domain= "lexicon"),
  make_long("raw_table_176", "Variant_Frigo", c("Frigo", "Frigidaire", "Killschaf"), c("Frigo", "Frigidaire"), domain= "lexicon"),
  make_long("raw_table_162", "Variant_Kannapee", c("Kannapee", "Kusch"), c("Kannapee"), domain= "lexicon"),
  make_long("raw_table_173", "Variant_Schwämm", c("Schwämm", "Piscine"), c("Piscine"), domain= "lexicon"),
  make_long("raw_table_198", "Variant_Klinick", c("Klinick", "Spidol"), c("Klinick"), domain= "lexicon"),
  make_long("raw_table_202", "Variant_Porsch_Genus", c("e_Porsche_(mask.)", "eng_Porsche_(fem.)"), c("eng_Porsche_(fem.)"), domain= "lexicon"),
#  make_long("raw_table_308", "Variant_Cousine", c("Cousine", "Kusing/Kuséng"), c("Cousine"), domain= "phonology"),
  make_long("raw_table_322", "Variant_Zoppeläffel", c("(Zoppe)Läffel", "(Zoppe)Louche"), c("(Zoppe)Louche"), domain= "lexicon"),
  make_long("raw_table_325", "Variant_Geschmaach", c("Geschmaach", "Goût"), c("Goût"), domain= "lexicon"),
  make_long("raw_table_337", "Variant_Begréissung_formell", c("Moien", "Gudde_Moien", "Bonjour", "Bonjue"), c("Bonjour", "Bonjue"), domain= "lexicon") %>%
  mutate(variant = recode_factor(variant, 
                          `Gudde_Moien` = "Moien",
                          `Bonjue` = "Bonjour")),
  make_long("raw_table_363", "Variant_Decisiounen", c("Decisiounen", "Entscheedungen"), c("Decisiounen"), domain= "lexicon"),
  make_long("raw_table_405", "Variant_Suen", c("Geld", "Suen"), c("Suen"), domain= "lexicon"),
  make_long("raw_table_421", "Variant_Exercice", c("Exercice", "(Haus-)aufgab"), c("Exercice"), domain= "lexicon"),
  make_long("raw_table_444", "Variant_Tastatur", c("Tastatur", "Clavier"), c("Clavier"), domain= "lexicon"),
  make_long("raw_table_450", "Variant_Spullsteen", c("Spullsteen", "Lavabo"), c("Lavabo"), domain= "lexicon"),
  make_long("raw_table_521", "Variant_Buttek", c("Buttek", "Geschäft"), c("Buttek"), domain= "lexicon"),
  make_long("raw_table_562", "Variant_desaktivéieren", c("desaktivéieren", "deaktivéieren"), c("desaktivéieren"), domain= "lexicon"),
  make_long("raw_table_569", "Variant_Quall", c("Quall", "Med[yː]s", "Medus", "Jelliskapp"), c("Med[yː]s"), domain= "lexicon") %>%
  mutate(variant = recode_factor(variant, `Medus` = "Med[yː]s")),
  make_long("raw_table_704", "Variant_Couvre-feu", c("Couvre-feu", "Ausgangsspär"), c("Couvre-feu"), domain= "lexicon"),
  make_long("raw_table_717", "Variant_Homeoffice", c("Homeoffice/Homeworking", "Teletravail"), c("Teletravail"), domain= "lexicon"),
  make_long("raw_table_738", "Variant_Impfstoff", c("Impfstoff/Impfung", "Vaccin"), c("Vaccin"), domain= "lexicon"),
) 
# %>%
#   na.omit()

# save as RDS
rio::export(input_data, "input_data.rds")

# data table of variants for publication
# x <- corr_data %>%
#   dplyr::select(variable, variant, French_origin) %>%
#   distinct(variable, variant) %>%
# #  pivot_wider(values_from = variant, names_from = variable)
#  group_by(variable) %>%
#   summarise(variant = paste(variant, collapse = ", ")) %>%
#   ungroup() %>%
#   as.data.frame(.)
# clipr::write_clip(x, object_type = c("table"))

# Correspondence regression
# best correlations:
# - education * age: 72%
# - age * competence_french; 72%
# - urbanity * competence_french: 76%
# - urbanity * age: 71%
# - urbanity * education: 75%
# - competence_french * education: 64%
# - competence_french * gender: 82%

corr.crg <- corregp(variant ~ competence_french * age, data=input_data,
                      part="variable", b=3000)

# Screeplot
screeplot(corr.crg, add_ci=TRUE, type="%")

summary(corr.crg)

# num of participants
num <- corr_data %>%
  distinct(id) %>%
  count() 

# ANOVA Table:
anova(corr.crg, nf=2)

# first plot
#plot(corr.crg, x_ell = TRUE, xsub = c("age", "competence_french"))

# Colors for plotting:
corr.col <- ifelse(xtabs(~variant + French_origin, data=corr_data)[,"French"] > 0,
                       "green3", "blue")

#  (Mono)Plot of the variants:
# plot(corr.crg, xsub=NA, col_btm=corr.col, cex_btm=0.7, cex_top=0.7, font_btm= 1, font_top=2, hlim=c(-0.5,0.6), vlim=c(-0.23,0.33), 
#      hlab = "Latent variable 1: 'Age'", vlab = "Latent variable 2: 'French preference'", add_ori=TRUE)

# biplot
# Oppassen: Outlier mit Extremwerten 
plot(corr.crg, x_ell=TRUE, xsub=c("competence_french", "age"), col_btm=corr.col, col_top="orange",
     cex_btm=0.7, cex_top=0.7, font_btm= 1, font_top=2, hlim=c(-1.2,0.9), vlim=c(-1,1), 
     hlab = "Latent variable 1: 'xxx'", vlab = "Latent variable 2: 'xxx'", add_ori=TRUE)

# # alternative plot with ggplot
# test <- as.data.frame(corr.crg[["y"]])
# test$words <- rownames(test)
# 
# ggplot(test) +
#   geom_text(aes(col= corr.col, x=`1`, y=`2`, label = words))

# plot3d(corr.crg, x_ell=TRUE, xsub=c("gender", "competence_french", "education"), col_btm=corr.col, col_top="orange",
#        cex_btm=0.75, cex_top=0.75, font_btm=3, font_top=2, hlim=c(-1,1), vlim=c(-1,1), add_ori=TRUE)

# Association graph:
agplot(corr.crg, axes=1:2, xsub="gender.competence_french", cex=0.75, ycol=corr.col,
         lcol=c("black","red"))


###################
### Mixed Model ###
###################

# see: https://slcladal.github.io/regression.html#Random_Effects
library(lme4)
library(sjPlot)

data <- input_data %>%
  # filter for domain (lexicon or phonology)
  filter(domain == "lexicon") %>%
  # convert binary variable to values 0 and 1
  mutate(across(French_origin, str_replace, "non-French", "0")) %>%
  mutate(across(French_origin, str_replace, "French", "1")) %>%
  mutate(French_origin = as.integer(French_origin)) %>%
  mutate(age = factor(age, ordered = FALSE)) %>%
  mutate(age6 = factor(age6, ordered = TRUE)) %>%
  mutate(education = factor(education, ordered = FALSE)) %>%
  mutate(competence_german = factor(competence_german, ordered = FALSE)) %>%
  mutate(competence_french = factor(competence_french, ordered = FALSE)) %>%
  mutate(urbanity = factor(urbanity, ordered = FALSE))

# baseline
# testing for random effects, creating baseline models
m0.glm <- glm(French_origin ~ 1, family = "binomial", data = data)
m0.lmer <- lmer(formula=French_origin ~ 1 + (1|id) + (1|variable), REML = T, data = data)

# testing if random effects are justified
AIC(logLik(m0.glm))

AIC(logLik(m0.lmer))
# model with random effects is better as it has lower AIC

# stepwise fitting of the model
# mixed model + random effects
# id = speaker; variable = word
m1.lmer <- lmer(formula = French_origin ~ age + (1 |id) + (1|variable),
                data = data, REML = T)

tab_model(m1.lmer)
anova(m1.lmer, m0.lmer, test = "Chi")

# add further factors
m2.lmer <- update(m1.lmer, .~.+ gender)

# compare models                
anova(m2.lmer, m1.lmer, test = "Chi")
tab_model(m2.lmer)

# maybe add interactions
m2.lmer <- update(m1.lmer, .~.+ gender*age)
# gender do not add to the model -> excluded

# compare models                
anova(m2.lmer, m1.lmer, test = "Chi")
tab_model(m2.lmer)
# same for the interaciton with age > gender or interaction excluded

m2.lmer <- update(m1.lmer, .~.+ urbanity)

# compare models                
anova(m3.lmer, m2.lmer, test = "Chi")
tab_model(m3.lmer)

m4.lmer <- update(m3.lmer, .~.+ region)

# compare models                
anova(m4.lmer, m3.lmer, test = "Chi")
tab_model(m4.lmer)

# best model
summary(m4.lmer)
tab_model(m4.lmer)

# -> age6 (6 age groups) is significant as opposed to age (3 groups): however, 
# older speaker do tend to have MORE french loans, which is weird
# -> urbanity, education, competence_french are the most important factors
# -> no interactions significant

# diagnostics
plot(m1.lmer, variable ~ resid(.), abline = 0 ) # generate diagnostic plots
plot(m1.lmer, resid(., type = "pearson") ~ fitted(.) | variable, id = 0.05, 
     adj = -0.3, pch = 20, col = "gray40")

# not working!
# new model using weights
# generate models
library(nlme)
m4.lme = lme(French_origin ~ langEduIndex, random = ~1|id/variable, data = data, method = "ML")
m5.lme <- update(m4.lme, weights = varIdent(form = ~ 1|id * 1|variable))

# compare models
anova(m5.lme, m4.lme)

###############
### BETAREG ###
###############

library(betareg)

#data("GasolineYield", package = "betareg")
#gy_logit <- betareg(yield ~ batch + temp, data = GasolineYield)
#summary(gy_logit)

beta <- betareg(formula = French_origin ~ langEduIndex_raw,
                data = data)

#############
### RESTE ###
#############

############
### FAMD ###
############

# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/115-famd-factor-analysis-of-mixed-data-in-r-essentials/

library("FactoMineR")
res.famd <- FAMD(corr_data %>%
                   dplyr::select(variant, age, education), graph = FALSE)

print(res.famd)

library("factoextra")
eig.val <- get_eigenvalue(res.famd)
head(eig.val)

fviz_screeplot(res.famd)


### MCA
df <- corr_data %>%
  dplyr::select(variant, variable, langEduIndex, French_origin)

mca1 <- MCA(df, graph = FALSE)

# table of eigenvalues
mca1$eig

# number of categories per variable
cats = apply(df, 2, function(x) nlevels(as.factor(x)))

# data frame with variable coordinates
mca1_vars_df = data.frame(mca1$var$coord, Variable = rep(names(cats), cats))

# data frame with observation coordinates
mca1_obs_df = data.frame(mca1$ind$coord)

# plot of variable categories
ggplot(data=mca1_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR")

