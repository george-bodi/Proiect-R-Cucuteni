library(readxl)
library(FactoMineR)
library(factoextra)
library(NbClust)
library(rgl)
library(pixiedust)
library(multcomp)
library(car)
library(nlme)
library(psych)
library(corrplot)
library(htmlwidgets)
library(MASS)
library(effectsize)


Cucuteni_pXRF <- read_excel("Cucuteni_pXRF.xlsx", sheet = "toate fara 15 RSD>10")

df_cuc <- as.data.frame(Cucuteni_pXRF)
#df_cuc <- df_cuc[,3:10]
df_cuc[,1:2] <- lapply(df_cuc[,1:2], as.factor)
#df_cuc[,5:36] <- lapply(df_cuc[,5:36], as.numeric)
#df_cuc <- df_cuc[-15,]                               #eliminam proba 15 ca outlier cu analize facute in suprafata
df_cuc_rare <- df_cuc[-15,c(1:2,8:14)]
#re_num_tot <- df_re[,5:26]


#PCA rare---------

pca_cuc_rare <- PCA(df_cuc_rare[,3:9], graph = FALSE)
summary(pca_cuc_rare)

graf_pca_ind_rare <- fviz_pca_biplot(pca_cuc_rare, addEllipses=FALSE,
                                    #col.ind =df_cuc_rare$TIP,                # cerem colorarea indivizilor in functie de cauza mortii
                                     col.var = "black",                       # cerem cerem reprezentarea variabilelor cu negru
                                     repel = TRUE,                            # cerem ca numele indivizilor sa nu se suprapuna
                                     legend.title = "Grupate pe tip - elemente rare")           # specificam titlul legendei
graf_pca_ind_rare



pca_cuc_rare_vars <- get_pca_var(pca_cuc_rare)
plot_cor_vars <- corrplot(pca_cuc_rare_vars$cos2, is.corr = FALSE) 
plot_contrib_vars <- fviz_contrib(pca_cuc_rare, choice = "var", axes=1:2)
plot_cor_vars
plot_contrib_vars

#pe baza contributiilor la PCA elimin Fe si Sr

#df_cuc_rare <-df_cuc_rare[,-c(5,8)]
df_cuc_rare <-df_cuc_rare[,-8]

############CLUSTERE----------------
gradient_col <-  list(low = "black", mid= "white",high = "red")  # creem un gradient de culori


df_cuc_rare_centr <- as.data.frame(scale(df_cuc_rare[,3:7],           # normalizam coloanele
                                         center = TRUE,       # extragem media din fiecare valoare a fiecarei coloane
                                        scale = TRUE))        # impartim valorile din coloanele centrate la deviatia standard

df_cuc_rare_log <- as.data.frame(log10(df_cuc_rare[,3:7]))        # impartim valorile din coloanele centrate la deviatia standard



cluster_tendency_centr <- get_clust_tendency(df_cuc_rare_centr,    #evaluam posibilitatea de existenta a unor clustere in setul de date
                                       n = 10,                           #specificam numarul de indivizi pentru evaluare
                                       gradient = gradient_col)          # cerem generarea graficului cu gradientul de culori definit

cluster_tendency_centr

cluster_tendency_log <- get_clust_tendency(df_cuc_rare_log,    #evaluam posibilitatea de existenta a unor clustere in setul de date
                                             n = 10,                           #specificam numarul de indivizi pentru evaluare
                                             gradient = gradient_col)          # cerem generarea graficului cu gradientul de culori definit

cluster_tendency_log

##### clustere ierarhice aglomerative ############


clust_cuc_rare_hclust_indet <- eclust(df_cuc_rare_log,                       # cerem calcularea clusterelor
                                FUNcluster = "agnes",             # specificam utilizarea unui algoritm aglomerativ ierarhic
                                hc_metric = "pearson",         # cerem utilizarea matricei de similaritate 
                                hc_method = "average")           # specificam tipul de disimilaritate utilizat
clusts_indet <- fviz_dend(clust_cuc_rare_hclust_indet)
clusts_indet

clust_cuc_rare_hclust_tests <- eclust(df_cuc_rare_log,                       # cerem calcularea clusterelor
                                      FUNcluster = "agnes",             # specificam utilizarea unui algoritm aglomerativ ierarhic
                                      k=4,      
                                      hc_metric = "pearson",         # cerem utilizarea matricei de similaritate 
                                      hc_method = "average")           # specificam tipul de disimilaritate utilizat

fviz_silhouette(clust_cuc_rare_hclust_tests)       
clusts_tests <- fviz_dend(clust_cuc_rare_hclust_tests)
clusts_tests


export_clus_rare <-clust_cuc_rare_hclust_tests$cluster
df_cuc_rare$clusters <- as.factor(export_clus_rare)


pca_cuc_rare <- PCA(df_cuc_rare[,3:7], graph = FALSE)
summary(pca_cuc_rare)

graf_pca_ind_rare <- fviz_pca_biplot(pca_cuc_rare, addEllipses=TRUE, ellipse.level=0.65,
                                     col.ind =df_cuc_rare$clusters,                   # cerem colorarea indivizilor in functie de cauza mortii
                                     #geom.ind = "point",
                                     col.var = "black",                       # cerem cerem reprezentarea variabilelor cu negru
                                     repel = TRUE,                            # cerem ca numele indivizilor sa nu se suprapuna
                                     title = "Overlay of clustering results over PCA",
                                     legend.title = "Clusters")           # specificam titlul legendei
graf_pca_ind_rare
graf_pca_ind_rare+
  geom_text(label=df_cuc_rare$TIP, hjust=0, nudge_y = -0.05, check_overlap = TRUE)

#########RARE PCA 3d---------


#pca_3d_rare <- prcomp(df_cuc_rare[,5:11], scale=TRUE)
#summary(pca_3d_rare)

scores_rare = as.data.frame(pca_cuc_rare$ind)

plot3d(scores_rare[,1:3], 
       size=10,
       col = df_cuc_rare$clusters)
options(rgl.printRglwidget = TRUE)

text3d(scores_rare[,1:3],
       texts=df_cuc_rare$TIP, 
       cex= 1, pos=3)
text3d(scores_rare[,1:3],
       texts=df_cuc_rare$SAMPLE, 
       cex= 1, pos=1)

text3d(pca_cuc_rare$var$coord[,1:3], 
       texts=rownames(pca_cuc_rare$var$coord[,1:3]), 
       col="red", 
       cex=0.8)
#legend3d("topright", legend = c('1', '2'), pch = 16, col = rainbow(2), cex=1, inset=c(0.02))

coords <- NULL
for (i in 1:nrow(pca_cuc_rare$var$coord)) {
  coords <- rbind(coords, 
                  rbind(c(0,0,0),
                        pca_cuc_rare$var$coord[i,1:3]))
}

lines3d(coords, 
        col="red", 
        lwd=1)

rgl.snapshot('3dplot_pca_rare2.png', fmt = 'png')


####model regresie liniara------------

df_cuc_3_clustere <- df_cuc_rare[-c(15,16),-c(1,2)] # elimin clusterul cu un doar doi membri pentru analiza variatiei. De asemenenea, 

df_cuc_3_clustere[, 1:6] <- scale(df_cuc_3_clustere[,1:6])

manova_cuc <- manova(cbind(df_cuc_3_clustere$Ti_avg, df_cuc_3_clustere$Mn_avg, df_cuc_3_clustere$Zn_avg, df_cuc_3_clustere$Rb_avg, df_cuc_3_clustere$Zr_avg)~clusters, df_cuc_3_clustere) 
summary(manova_cuc)
summary.aov(manova_cuc) # variația pe titaniu nu e diferită

library(effectsize)
eta_squared(manova_cuc) # dacă valoarea este mai mare de 0.14, mărimea efectului este mare


#elemente <- cbind(df_cuc_3_clustere$Ti_avg, df_cuc_3_clustere$Mn_avg, df_cuc_3_clustere$Zn_avg, df_cuc_3_clustere$Rb_avg, df_cuc_3_clustere$Zr_avg)

cuc_rare_lda <- lda(df_cuc_3_clustere$clusters~., df_cuc_3_clustere, CV = F)
cuc_rare_lda

lda_df <- data.frame(
  clusters = df_cuc_3_clustere[, "clusters"],
  lda = predict(cuc_rare_lda)$x
)
lda_df

plot(cuc_rare_lda)

ggplot(lda_df) +
  geom_point(aes(x = lda.LD1, y = lda.LD2, color = clusters), size = 4) +
  theme_light()


model_tip=glm(Ti_avg+Mn_avg+Zn_avg+Rb_avg+Zr_avg~TIP, data = df_cuc_3_clustere) #nu exista o diferenta reala - compozitia chimica nu variaza in functie de tipul ceramicii
sumar_model_tip <- summary(model_tip)
sumar_model_tip

write_xlsx(df_cuc_rare, "C:/Users/georg/OneDrive/01. probe ceramice/Ceramica George pXRF/Proiect R/Cucuteni_elemente.xlsx", 
           col_names = TRUE, format_headers = TRUE)

model_cluster=glm(Ti_avg+Rb_avg+Sr_avg+Zr_avg~clus_mink, data = df_cuc_rare)

summary(model_cluster)

out_model_cluster <- dust(model_cluster)


plot(model_cluster)
