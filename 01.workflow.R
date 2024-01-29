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

Cucuteni_pXRF <- read_excel("Cucuteni_pXRF.xlsx", sheet = "toate fara 15 RSD>10")
df_cuc <- as.data.frame(Cucuteni_pXRF)
df_cuc[,1:2] <- lapply(df_cuc[,1:2], as.factor)
df_cuc <- df_cuc[-15,]
#df_cuc <- df_cuc[-27,]
#df_cuc_rare <- df_cuc[,c(1,2,6:8)]
#re_num_tot <- df_re[,5:26]




###PCA tot---------
pca_cuc_expl <- PCA(log10(df_cuc[,c(3:14)]), graph = FALSE)
summary(pca_cuc_expl)

graf_pca_ind_expl <- fviz_pca_biplot(pca_cuc_expl, addEllipses=TRUE,
                                     ellipse.level=0.65,
                                    col.ind = df_cuc$TIP,                   # cerem colorarea indivizilor in functie de cauza mortii
                                    col.var = "black",                       # cerem cerem reprezentarea variabilelor cu negru
                                    repel = TRUE,                            # cerem ca numele indivizilor sa nu se suprapuna
                                    legend.title = "Grupate pe asezari toate elementele")           # specificam titlul legendei
graf_pca_ind_expl

pca_cuc_expl_vars <- get_pca_var(pca_cuc_expl)

plot_cor_vars <- corrplot(pca_cuc_expl_vars$cos2, is.corr = FALSE)
plot_contrib_vars <- fviz_contrib(pca_cuc_expl, choice = "var", axes=1:2)
plot_cor_vars
plot_contrib_vars

df_cuc_centr <- as.data.frame(log(scale(df_cuc[,-c(1,2)],           # normalizam coloanele
                                         center = FALSE,       # extragem media din fiecare valoare a fiecarei coloane
                                         scale = FALSE)))        # impartim valorile din coloanele centrate la deviatia standard


gradient_col <-  list(low = "black", mid= "white",high = "red")  # creem un gradient de culori

cluster_tendency <- get_clust_tendency(df_cuc_centr,    #evaluam posibilitatea de existenta a unor clustere in setul de date
                                       n = 10,                           #specificam numarul de indivizi pentru evaluare
                                       gradient = gradient_col)          # cerem generarea graficului cu gradientul de culori definit

cluster_tendency

##### clustere ierarhice aglomerative ############


nr_clust_ward <- NbClust(df_cuc_centr, distance = "minkowski", method = "ward.D2") #

nr_clust_kmeans <- NbClust(df_cuc_centr, distance = "minkowski", method = "kmeans") #2

nr_clust_average <- NbClust(df_cuc_centr, distance = "minkowski", method = "average")#15

nr_clust_complete <- NbClust(df_cuc_centr, distance = "minkowski", method = "complete")#15

nr_clust_centroid <- NbClust(df_cuc_centr, distance = "minkowski", method = "centroid")#2

nr_clust_median <- NbClust(df_cuc_centr, distance = "minkowski", method = "median")#3



clust_cuc_hclust <- eclust(df_cuc_centr,                       # cerem calcularea clusterelor
                                FUNcluster = "hclust",             # specificam utilizarea unui algoritm aglomerativ ierarhic
                                k=4,      
                                hc_metric = "pearson",         # cerem utilizarea distantei euclidiene
                                hc_method = "average")           # specificam tipul de disimilaritate utilizat

fviz_silhouette(clust_cuc_hclust)       
fviz_dend(clust_cuc_hclust)


export_hclust <-clust_cuc_hclust$cluster
df_cuc$clusters <- as.factor(export_hclust)


pca_cuc <- PCA(df_cuc[,3:13], graph = FALSE)
summary(pca_cuc)

graf_pca_ind_clust <- fviz_pca_biplot(pca_cuc, addEllipses=TRUE, ellipse.level=0.65,
                                     col.ind =df_cuc$clusters,                   # cerem colorarea indivizilor in functie de cauza mortii
                                     #geom.ind = "point",
                                     col.var = "black",                       # cerem cerem reprezentarea variabilelor cu negru
                                     repel = TRUE,                            # cerem ca numele indivizilor sa nu se suprapuna
                                     title = "Overlay of clustering results over PCA",
                                     legend.title = "Clusters")           # specificam titlul legendei
graf_pca_ind_clust
graf_pca_ind_clust+
  geom_text(label=df_cuc$TIP, hjust=0, nudge_y = -0.05, check_overlap = TRUE)

#########RARE PCA 3d---------


#pca_3d_rare <- prcomp(df_cuc_rare[,5:11], scale=TRUE)
#summary(pca_3d_rare)

scores_pca = as.data.frame(pca_cuc$ind)

plot3d(scores_pca[,1:3], 
       size=10,
       col = df_cuc$clusters)

text3d(scores_pca[,1:3],
       texts=df_cuc$TIP, 
       cex= 1, pos=3)
text3d(scores_pca[,1:3],
       texts=df_cuc$SAMPLE, 
       cex= 1, pos=1)

text3d(pca_cuc$var$coord[,1:3], 
       texts=rownames(pca_cuc$var$coord[,1:3]), 
       col="red", 
       cex=0.8)
#legend3d("topright", legend = c('1', '2'), pch = 16, col = rainbow(2), cex=1, inset=c(0.02))

coords <- NULL
for (i in 1:nrow(pca_cuc$var$coord)) {
  coords <- rbind(coords, 
                  rbind(c(0,0,0),
                        pca_cuc$var$coord[i,1:3]))
}

lines3d(coords, 
        col="red", 
        lwd=2)

rgl.snapshot('3dplot_pca_rare2.png', fmt = 'png')


####model regresie liniara------------
model_tip=glm(Al2O4_avg+SiO3_avg+P_avg+K2O_avg+Ca_avg+Ti_avg+Mn_avg+Fe_avg+Zn_avg+Rb_avg+Sr_avg+Zr_avg~clusters, data = df_cuc) #nu exista o diferenta reala - compozitia chimica nu variaza in functie de tipul ceramicii
model_tip
sumar_model_tip <- summary(model_tip)
sumar_model_tip
plot(model_tip)


out_model_tip <- dust(model_tip)
out_model_tip

df_cuc_f19 <- df_cuc[-18, ]
model_gls_clustere <- gls(clusters~Al2O4_avg+SiO3_avg+P_avg+K2O_avg+Ca_avg+Ti_avg+Mn_avg+Fe_avg+Zn_avg+Rb_avg+Sr_avg+Zr_avg, 
                          data = df_cuc, weights=varPower()) # weights=varPower descrie heteroscedasticitatea (la date bivariate, variabila y prezinta heteroscedasticitate daca imprastierea valorilor y depinde de x)
plot(model_gls_clustere)
qqnorm(model_gls_clustere)
summary(model_gls_clustere)
glht(model_gls_clustere, linfct=mcp(clus_mink="Tukey"))

write_xlsx(df_cuc_rare, "C:/Users/georg/OneDrive/01. probe ceramice/Ceramica George pXRF/Proiect R/Cucuteni_elemente.xlsx", 
          col_names = TRUE, format_headers = TRUE)

model_cluster=glm(Ti_avg+Rb_avg+Sr_avg+Zr_avg~clus_mink, data = df_cuc_rare)

summary(model_cluster)

out_model_cluster <- dust(model_cluster)


plot(model_cluster)

posthoc=glht(model_cluster, linfct=mcp(clus_mink ="Tukey"))

summary_posthoc=summary(posthoc)

summary_posthoc

litere_cld=cld(posthoc, level = 0.05)
summary(litere_cld)
plot_cld=plot(litere_cld, sub="Clusters Multiple Comparisons of Means: Tukey Contrasts")
plot_cld

