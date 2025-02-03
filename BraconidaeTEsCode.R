
#loading packages
#for making life easier data-wise
library(dplyr)
library(readxl)
library(reshape2) 
library(tidyverse)
library(readxl)
library(janitor)
library(writexl)
#for graphing
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(wesanderson)
library(ggiraphExtra)
library(ggeasy)
library(ggpubr)
library(hrbrthemes)
library(moonBook)
library(ggiraph)
library(sjmisc)
#for bayesian
library(cmdstanr)
library(rethinking)
library(bbmle)
library(nlme)
library(lme4)     # for fitting GLMMs
library(lattice)
#for phylogenetic signal
library(phylosignal)
library(adephylo)
library(ape)
library(phylobase)
library(phytools)
library(picante)
#other
library(devtools)
#1. read in the data
  #input= the outputs of OneCode
  #output= a combined dataframe of all results
##code notes
  #rbind= combines the data as rows based on a common column (in this case scientific_name)
  #sep="\t" separate columns by tabs (since OneCode outputs as a tab-delimited)
  #comment.char="" do not remove comments (or you will lose data)
  #since my analysis is at the order level and not family, I am filtering to just include the summary lines
  #I am using the function separate() in order to get an order-level column while still keeping the superfamiliy data
  #mutate and gsub are used to remove the #'s from the summary lines
data<- rbind(read.table(file ="TEOutputs/Aleiodes2TE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Aleiodes2"),
            read.table(file ="TEOutputs/AleiodesalternatorTE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Aleiodesalternator"),
             read.table(file ="TEOutputs/AleiodesleptofemurgenomeTE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Aleiodesleptofemur"),
             read.table(file ="TEOutputs/AleiodestestaceusTE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Aleiodestestaceus"),
             read.table(file ="TEOutputs/AphidiuscolemaniTE", sep = "\t", comment.char = "",header =FALSE) %>%
               mutate(scientific_name = "Aphidiuscolemani"), 
             read.table(file ="TEOutputs/Aphidiuservi1TE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Aphidiuservi1"),
             read.table(file ="TEOutputs/AphidiusgifuensisTE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Aphidiusgifuensis"),
             read.table(file ="TEOutputs/Asobarajaponica1TE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Asobarajaponica1"),
             read.table(file ="TEOutputs/ChelonusformosanusTE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Chelonusformosanus"),
             read.table(file ="TEOutputs/ChelonusinsularisTE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Chelonusinsularis"),
             read.table(file ="TEOutputs/CoeliniusTE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Coelinius"),
             read.table(file ="TEOutputs/CotesiachilonisTE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Cotesiachilonis"),
             read.table(file ="TEOutputs/CotesiacongregataTE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Cotesiacongregata"),
             read.table(file ="TEOutputs/CotesiaglomerataTE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Cotesiaglomerata"),
             read.table(file ="TEOutputs/CotesiatyphaeTE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Cotesiatyphae"),
             read.table(file ="TEOutputs/Diachasmaalloeum1TE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Diachasmaalloeum1"),
             read.table(file ="TEOutputs/DiachasmimorphalongicaudataTE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Diachasmimorphalongicaudata"),
             read.table(file ="TEOutputs/Dinocampuscoccinellae1genomeTE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Dinocampuscoccinellae1"),
             read.table(file ="TEOutputs/FopiusarisanusTE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Fopiusarisanus"),
             read.table(file ="TEOutputs/LysiphlebusfabarumTE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Lysiphlebusfabarum"),
             read.table(file ="TEOutputs/MacrocentruscingulumTE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Macrocentruscingulum"),
             read.table(file ="TEOutputs/MacrocentrusTE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Macrocentrus"),
             read.table(file ="TEOutputs/MeteoruscinctellusTE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Meteoruscinctellus"),
             read.table(file ="TEOutputs/MeteorusTE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Meteorus"),
             read.table(file ="TEOutputs/Meteoruscolon1TE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Meteoruscolon1"),
             read.table(file ="TEOutputs/Microctonusaethiopoides1TE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Microctonusaethiopoides1"),
             read.table(file ="TEOutputs/Microctonusbrassicae1TE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Microctonusbrassicae1"),
             read.table(file ="TEOutputs/MicroctonushyperodaeTE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Microctonushyperodae"),
             read.table(file ="TEOutputs/Microplitisdemolitor2TE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Microplitisdemolitor2"),
             read.table(file ="TEOutputs/MicroplitismanilaeTE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Microplitismanilae"),
             read.table(file ="TEOutputs/MicroplitismediatorTE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Microplitismediator"),
             read.table(file ="TEOutputs/RhaconotusTE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Rhaconotus"),
             read.table(file ="TEOutputs/ZelealbiditarsusTE", sep = "\t", comment.char = "", header =FALSE) %>%
               mutate(scientific_name = "Zelealbiditarsus")) %>% 
  filter(V2== "All_elements") %>% filter(V1 != "###") %>%
  select(V1,V5, V7,scientific_name) %>%
  rename (TEFamily = V1,
          Copies = V5,
          Total_Bp = V7) %>%
#TEFamily = repeat class/family of the TE
#Copies=  total number of copies reconstructed from the hits (if the --strict option was selected, this number can be null, meaning that none of the fragments passed our 80-80 rule)
#Total_Bp, total number of base pairs corresponding to a given TE for the analyzed query sequence
  filter(Total_Bp != 0)  %>% 
  separate(TEFamily, c('Class', 'TEfamily'), sep = "/") %>%
  mutate(Class = as.character(Class),
         TEfamily = as.character(TEfamily)) %>%
  mutate(Class = gsub("###*","",Class)) %>%
  mutate(Class= gsub("Type:Unknown", "Unknown", Class))
  
#2. Updating the TE taxonomy
  #input=the dataframe you created in step 1
  #output=a new column to that dataframe that designates TE Order
###code notes
#the function mutate with ifelse doesnt work for categorical to categorical data
#instead have to use independent function if_else and base r for making new column
#there is probably a specific operator that lets you replace all cryptons in one line but i could not figure it out
data$TEOrder <- if_else(data$TEfamily == "RC", "Helitrons",
                if_else(data$TEfamily == "Penelope", "PLE",
                if_else(data$TEfamily == "DIRS", "DIRS",
                if_else(data$TEfamily == "Maverick", "Mavericks",
                if_else(data$TEfamily == "Crypton-A" |
                                  data$TEfamily == "Crypton-F" | 
                                  data$TEfamily == "Crypton-H" | 
                                  data$TEfamily == "Crypton-I" |
                                  data$TEfamily =="Crypton-V", "Cryptons",
                if_else(data$TEfamily == "Helitron", "Helitrons",
                if_else(data$Class == "LTR", "LTR",
                if_else(data$Class == "LINE", "LINE",
                if_else(data$Class == "SINE", "SINE",
                if_else(data$Class == "DNA", "TIR",
                        false = "-"))))))))))
#3. ##read taxonomy
taxonomy <- read_excel("C:/Users/Rachel/Desktop/Rfiles2024/bractaxonomy.xlsx")

#4.Calculating TE content
#separating out the summary so dont double the values
#making sure the unclassified are included in the data but removing the non TE
unclassified <- data%>% filter(Class == "Unknown") %>% 
  mutate(TEOrder= Class) %>%
  select(-Class, -TEfamily)
order <- na.omit(data) %>% select(-Class, -TEfamily)
summary<-full_join(bind_rows(order,unclassified),taxonomy) %>% 
  select(-Species,-Lineages,-Genome_GC, -Busco) %>%
  group_by(scientific_name) %>%
  mutate(sumTElength = sum(Total_Bp),
         sumTEcopies = sum(Copies),
         TEcontent =sumTElength/GenomeLength)%>%
  ungroup() %>%
  group_by(scientific_name,TEOrder) %>%
  mutate(OrderSum=sum(Total_Bp),
         OrderCopies =sum(Copies),
         propGenlength = OrderSum/GenomeLength,
         propTElength =OrderSum/sumTElength)%>%
  select(-Total_Bp,-Copies) %>%
  distinct(scientific_name, .keep_all = TRUE) %>%
  ungroup()%>%
  mutate(logGL= log10(GenomeLength),
         logTE= log10(sumTElength),
         logCopies= log10(OrderCopies))
#5. lollipop graph 
summary %>%
  arrange(TaxonNumber)%>%
  ggplot(aes(x= fct_reorder(scientific_name,-TaxonNumber), y=TEcontent)) +
  geom_point(aes(size = GenomeLength)) + 
  geom_segment( aes(x=scientific_name, xend=scientific_name, y=0, yend=TEcontent))+
  labs(x= "", y= "Proportion of TE in Genome") + 
  coord_flip() +
  guides(size = FALSE) +
  theme(legend.position="bottom")
#####################

#6 stacked bar graph for TE content
#summary table of total genome proportion by TE Order with Unclassified Included
TEORDERpropGenome <- summary %>%
  select(scientific_name, TEOrder, propGenlength)%>%
  pivot_wider(names_from = TEOrder, values_from = propGenlength)
#summary table of relative TE proportion by Order with Unclassified included 
TEORDERpropTE <- summary %>%
  select(scientific_name, TEOrder, propTElength)%>%
  pivot_wider(names_from = TEOrder, values_from = propTElength)

#The following code allows me to put the colors in order of TE taxonomy since the default is alphabetical
TEProportion<- summary%>%
  mutate(OrderTE =
           if_else(TEOrder == "LTR",1,
                   if_else(TEOrder == "DIRS", 2,
                           if_else(TEOrder == "PLE",3,
                                   if_else(TEOrder == "LINE", 4,
                                           if_else(TEOrder == "SINE", 5,
                                                   if_else(TEOrder == "TIR",6,
                                                           if_else(TEOrder == "Cryptons", 7,
                                                                   if_else(TEOrder == "Helitrons", 8,
                                                                           if_else(TEOrder =="Mavericks",9,
                                                                                   false = NA)))))))))) %>%
  select(scientific_name, TaxonNumber, TEOrder, OrderTE, propTElength)

#graphing the proportion of each TE Order to total TE content but with Unclassified greyed out
TEProportion %>%
  filter(TEOrder != "Unknown") %>%
  ggplot(aes(x= fct_reorder(scientific_name,-TaxonNumber), y=propTElength, fill = fct_reorder(TEOrder,OrderTE, .na_rm=FALSE))) +
  geom_col() +
  scale_fill_brewer(palette="Spectral") +
  labs(x= "Scientific Name", y= "Proportion of TE Length", fill = "TE Order") + 
  coord_flip()

class<- na.omit(data) %>% select(-TEfamily,-TEOrder) 
class$ClassNew <- if_else(class$Class == "LTR" |
                         class$Class == "SINE" |
                         class$Class == "LINE", "Retro",
                       if_else(class$Class == "DNA", "DNA",
                               false= "NA"))

class2<-class%>%
  select(-Class) %>%
  group_by(scientific_name) %>%
  mutate(sumTEclasslength = sum(Total_Bp),
         sumTEclasscopies= sum(Copies))%>%
  ungroup() %>%
  group_by(scientific_name,ClassNew) %>%
  mutate(ClassSum=sum(Total_Bp),
         ClassCopies =sum(Copies),
         propTEclasscopies =ClassCopies/sumTEclasscopies,
         propTEclasslength =ClassSum/sumTEclasslength)%>%
  select(-Total_Bp,-Copies) %>%
  distinct(scientific_name, .keep_all = TRUE) %>%
  ungroup() %>%
  select(-sumTEclasslength, -sumTEclasscopies) %>%
  full_join(taxonomy)

DNA<-class2%>% filter(ClassNew == "DNA")
mean(DNA$propTEclasslength)
Retro <-class2%>%filter(ClassNew == "Retro")
mean(Retro$propTEclasslength)

DNAcyc<-DNA%>%filter(Lineage =="Cyclostome")
DNAnoncyc<-DNA %>%filter(Lineage =="Non-Cyclostome")

shapiro.test(DNAcyc$propTEclasslength)
#normal
shapiro.test(DNAnoncyc$propTEclasslength)
#normal
#both normal suprisingly so ttest instead of MWU
#checkvar
var.test(DNAcyc$propTEclasslength, DNAnoncyc$propTEclasslength)
#yes equal var
t.test(DNAcyc$propTEclasslength, DNAnoncyc$propTEclasslength, 
       alternative ="two.sided",
       paired= FALSE,
       var.equal = TRUE)
###
#unknown
unk<-summary%>% filter(TEOrder=="Unknown")
###
#7. Minor Calculations
Calculate<-summary %>% 
  select(scientific_name, TEcontent)%>%
  distinct(scientific_name, .keep_all = TRUE) 

min(Calculate$TEcontent)
max(Calculate$TEcontent)
sd(Calculate$TEcontent)
mean(Calculate$TEcontent) 
#####
#te content of genome by lineage
propgenome <- summary %>%
  select(scientific_name, TEcontent, Lineage)%>%
  distinct(scientific_name, .keep_all = TRUE) 

propgenomecyc<- propgenome %>% filter(Lineage == "Cyclostome")
mean(propgenomecyc$TEcontent)
sd(propgenomecyc$TEcontent) 
propgenomenoncyc<- propgenome %>% filter(Lineage == "Non-Cyclostome")
mean(propgenomenoncyc$TEcontent) 
sd(propgenomenoncyc$TEcontent) 

shapiro.test(propgenomecyc$TEcontent)
#yes normal
shapiro.test(propgenomenoncyc$TEcontent)
#not normal
wilcox.test(propgenomecyc$TEcontent, propgenomenoncyc$TEcontent, paired=FALSE)
wilcox.test(propgenomenoncyc$TEcontent, propgenomecyc$TEcontent, paired=FALSE)
#####
#8. Heatmap for Copy #
#Summary table of copy number by TE order with Unclassified included
TEORDERCopy <- summary %>%
  select(scientific_name, TEOrder, OrderCopies)%>%
  pivot_wider(names_from = TEOrder, values_from = OrderCopies) 

#The following code allows me to put the colors in order of TE taxonomy since the default is alphabetical
#I am also removing unclassified as to not skew the heatmap
#Using Log transformed copy number for skew prevention as well
TECopy<- summary %>%
  filter(TEOrder != "Unknown") %>%
  mutate(OrderTE =
           if_else(TEOrder == "LTR",1,
                   if_else(TEOrder == "DIRS", 2,
                           if_else(TEOrder == "PLE",3,
                                   if_else(TEOrder == "LINE", 4,
                                           if_else(TEOrder == "SINE", 5,
                                                   if_else(TEOrder == "TIR",6,
                                                           if_else(TEOrder == "Cryptons", 7,
                                                                   if_else(TEOrder == "Helitrons", 8,
                                                                         if_else(TEOrder =="Mavericks",9,
                                                                                   false = NA)))))))))) %>%
  select(scientific_name, TaxonNumber, TEOrder, OrderTE, logCopies)

TECopy %>%
  ggplot(aes(x= fct_reorder(scientific_name,-TaxonNumber), y=fct_reorder(TEOrder, OrderTE), fill =logCopies)) +
  geom_tile() +
  scale_fill_distiller(palette = "YlOrRd", na.value = "white", direction = 1)+
  labs(x= "Scientific Name", y= "Transposable Element Order", fill= "log(# of Copies)") + 
  scale_y_discrete(position = "right") +
  coord_flip()
######################
#9. boxplots for proportion
#organizing the data so I can do lineage calculations
TEORDERCopy<- TEORDERCopy %>% full_join(taxonomy)
copycyc<- TEORDERCopy%>% filter(Lineage == "Cyclostome")
copynoncyc<- TEORDERCopy %>% filter(Lineage=="Non-Cyclostome")
#
TEORDERpropTE<- TEORDERpropTE %>% full_join(taxonomy)
propTEcyc<-TEORDERpropTE %>% filter(Lineage == "Cyclostome")
propTEnoncyc<-TEORDERpropTE %>% filter(Lineage == "Non-Cyclostome")

copynoncyc<-copynoncyc %>% replace(is.na(.), 0)
copycyc<-copycyc %>% replace(is.na(.), 0)   
propTEcyc<-propTEcyc%>% replace(is.na(.), 0)  
propTEnoncyc<-propTEnoncyc%>% replace(is.na(.), 0)  


##
###Unclassified
#boxplot of proportion of Unclassified to total TE content
TEORDERpropTE %>% 
  full_join(taxonomy) %>%
  ggplot(aes(x= "" ,y=Unknown)) +
  geom_boxplot() +
  ylab("Proportion") +
  xlab("")+
  geom_jitter(aes(y=Unknown, alpha=0.5, size=TEORDERCopy$Unknown)) +
  guides(alpha= FALSE, size= FALSE) +
  labs(title = "Distribution of Unclassified TE Content") 
#boxplot unclassified copy
TEORDERCopy %>% 
  full_join(taxonomy) %>%
  ggplot(aes(x= "" ,y=Unknown)) +
  geom_boxplot() +
  ylab("Copy Number") +
  xlab("")+
  geom_jitter(aes(y=Unknown, alpha=0.5, size=TEORDERpropTE$Unknown)) +
  guides(alpha= FALSE, size= FALSE) +
  labs(title = "Distribution of Unclassified Copy Number") 
#mean and sd for total unknown prop
mean(TEORDERpropTE$Unknown)
sd(TEORDERpropTE$Unknown)
###LTR
#boxplot of proportion of LTR to total TE content by lineage
TEORDERpropTE %>% 
  full_join(taxonomy) %>%
  ggplot(aes(x= "" ,y=LTR)) +
  geom_boxplot() +
  ylab("Proportion") +
  xlab("")+
  geom_jitter(aes(y=LTR, alpha=0.5, size=TEORDERCopy$LTR)) +
  guides(alpha= FALSE, size= FALSE) +
  labs(title = "Distribution of LTR TE Content by Lineage") +
  facet_wrap(~Lineage)
#boxplot of LTR copy number by lineage
TEORDERCopy %>% 
  full_join(taxonomy) %>%
  ggplot(aes(x= "" ,y=LTR)) +
  geom_boxplot() +
  ylab("Copy Number") +
  xlab("")+
  geom_jitter(aes(y=LTR, alpha=0.5, size=TEORDERpropTE$LTR)) +
  guides(alpha= FALSE, size= FALSE) +
  labs(title = "Distribution of LTR Copy Number by Lineage") +
  facet_wrap(~Lineage)
##calculations
#mean and sd for total copy and prop
mean(TEORDERpropTE$LTR)
sd(TEORDERpropTE$LTR)
mean(TEORDERCopy$LTR)
sd(TEORDERCopy$LTR)
#mean and sd for cyclostome copy and prop
mean(copycyc$LTR)
sd(copycyc$LTR)
mean(propTEcyc$LTR)
sd(propTEcyc$LTR)
#mean and sd for non cyc copy and prop
mean(copynoncyc$LTR)
sd(copynoncyc$LTR)
mean(propTEnoncyc$LTR)
sd(propTEnoncyc$LTR)
###DIRS
#boxplot of proportion of DIRS to total TE content by lineage
TEORDERpropTE %>% 
  full_join(taxonomy) %>%
  ggplot(aes(x= "" ,y=DIRS)) +
  geom_boxplot() +
  ylab("Proportion") +
  xlab("")+
  geom_jitter(aes(y=DIRS, alpha=0.5, size=TEORDERCopy$DIRS)) +
  guides(alpha= FALSE, size= FALSE) +
  labs(title = "Distribution of DIRS TE Content by Lineage") +
  facet_wrap(~Lineage)
#boxplot of copy number of DIRS by lineage
TEORDERCopy %>% 
  full_join(taxonomy) %>%
  ggplot(aes(x= "" ,y=DIRS)) +
  geom_boxplot() +
  ylab("Copy Number") +
  xlab("")+
  geom_jitter(aes(y=DIRS, alpha=0.5, size=TEORDERpropTE$DIRS)) +
  guides(alpha= FALSE, size= FALSE) +
  labs(title = "Distribution of DIRS Copy Number by Lineage") +
  facet_wrap(~Lineage)
##calculations
#na.rm=TRUE is needed for calculations or else R gets confused by the empty entries
#mean and sd for total prop  and copy
mean(TEORDERpropTE$DIRS,na.rm=TRUE)
sd(TEORDERpropTE$DIRS,na.rm=TRUE)
mean(TEORDERCopy$DIRS,na.rm=TRUE)
sd(TEORDERCopy$DIRS,na.rm=TRUE)
#mean and sd for cyclostome prop and copy
mean(copycyc$DIRS,na.rm=TRUE)
sd(copycyc$DIRS,na.rm=TRUE)
mean(propTEcyc$DIRS,na.rm=TRUE)
sd(propTEcyc$DIRS,na.rm=TRUE)
#mean and sd for noncyc prop and copy
mean(copynoncyc$DIRS,na.rm=TRUE)
sd(copynoncyc$DIRS,na.rm=TRUE)
mean(propTEnoncyc$DIRS,na.rm=TRUE)
sd(propTEnoncyc$DIRS,na.rm=TRUE)
###PLE
#boxplot of proportion of PLE to total TE content by lineage
TEORDERpropTE %>% 
  full_join(taxonomy) %>%
  ggplot(aes(x= "" ,y=PLE)) +
  geom_boxplot() +
  ylab("Proportion") +
  xlab("")+
  geom_jitter(aes(y=PLE, alpha=0.5, size=TEORDERCopy$PLE)) +
  guides(alpha= FALSE, size= FALSE) +
  labs(title = "Distribution of PLE TE Content by Lineage") +
  facet_wrap(~Lineage)
#boxplot of copy number of PLE by lineage
TEORDERCopy %>% 
  full_join(taxonomy) %>%
  ggplot(aes(x= "" ,y=PLE)) +
  geom_boxplot() +
  ylab("Copy Number") +
  xlab("")+
  geom_jitter(aes(y=PLE, alpha=0.5, size=TEORDERpropTE$PLE)) +
  guides(alpha= FALSE, size= FALSE) +
  labs(title = "Distribution of PLE Copy Number by Lineage") +
  facet_wrap(~Lineage)
##calculations
#na.rm=TRUE is needed for calculations or else R gets confused by the empty entries
#mean and sd for total prop  and copy
mean(TEORDERpropTE$PLE,na.rm=TRUE)
sd(TEORDERpropTE$PLE,na.rm=TRUE)
mean(TEORDERCopy$PLE,na.rm=TRUE)
sd(TEORDERCopy$PLE,na.rm=TRUE)
#mean and sd for cyclostome prop and copy
mean(copycyc$PLE,na.rm=TRUE)
sd(copycyc$PLE,na.rm=TRUE)
mean(propTEcyc$PLE,na.rm=TRUE)
sd(propTEcyc$PLE,na.rm=TRUE)
#mean and sd for noncyc prop and copy
mean(copynoncyc$PLE,na.rm=TRUE)
sd(copynoncyc$PLE,na.rm=TRUE)
mean(propTEnoncyc$PLE,na.rm=TRUE)
sd(propTEnoncyc$PLE,na.rm=TRUE)
###
###SINE
#boxplot of proportion of SINE to total TE content by lineage
TEORDERpropTE %>% 
  full_join(taxonomy) %>%
  ggplot(aes(x= "" ,y=SINE)) +
  geom_boxplot() +
  ylab("Proportion") +
  xlab("")+
  geom_jitter(aes(y=SINE, alpha=0.5, size=TEORDERCopy$SINE)) +
  guides(alpha= FALSE, size= FALSE) +
  labs(title = "Distribution of SINE TE Content by Lineage") +
  facet_wrap(~Lineage)
#boxplot of copy number of SINE by lineage
TEORDERCopy %>% 
  full_join(taxonomy) %>%
  ggplot(aes(x= "" ,y=SINE)) +
  geom_boxplot() +
  ylab("Copy Number") +
  xlab("")+
  geom_jitter(aes(y=SINE, alpha=0.5, size=TEORDERpropTE$SINE)) +
  guides(alpha= FALSE, size= FALSE) +
  labs(title = "Distribution of SINE Copy Number by Lineage") +
  facet_wrap(~Lineage)
##calculations
#na.rm=TRUE is needed for calculations or else R gets confused by the empty entries
#mean and sd for total prop  and copy
mean(TEORDERpropTE$SINE,na.rm=TRUE)
sd(TEORDERpropTE$SINE,na.rm=TRUE)
mean(TEORDERCopy$SINE,na.rm=TRUE)
sd(TEORDERCopy$SINE,na.rm=TRUE)
#mean and sd for cyclostome prop and copy
mean(copycyc$SINE,na.rm=TRUE)
sd(copycyc$SINE,na.rm=TRUE)
mean(propTEcyc$SINE,na.rm=TRUE)
sd(propTEcyc$SINE,na.rm=TRUE)
#mean and sd for noncyc prop and copy
mean(copynoncyc$SINE,na.rm=TRUE)
sd(copynoncyc$SINE,na.rm=TRUE)
mean(propTEnoncyc$SINE,na.rm=TRUE)
sd(propTEnoncyc$SINE,na.rm=TRUE)
###
###LINE
#boxplot of proportion of LINE to total TE content by lineage
TEORDERpropTE %>% 
  full_join(taxonomy) %>%
  ggplot(aes(x= "" ,y=LINE)) +
  geom_boxplot() +
  ylab("Proportion") +
  xlab("")+
  geom_jitter(aes(y=LINE, alpha=0.5, size=TEORDERCopy$LINE)) +
  guides(alpha= FALSE, size= FALSE) +
  labs(title = "Distribution of LINE TE Content by Lineage") +
  facet_wrap(~Lineage)
#boxplot of copy number of LINE by lineage
TEORDERCopy %>% 
  full_join(taxonomy) %>%
  ggplot(aes(x= "" ,y=LINE)) +
  geom_boxplot() +
  ylab("Copy Number") +
  xlab("")+
  geom_jitter(aes(y=LINE, alpha=0.5, size=TEORDERpropTE$LINE)) +
  guides(alpha= FALSE, size= FALSE) +
  labs(title = "Distribution of LINE Copy Number by Lineage") +
  facet_wrap(~Lineage)
##calculations
#na.rm=TRUE is needed for calculations or else R gets confused by the empty entries
#mean and sd for total prop  and copy
mean(TEORDERpropTE$LINE,na.rm=TRUE)
sd(TEORDERpropTE$LINE,na.rm=TRUE)
mean(TEORDERCopy$LINE,na.rm=TRUE)
sd(TEORDERCopy$LINE,na.rm=TRUE)
#mean and sd for cyclostome prop and copy
mean(copycyc$LINE,na.rm=TRUE)
sd(copycyc$LINE,na.rm=TRUE)
mean(propTEcyc$LINE,na.rm=TRUE)
sd(propTEcyc$LINE,na.rm=TRUE)
#mean and sd for noncyc prop and copy
mean(copynoncyc$LINE,na.rm=TRUE)
sd(copynoncyc$LINE,na.rm=TRUE)
mean(propTEnoncyc$LINE,na.rm=TRUE)
sd(propTEnoncyc$LINE,na.rm=TRUE)
############
###TIR
#boxplot of proportion of TIR to total TE content by lineage
TEORDERpropTE %>% 
  full_join(taxonomy) %>%
  ggplot(aes(x= "" ,y=TIR)) +
  geom_boxplot() +
  ylab("Proportion") +
  xlab("")+
  geom_jitter(aes(y=TIR, alpha=0.5, size=TEORDERCopy$TIR)) +
  guides(alpha= FALSE, size= FALSE) +
  labs(title = "Distribution of TIR TE Content by Lineage") +
  facet_wrap(~Lineage)
#boxplot of copy number of TIR by lineage
TEORDERCopy %>% 
  full_join(taxonomy) %>%
  ggplot(aes(x= "" ,y=TIR)) +
  geom_boxplot() +
  ylab("Copy Number") +
  xlab("")+
  geom_jitter(aes(y=TIR, alpha=0.5, size=TEORDERpropTE$TIR)) +
  guides(alpha= FALSE, size= FALSE) +
  labs(title = "Distribution of TIR Copy Number by Lineage") +
  facet_wrap(~Lineage)
##calculations
#na.rm=TRUE is needed for calculations or else R gets confused by the empty entries
#mean and sd for total prop  and copy
mean(TEORDERpropTE$TIR,na.rm=TRUE)
sd(TEORDERpropTE$TIR,na.rm=TRUE)
mean(TEORDERCopy$TIR,na.rm=TRUE)
sd(TEORDERCopy$TIR,na.rm=TRUE)
#mean and sd for cyclostome prop and copy
mean(copycyc$TIR,na.rm=TRUE)
sd(copycyc$TIR,na.rm=TRUE)
mean(propTEcyc$TIR,na.rm=TRUE)
sd(propTEcyc$TIR,na.rm=TRUE)
#mean and sd for noncyc prop and copy
mean(copynoncyc$TIR,na.rm=TRUE)
sd(copynoncyc$TIR,na.rm=TRUE)
mean(propTEnoncyc$TIR,na.rm=TRUE)
sd(propTEnoncyc$TIR,na.rm=TRUE)
#####Cryp
#boxplot of proportion of Cryp to total TE content by lineage
TEORDERpropTE %>% 
  full_join(taxonomy) %>%
  ggplot(aes(x= "" ,y=Cryptons)) +
  geom_boxplot() +
  ylab("Proportion") +
  xlab("")+
  geom_jitter(aes(y=Cryptons, alpha=0.5, size=TEORDERCopy$Cryptons)) +
  guides(alpha= FALSE, size= FALSE) +
  labs(title = "Distribution of Crypton TE Content by Lineage") +
  facet_wrap(~Lineage)
#boxplot of copy number of Cryptons by lineage
TEORDERCopy %>% 
  full_join(taxonomy) %>%
  ggplot(aes(x= "" ,y=Cryptons)) +
  geom_boxplot() +
  ylab("Copy Number") +
  xlab("")+
  geom_jitter(aes(y=Cryptons, alpha=0.5, size=TEORDERpropTE$Cryptons)) +
  guides(alpha= FALSE, size= FALSE) +
  labs(title = "Distribution of Crypton Copy Number by Lineage") +
  facet_wrap(~Lineage)
##calculations
#na.rm=TRUE is needed for calculations or else R gets confused by the empty entries
#mean and sd for total prop  and copy
mean(TEORDERpropTE$Cryptons,na.rm=TRUE)
sd(TEORDERpropTE$Cryptons,na.rm=TRUE)
mean(TEORDERCopy$Cryptons,na.rm=TRUE)
sd(TEORDERCopy$Cryptons,na.rm=TRUE)
#mean and sd for cyclostome prop and copy
mean(copycyc$Cryptons,na.rm=TRUE)
sd(copycyc$Cryptons,na.rm=TRUE)
mean(propTEcyc$Cryptons,na.rm=TRUE)
sd(propTEcyc$Cryptons,na.rm=TRUE)
#mean and sd for noncyc prop and copy
mean(copynoncyc$Cryptons,na.rm=TRUE)
sd(copynoncyc$Cryptons,na.rm=TRUE)
mean(propTEnoncyc$Cryptons,na.rm=TRUE)
sd(propTEnoncyc$Cryptons,na.rm=TRUE)
####Hel
#boxplot of proportion of Hel to total TE content by lineage
TEORDERpropTE %>% 
  full_join(taxonomy) %>%
  ggplot(aes(x= "" ,y=Helitrons)) +
  geom_boxplot() +
  ylab("Proportion") +
  xlab("")+
  geom_jitter(aes(y=Helitrons, alpha=0.5, size=TEORDERCopy$Helitrons)) +
  guides(alpha= FALSE, size= FALSE) +
  labs(title = "Distribution of Helitron TE Content by Lineage") +
  facet_wrap(~Lineage)
#boxplot of copy number of Helitrons by lineage
TEORDERCopy %>% 
  full_join(taxonomy) %>%
  ggplot(aes(x= "" ,y=Helitrons)) +
  geom_boxplot() +
  ylab("Copy Number") +
  xlab("")+
  geom_jitter(aes(y=Helitrons, alpha=0.5, size=TEORDERpropTE$Helitrons)) +
  guides(alpha= FALSE, size= FALSE) +
  labs(title = "Distribution of Helitron Copy Number by Lineage") +
  facet_wrap(~Lineage)
##calculations
#na.rm=TRUE is needed for calculations or else R gets confused by the empty entries
#mean and sd for total prop  and copy
mean(TEORDERpropTE$Helitrons,na.rm=TRUE)
sd(TEORDERpropTE$Helitrons,na.rm=TRUE)
mean(TEORDERCopy$Helitrons,na.rm=TRUE)
sd(TEORDERCopy$Helitrons,na.rm=TRUE)
#mean and sd for cyclostome prop and copy
mean(copycyc$Helitrons,na.rm=TRUE)
sd(copycyc$Helitrons,na.rm=TRUE)
mean(propTEcyc$Helitrons,na.rm=TRUE)
sd(propTEcyc$Helitrons,na.rm=TRUE)
#mean and sd for noncyc prop and copy
mean(copynoncyc$Helitrons,na.rm=TRUE)
sd(copynoncyc$Helitrons,na.rm=TRUE)
mean(propTEnoncyc$Helitrons,na.rm=TRUE)
sd(propTEnoncyc$Helitrons,na.rm=TRUE)
#######Mav
#boxplot of proportion of Mav to total TE content by lineage
TEORDERpropTE %>% 
  full_join(taxonomy) %>%
  ggplot(aes(x= "" ,y=Mavericks)) +
  geom_boxplot() +
  ylab("Proportion") +
  xlab("")+
  geom_jitter(aes(y=Mavericks, alpha=0.5, size=TEORDERCopy$Mavericks)) +
  guides(alpha= FALSE, size= FALSE) +
  labs(title = "Distribution of Maverick TE Content by Lineage") +
  facet_wrap(~Lineage)
#boxplot of copy number of Mavericks by lineage
TEORDERCopy %>% 
  full_join(taxonomy) %>%
  ggplot(aes(x= "" ,y=Mavericks)) +
  geom_boxplot() +
  ylab("Copy Number") +
  xlab("")+
  geom_jitter(aes(y=Mavericks, alpha=0.5, size=TEORDERpropTE$Mavericks)) +
  guides(alpha= FALSE, size= FALSE) +
  labs(title = "Distribution of Maverick Copy Number by Lineage") +
  facet_wrap(~Lineage)
##calculations
#na.rm=TRUE is needed for calculations or else R gets confused by the empty entries
#mean and sd for total prop  and copy
mean(TEORDERpropTE$Mavericks,na.rm=TRUE)
sd(TEORDERpropTE$Mavericks,na.rm=TRUE)
mean(TEORDERCopy$Mavericks,na.rm=TRUE)
sd(TEORDERCopy$Mavericks,na.rm=TRUE)
#mean and sd for cyclostome prop and copy
mean(copycyc$Mavericks,na.rm=TRUE)
sd(copycyc$Mavericks,na.rm=TRUE)
mean(propTEcyc$Mavericks,na.rm=TRUE)
sd(propTEcyc$Mavericks,na.rm=TRUE)
#mean and sd for noncyc prop and copy
mean(copynoncyc$Mavericks,na.rm=TRUE)
sd(copynoncyc$Mavericks,na.rm=TRUE)
mean(propTEnoncyc$Mavericks,na.rm=TRUE)
sd(propTEnoncyc$Mavericks,na.rm=TRUE)
##############################
#10 stacked bar of CLASSIFIED TE content

TEORDERpropTE<- TEORDERpropTE %>%
  rowwise() %>%
  mutate(TotalClassified = sum(LTR,DIRS,PLE,SINE,LINE,TIR,Helitrons,Mavericks,Cryptons,na.rm=TRUE))

summary2<- summary  %>%
  select(scientific_name, GenomeLength, TEcontent, Lineage)%>%
  distinct(scientific_name, .keep_all = TRUE)
mean(summary2$TEcontent)
max(summary2$TEcontent)-min(summary2$TEcontent)
cyclostome <- summary2 %>% filter(Lineage=="Cyclostome")
mean(cyclostome$GenomeLength)
max(cyclostome$GenomeLength)-min(cyclostome$GenomeLength)
mean(cyclostome$TEcontent)
max(cyclostome$TEcontent)-min(cyclostome$TEcontent)
noncyc<- summary2%>% filter(Lineage == "Non-Cyclostome")
mean(noncyc$GenomeLength)
max(noncyc$GenomeLength)-min(noncyc$GenomeLength)
mean(noncyc$TEcontent)
max(noncyc$TEcontent)-min(cyclostome$TEcontent)

taxonomy %>% 
  ggplot(aes(x= "" ,y=GenomeLength)) +
  geom_boxplot() +
  ylab("GenomeLength") +
  xlab("")+
  geom_jitter(aes(y=GenomeLength, alpha=0.5)) +
  guides(alpha= FALSE) +
  labs(title = "Distribution of Genome Length by Lineage") +
  facet_wrap(~Lineage)

taxonomy %>%
  mutate(logGL = log10(GenomeLength)) %>%
  ggplot(aes(x= "" ,y=logGL)) +
  geom_boxplot() +
  ylab("GenomeLength") +
  xlab("")+
  geom_jitter(aes(y=logGL, alpha=0.5)) +
  guides(alpha= FALSE) +
  labs(title = "Distribution of Genome Length by Lineage") +
  facet_wrap(~Lineage)



summary2 %>%
  ggplot(aes(x= "" ,y=TEcontent)) +
  geom_boxplot() +
  ylab("TEcontent") +
  xlab("")+
  geom_jitter(aes(y=TEcontent, alpha=0.5)) +
  guides(alpha= FALSE) +
  labs(title = "Distribution of TEcontent by Lineage") +
  facet_wrap(~Lineage)

############################
#11. plotting GL and doing models
#the ggplot way
summary %>% ggplot(aes(x=logGL, y=logTE)) +
  geom_point(aes(size=1)) +
  labs(x= "Log Genome Length", y= "Log Total TE length") +
  geom_smooth(method=lm, col="darkblue") +
  guides(size= FALSE) +
  labs(title = "Relationship betwen Genome and total TE length")

#visualising data for bayesian
par( mfrow=c(1,1))
plot(summary$logGL, summary$logTE, main="Relationship between Braconid TE length and Genome Length (n=33)",xlab="log(Genome length)",
     ylab="log(total TE length)",type = "n")
with(subset(summary, Lineage == "Cyclostome"), points(logGL, logTE, col = "blue",pch=16,cex=1))
with(subset(summary, Lineage == "Non-Cyclostome"), points(logGL, logTE, col = "red",pch=16,cex=1))
legend("topleft", pch = 16, col = c("blue", "red"), legend = c("Cyclostome", "Non-Cyclostome"))

#basic linear model
summary$cyc <- 0
summary$cyc[summary$Lineage=="Non-Cyclostome"] <-1
tesimple<- summary%>%
  select(logGL,logTE,cyc) %>% distinct(logGL, .keep_all = TRUE) 
#can you predict TE from GL? (data is log transformed)
#log linear model
model1 <- ulam(
  alist(
    logTE ~ dnorm(mu,sigma),
    mu <- a  + b*logGL,
    a ~ dnorm(0,100),
    #a= intercept of total  
    b ~ dnorm(0,100),
    #b= slope of total
    sigma ~ dcauchy(0,1)
    #the variance which i am assuming is constant
  ),
  data = tesimple,
  iter=4000,warmup=1000,chains =3,cores=3, log_lik = TRUE
)
precis(model1,digits=1)
par( mfrow=c(1,1))
plot(model1,pars=c("a","b","sigma"), depth=2)
pairs(model1,horInd=c(1:3),verInd=c(1:3))
post <- extract.samples(model1)
par(mfrow=c(3,3))
for(i in names(post)){
  dfpost <- data.frame(post[i])
  plot(as.numeric(rownames(dfpost)), dfpost[,1], type = "l",
       xlab = "Index",
       ylab = i)
}

#ok now second model
#can you predict TE from GL by lineage? (data is log transformed)
model2 <- ulam(
  alist(
    logTE ~ dnorm(mu,sigma),
    mu <- a  + b*logGL + c*cyc + d*cyc*logGL,
    a ~ dnorm(0,100),
    #a= intercept of Cyclostome   
    b ~ dnorm(0,100),
    #b= slope of Cyclostome
    c ~ dnorm(0,100),
    #c= intercept of NonCyclostome
    d ~ dnorm(0,100),
    #d= difference between Cyclostome and NonCyclostome
    sigma ~ dcauchy(0,1)
    #the variance which i am assuming is constant
  ),
  data = tesimple,
  iter=4000,warmup=1000,chains =3,cores=3,log_lik = TRUE
)

precis(model2,digits=1)
par( mfrow=c(1,1))
plot(model2,pars=c("a","b","c","d","sigma"), depth=2)
pairs(model2,horInd=c(1:5),verInd=c(1:5))
post2 <- extract.samples(model2)
par(mfrow=c(3,3))
for(i in names(post2)){
  dfpost2 <- data.frame(post[i])
  plot(as.numeric(rownames(dfpost2)), dfpost2[,1], type = "l",
       xlab = "Index",
       ylab = i)
}
compare(model1,model2)


seq_length <- seq(8, 8.6, 0.1)
Zeros <- c(rep(0,times = length(seq_length)))
Ones <- c(rep(1,times = length(seq_length)))
#have to create link matrix (this is where messed up last time)
Cycl <- link(model2, data=data.frame(logGL=seq_length, cyc=Zeros))
NonCyc <- link(model2, data=data.frame(logGL=seq_length, cyc=Ones))
Total <- link(model1, data=data.frame(logGL=seq_length))
#calculation of mean
Cyc_Mean <- apply(Cycl,2,mean)
Noncyc_Mean <- apply(NonCyc,2,mean)
Total_Mean<- apply(Total, 2, mean)
#calculation of CI
Cycl_Mean2 <- apply(Cycl,2, PI, prob=0.95)
NonCyc_Mean2 <- apply(NonCyc,2, PI, prob=0.95)
Total_Mean2<- apply(Total, 2, PI, prob=0.95)

#plot
t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}

mycol <- t_col("gray", perc = 50, name = "lt.gray")
par(mfrow=c(1,2))
plot(summary$logGL, summary$logTE, main="",xlab="log(Genome length)",
     ylab="log(total TE length)",type = "n")
with(subset(summary, Lineage == "Cyclostome"), points(logGL, logTE, col = "blue",pch=16,cex=1))
with(subset(summary, Lineage == "Non-Cyclostome"), points(logGL, logTE, col = "red",pch=16,cex=1))
lines(seq_length, Total_Mean, col="black")
shade(Total_Mean2,seq_length, col=mycol)
plot(tesimple$logTE ~ tesimple$logGL,xlab="log(Genome length)",
     ylab="log(total TE length)")
lines(seq_length, Cyc_Mean, col = "blue")
shade(Cycl_Mean2, seq_length, col = "#0000FF80")
lines(seq_length, Noncyc_Mean, col = "red")
shade(NonCyc_Mean2, seq_length, col = "#FF000080")

###

#11. phylogenetic signal
#read in the newick style tree
tree<- read.newick(file = "tree32nooutliers.treefile")
#read in the data
taxa<- read.csv("C:/Users/Rachel/Desktop/Rfiles2024/pageltaxnooutliers.csv") %>% 
  filter(scientific_name!= "Dinocampuscoccinellae1") 

##for genome length
#creating a dataframe to link everything together and make a phylo4d(need for phylosignal)
#make empty dataframe 
GLP <- list()
#make new column called mass and pull out the vector of characters from the data file
GLP$GL <- taxa$GenomeLength
#a column of random (for comparison)
GLP$random <- rnorm(32, sd = 10)
#a column of brownian motion-simulated values (for comparison)
#traits evolve randomly BUT at a fixed rate
GLP$bm <- rTraitCont(tree)
#convert to data frame
GLP <- as.data.frame(GLP)
#convert dataframe into a phylo4d object
bracp4d2 <- phylo4d(tree, GLP)
#plot data
barplot.phylo4d(bracp4d2, tree.type = "phylo", tree.ladderize = TRUE)
#calculate phylogenetic signal
phyloSignal(bracp4d2, method = "all")
#method=all means show me mean, morans index(I), Pagels lambda, and Blombergs K/K* 

## for GC content
#creating a dataframe to link everything together and make a phylo4d(need for phylosignal)
#make empty dataframe 
GCP <- list()
#make new column called mass and pull out the vector of characters from the data file
GCP$GC <- taxa$Genome_GC
#a column of random (for comparison)
GCP$random <- rnorm(32, sd = 10)
#a column of brownian motion-simulated values (for comparison)
#traits evolve randomly BUT at a fixed rate
GCP$bm <- rTraitCont(tree)
#convert to data frame
GCP <- as.data.frame(GCP)
#convert dataframe into a phylo4d object
GCPp4d2 <- phylo4d(tree, GCP)
#plot data
barplot.phylo4d(GCPp4d2, tree.type = "phylo", tree.ladderize = TRUE)
#calculate phylogenetic signal
phyloSignal(GCPp4d2, method = "all")
#method=all means show me mean, morans index(I), Pagels lambda, and Blombergs K/K* 

##TEPropGenome
TEP <- list()
TEP$TE <- taxa$TEPropGenome
TEP$random <- rnorm(32, sd = 10)
TEP$bm <- rTraitCont(tree)
TEP <- as.data.frame(TEP)
TEPp4d2 <- phylo4d(tree, TEP)
barplot.phylo4d(TEPp4d2, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(TEPp4d2, method = "all")

##Total Copy Number
CN <- list()
CN$TE <- taxa$TECopies
CN$random <- rnorm(32, sd = 10)
CN$bm <- rTraitCont(tree)
CN <- as.data.frame(CN)
CNp4d2 <- phylo4d(tree, CN)
barplot.phylo4d(CNp4d2, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(CNp4d2, method = "all")

#total TE length
TL <- list()
TL$TL <- taxa$TELength
TL$random <- rnorm(32, sd = 10)
TL$bm <- rTraitCont(tree)
TL <- as.data.frame(TL)
TLp4d2 <- phylo4d(tree, TL)
barplot.phylo4d(TLp4d2, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(TLp4d2, method = "all")

##HelPropGenome
HPG <- list()
HPG$HPG <- taxa$HelPropGenome
HPG$random <- rnorm(32, sd = 10)
HPG$bm <- rTraitCont(tree)
HPG <- as.data.frame(HPG)
HPGp4d2 <- phylo4d(tree, HPG)
barplot.phylo4d(HPGp4d2, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(HPGp4d2, method = "all")

##TIRPropGenome
TPG <- list()
TPG$TPG <- taxa$TIRPropGenome
TPG$random <- rnorm(32, sd = 10)
TPG$bm <- rTraitCont(tree)
TPG <- as.data.frame(TPG)
TPGp4d2 <- phylo4d(tree, TPG)
barplot.phylo4d(TPGp4d2, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(TPGp4d2, method = "all")

##MavericksPropGenome

MPG <- list()
MPG$MPG <- taxa$MavericksPropGenome
MPG$random <- rnorm(32, sd = 10)
MPG$bm <- rTraitCont(tree)
MPG <- as.data.frame(MPG)
MPGp4d2 <- phylo4d(tree, MPG)
barplot.phylo4d(MPGp4d2, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(MPGp4d2, method = "all")

##LINEPropGenome
LPG <- list()
LPG$LPG <- taxa$LINEPropGenome
LPG$random <- rnorm(32, sd = 10)
LPG$bm <- rTraitCont(tree)
LPG <- as.data.frame(LPG)
LPGp4d2 <- phylo4d(tree, LPG)
barplot.phylo4d(LPGp4d2, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(LPGp4d2, method = "all")

##PLEPropGenome
PPG <- list()
PPG$PPG <- taxa$PLEPropGenome
PPG$random <- rnorm(32, sd = 10)
PPG$bm <- rTraitCont(tree)
PPG <- as.data.frame(PPG)
PPGp4d2 <- phylo4d(tree, PPG)
barplot.phylo4d(PPGp4d2, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(PPGp4d2, method = "all")

##SINEPropGenome
SPG <- list()
SPG$SPG <- taxa$SINEPropGenome
SPG$random <- rnorm(32, sd = 10)
SPG$bm <- rTraitCont(tree)
SPG <- as.data.frame(SPG)
SPGp4d2 <- phylo4d(tree, SPG)
barplot.phylo4d(SPGp4d2, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(SPGp4d2, method = "all")

##LTRPropGenome
LTRPG <- list()
LTRPG$LTRPG <- taxa$LTRPropGenome
LTRPG$random <- rnorm(32, sd = 10)
LTRPG$bm <- rTraitCont(tree)
LTRPG <- as.data.frame(LTRPG)
LTRPGp4d2 <- phylo4d(tree, LTRPG)
barplot.phylo4d(LTRPGp4d2, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(LTRPGp4d2, method = "all")

##DIRSPropGenome
DPG <- list()
DPG$DPG <- taxa$DIRSPropGenome
DPG$random <- rnorm(32, sd = 10)
DPG$bm <- rTraitCont(tree)
DPG <- as.data.frame(DPG)
DPGp4d2 <- phylo4d(tree, DPG)
barplot.phylo4d(DPGp4d2, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(DPGp4d2, method = "all")

##CryptonsPropGenome
CPG <- list()
CPG$CPG <- taxa$CryptonsPropGenome
CPG$random <- rnorm(32, sd = 10)
CPG$bm <- rTraitCont(tree)
CPG <- as.data.frame(CPG)
CPGp4d2 <- phylo4d(tree, CPG)
barplot.phylo4d(CPGp4d2, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(CPGp4d2, method = "all")

##UnknownPropGenome
UPG <- list()
UPG$UPG <- taxa$UnknownPropGenome
UPG$random <- rnorm(32, sd = 10)
UPG$bm <- rTraitCont(tree)
UPG <- as.data.frame(UPG)
UPGp4d2 <- phylo4d(tree, UPG)
barplot.phylo4d(UPGp4d2, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(UPGp4d2, method = "all")

##Maverick Copy #
MCN <- list()
MCN$MCN <- taxa$MavCopy
MCN$random <- rnorm(32, sd = 10)
MCN$bm <- rTraitCont(tree)
MCN <- as.data.frame(MCN)
MCNp4d2 <- phylo4d(tree, MCN)
barplot.phylo4d(MCNp4d2, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(MCNp4d2, method = "all")

#HelCopy
HCN <- list()
HCN$HCN <- taxa$HelCopy
HCN$random <- rnorm(32, sd = 10)
HCN$bm <- rTraitCont(tree)
HCN <- as.data.frame(HCN)
HCNp4d2 <- phylo4d(tree, HCN)
barplot.phylo4d(HCNp4d2, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(HCNp4d2, method = "all")

##TIRCopy
TCN <- list()
TCN$TCN <- taxa$TIRCopy
TCN$random <- rnorm(32, sd = 10)
TCN$bm <- rTraitCont(tree)
TCN <- as.data.frame(TCN)
TCNp4d2 <- phylo4d(tree, TCN)
barplot.phylo4d(TCNp4d2, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(TCNp4d2, method = "all")

##CryptonsCopy
CCN <- list()
CCN$CCN <- taxa$CrypCopy
CCN$random <- rnorm(32, sd = 10)
CCN$bm <- rTraitCont(tree)
CCN <- as.data.frame(CCN)
CCNp4d2 <- phylo4d(tree, CCN)
barplot.phylo4d(CCNp4d2, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(CCNp4d2, method = "all")

##LINECopy
LCN <- list()
LCN$LCN <- taxa$LINECopy
LCN$random <- rnorm(32, sd = 10)
LCN$bm <- rTraitCont(tree)
LCN <- as.data.frame(LCN)
LCNp4d2 <- phylo4d(tree, LCN)
barplot.phylo4d(LCNp4d2, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(LCNp4d2, method = "all")

##PLE copy
PCN <- list()
PCN$PCN <- taxa$PLECopy
PCN$random <- rnorm(32, sd = 10)
PCN$bm <- rTraitCont(tree)
PCN <- as.data.frame(PCN)
PCNp4d2 <- phylo4d(tree, PCN)
barplot.phylo4d(PCNp4d2, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(PCNp4d2, method = "all")

##LTRCopy
LTRCN <- list()
LTRCN$LTRCN <- taxa$LTRCopy
LTRCN$random <- rnorm(32, sd = 10)
LTRCN$bm <- rTraitCont(tree)
LTRCN <- as.data.frame(LTRCN)
LTRCNp4d2 <- phylo4d(tree, LTRCN)
barplot.phylo4d(LTRCNp4d2, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(LTRCNp4d2, method = "all")

##DIRSCopy
DCN <- list()
DCN$DCN <- taxa$DIRSCopy
DCN$random <- rnorm(32, sd = 10)
DCN$bm <- rTraitCont(tree)
DCN <- as.data.frame(DCN)
DCNp4d2 <- phylo4d(tree, DCN)
barplot.phylo4d(DCNp4d2, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(DCNp4d2, method = "all")

##SINECopy
SCN <- list()
SCN$SCN <- taxa$SINECopy
SCN$random <- rnorm(32, sd = 10)
SCN$bm <- rTraitCont(tree)
SCN <- as.data.frame(SCN)
SCNp4d2 <- phylo4d(tree, SCN)
barplot.phylo4d(SCNp4d2, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(SCNp4d2, method = "all")

#####TELength= OrderSum

TEORDERLength <- summary %>%
  select(scientific_name, TEOrder, OrderSum)%>%
  pivot_wider(names_from = TEOrder, values_from = OrderSum)

mean(TEORDERLength$LTR,na.rm=TRUE)
sd(TEORDERLength$LTR,na.rm=TRUE)

mean(TEORDERLength$DIRS,na.rm=TRUE)
sd(TEORDERLength$DIRS,na.rm=TRUE)

mean(TEORDERLength$PLE,na.rm=TRUE)
sd(TEORDERLength$PLE,na.rm=TRUE)

mean(TEORDERLength$SINE,na.rm=TRUE)
sd(TEORDERLength$SINE,na.rm=TRUE)

mean(TEORDERLength$LINE,na.rm=TRUE)
sd(TEORDERLength$LINE,na.rm=TRUE)

####giant supplementary data table 
A<- TEORDERCopy %>% 
  select(scientific_name,LTR,DIRS,PLE,SINE,LINE,TIR,Cryptons,Mavericks,Helitrons,Unknown) %>% 
  rename(LTRCopy = LTR,DIRSCopy = DIRS, PLECopy = PLE, SINECopy = SINE, LINECopy= LINE, TIRCopy = TIR, CrypCopy = Cryptons, MavCopy = Mavericks, HelCopy = Helitrons, UnclasCopy= Unknown)

B<- TEORDERLength %>% 
  select(LTR,DIRS,PLE,SINE,LINE,TIR,Cryptons,Mavericks,Helitrons,Unknown) %>% 
  rename(LTRLength = LTR,DIRSLength = DIRS, PLELength = PLE, SINELength = SINE, LINELength= LINE,TIRLength = TIR, CrypLength = Cryptons, MavLength = Mavericks, HelLength = Helitrons, UnclasLength= Unknown)

C<-TEORDERpropGenome %>%
  select(LTR,DIRS,PLE,SINE,LINE,TIR,Cryptons,Mavericks,Helitrons,Unknown) %>% 
  rename(LTRpropGenome = LTR,DIRSpropGenome = DIRS, PLEpropGenome = PLE, SINEpropGenome = SINE, LINEpropGenome= LINE, TIRpropGenome = TIR, CryppropGenome = Cryptons, MavpropGenome = Mavericks, HelpropGenome = Helitrons, UnclaspropGenome= Unknown)

D<-TEORDERpropTE %>% 
  select(LTR,DIRS,PLE,SINE,LINE,TIR,Cryptons,Mavericks,Helitrons,Unknown) %>% 
  rename(LTRpropTE = LTR,DIRSpropTE = DIRS, PLEpropTE = PLE, SINEpropTE = SINE, LINEpropTE= LINE,TIRpropTE = TIR, CryppropTE = Cryptons, MavpropTE = Mavericks, HelpropTE = Helitrons, UnclaspropTE= Unknown)

DataTable<- cbind(A,B,C,D) %>% mutate(Name = taxonomy$Species) %>% select(-scientific_name) %>% relocate(Name)

write_xlsx(DataTable, path = "Supplementary3.xlsx")

sd(taxonomy$GenomeLength)
sd(copycyc$GenomeLength)
sd(copynoncyc$GenomeLength)

copynoncyc$logGL<- log10(copynoncyc$GenomeLength)
par(mfrow=c(1,2))
boxplot(copynoncyc$GenomeLength)
boxplot(copycyc$GenomeLength)
#looks not normal both lets double check
shapiro.test(copycyc$GenomeLength)
shapiro.test(copynoncyc$GenomeLength)
#ya not normal 
copycyc$logGL<-log10(copycyc$GenomeLength)
par(mfrow=c(1,2))
boxplot(copynoncyc$logGL)
boxplot(copycyc$logGL)
#double check now that log transformed
shapiro.test(copycyc$logGL)
shapiro.test(copynoncyc$logGL)
#cyc is normal but noncyc not normal

wilcox.test(copycyc$GenomeLength, copynoncyc$GenomeLength, paired=FALSE)
#no diff
wilcox.test(copynoncyc$GenomeLength, copycyc$GenomeLength, paired=FALSE)
#no diff and lower w stat


TElength <- summary%>% select(scientific_name, Lineage,sumTElength, GenomeLength) %>% distinct(scientific_name, .keep_all = TRUE)
  
#correlation between genome and TE length
#default is pearson but wont work since not normal dist
#cor(TElength$GenomeLength, TElength$sumTElength)
cyclength<- TElength%>% filter(Lineage=="Cyclostome")
noncyclength <- TElength %>% filter(Lineage=="Non-Cyclostome")
shapiro.test(cyclength$sumTElength)
#notnormal
shapiro.test(noncyclength$sumTElength)
#not normal

cor.test(TElength$GenomeLength, TElength$sumTElength, method="spearman")
cor.test(TElength$sumTElength,TElength$GenomeLength, method="spearman")
######how correlated is GL and TEprop in cyc
cor.test(cyclength$GenomeLength, cyclength$sumTElength, method="spearman")
###########how correlated is GL and TEprop in noncyc
cor.test(noncyclength$GenomeLength, noncyclength$sumTElength, method="spearman")
#######################
mean(TElength$sumTElength)
sd(TElength$sumTElength)
mean(cyclength$sumTElength)
sd(cyclength$sumTElength)
mean(noncyclength$sumTElength)
sd(noncyclength$sumTElength)
wilcox.test(cyclength$sumTElength, noncyclength$sumTElength, paired=FALSE)
wilcox.test(noncyclength$sumTElength, cyclength$sumTElength, paired=FALSE)

shapiro.test(propTEcyc$LTR)
shapiro.test(propTEnoncyc$LTR)
#no for cyc yes for noncyc
wilcox.test(propTEcyc$LTR, propTEnoncyc$LTR, paired=FALSE)
wilcox.test(propTEnoncyc$LTR, propTEcyc$LTR, paired=FALSE)
#not diff
shapiro.test(copycyc$LTR)
shapiro.test(copynoncyc$LTR)
#no for either
wilcox.test(copycyc$LTR, copynoncyc$LTR, paired=FALSE)
wilcox.test(copynoncyc$LTR, copycyc$LTR, paired=FALSE)
#yes diff

shapiro.test(propTEcyc$DIRS)
shapiro.test(propTEnoncyc$DIRS)
#no for both
wilcox.test(propTEcyc$DIRS, propTEnoncyc$DIRS, paired=FALSE)
wilcox.test(propTEnoncyc$DIRS, propTEcyc$DIRS, paired=FALSE)
shapiro.test(copycyc$DIRS)
shapiro.test(copynoncyc$DIRS)
#no for either
wilcox.test(copycyc$DIRS, copynoncyc$DIRS, paired=FALSE)
wilcox.test(copynoncyc$DIRS, copycyc$DIRS, paired=FALSE)


shapiro.test(propTEcyc$PLE)
shapiro.test(propTEnoncyc$PLE)
#no 
wilcox.test(propTEcyc$PLE, propTEnoncyc$PLE, paired=FALSE)
wilcox.test(propTEnoncyc$PLE, propTEcyc$PLE, paired=FALSE)
#yes diff
shapiro.test(copycyc$PLE)
shapiro.test(copynoncyc$PLE)
#no for either
wilcox.test(copycyc$PLE, copynoncyc$PLE, paired=FALSE)
wilcox.test(copynoncyc$PLE, copycyc$PLE, paired=FALSE)

shapiro.test(propTEcyc$SINE)
shapiro.test(propTEnoncyc$SINE)
wilcox.test(propTEcyc$SINE, propTEnoncyc$SINE, paired=FALSE)
wilcox.test(propTEnoncyc$SINE, propTEcyc$SINE, paired=FALSE)
shapiro.test(copycyc$SINE)
shapiro.test(copynoncyc$SINE)
wilcox.test(copycyc$SINE, copynoncyc$SINE, paired=FALSE)
wilcox.test(copynoncyc$SINE, copycyc$SINE, paired=FALSE)

shapiro.test(propTEcyc$LINE)
shapiro.test(propTEnoncyc$LINE)
wilcox.test(propTEcyc$LINE, propTEnoncyc$LINE, paired=FALSE)
wilcox.test(propTEnoncyc$LINE, propTEcyc$LINE, paired=FALSE)
shapiro.test(copycyc$LINE)
shapiro.test(copynoncyc$LINE)
wilcox.test(copycyc$LINE, copynoncyc$LINE, paired=FALSE)
wilcox.test(copynoncyc$LINE, copycyc$LINE, paired=FALSE)
####

shapiro.test(propTEcyc$TIR)
shapiro.test(propTEnoncyc$TIR)
#both normal suprisingly so ttest instead of MWU
#checkvar
var.test(propTEcyc$TIR, propTEnoncyc$TIR)
#not equal var
t.test(propTEcyc$TIR, propTEnoncyc$TIR, 
       alternative ="two.sided",
       paired= FALSE,
       var.equal = FALSE)
#yes different
shapiro.test(copycyc$TIR)
shapiro.test(copynoncyc$TIR)
wilcox.test(copycyc$TIR, copynoncyc$TIR, paired=FALSE)
wilcox.test(copynoncyc$TIR, copycyc$TIR, paired=FALSE)

shapiro.test(propTEcyc$Cryptons)
shapiro.test(propTEnoncyc$Cryptons)
wilcox.test(propTEcyc$Cryptons, propTEnoncyc$Cryptons, paired=FALSE)
wilcox.test(propTEnoncyc$Cryptons, propTEcyc$Cryptons, paired=FALSE)
shapiro.test(copycyc$Cryptons)
shapiro.test(copynoncyc$Cryptons)
wilcox.test(copycyc$Cryptons, copynoncyc$Cryptons, paired=FALSE)
wilcox.test(copynoncyc$Cryptons, copycyc$Cryptons, paired=FALSE)


shapiro.test(propTEcyc$Helitrons)
shapiro.test(propTEnoncyc$Helitrons)
var.test(propTEcyc$Helitrons, propTEnoncyc$Helitrons)
#var equal
t.test(propTEcyc$Helitrons, propTEnoncyc$Helitrons, 
       alternative ="two.sided",
       paired= FALSE,
       var.equal = TRUE)
shapiro.test(copycyc$Helitrons)
shapiro.test(copynoncyc$Helitrons)
wilcox.test(copycyc$Helitrons, copynoncyc$Helitrons, paired=FALSE)
wilcox.test(copynoncyc$Helitrons, copycyc$Helitrons, paired=FALSE)

shapiro.test(propTEcyc$Mavericks)
shapiro.test(propTEnoncyc$Mavericks)
wilcox.test(propTEcyc$Mavericks, propTEnoncyc$Mavericks, paired=FALSE)
wilcox.test(propTEnoncyc$Mavericks, propTEcyc$Mavericks, paired=FALSE)
shapiro.test(copycyc$Mavericks)
shapiro.test(copynoncyc$Mavericks)
wilcox.test(copycyc$Mavericks, copynoncyc$Mavericks, paired=FALSE)
wilcox.test(copynoncyc$Mavericks, copycyc$Mavericks, paired=FALSE)

########################################3
plot(summary$OrderSum, summary$OrderCopies)
#cor.test(summary$OrderSum, summary$OrderCopies)
#since not normal change to spearman (pearson good for normal dist)
cor.test(summary$OrderSum, summary$OrderCopies, method="spearman")
#for all TE's yes very strong pos correlation
TEORDERLength <- summary %>% 
  full_join(taxonomy) %>%
  select(scientific_name, TEOrder, OrderSum)%>%
  pivot_wider(names_from = TEOrder, values_from = OrderSum)
TEORDERLength[is.na(TEORDERLength)] <- 0
TEORDERCopy[is.na(TEORDERCopy)] <- 0
TEORDERpropGenome[is.na(TEORDERpropGenome)] <- 0
TEORDERpropGenome <- TEORDERpropGenome%>% 
  full_join(taxonomy) %>%
  select(scientific_name, LTR, DIRS, PLE, SINE, LINE, TIR, Helitrons, Mavericks, Cryptons, Lineage) %>%
  mutate(RetropropGenome = LTR+DIRS+PLE+SINE+LINE,
         DNApropGenome = TIR+Helitrons+Mavericks+Cryptons)

#includes unknowns
cor.test(summary$propTElength, summary$OrderCopies, method="spearman")
cor.test(summary$propGenlength, summary$OrderCopies, method="spearman")
cor.test(summary$propGenlength, summary$propTElength, method="spearman")
cor.test(summary$propGenlength, summary$OrderSum, method="spearman")
cor.test(summary$propTElength, summary$OrderSum, method="spearman")
cor.test(summary$OrderSum, summary$OrderCopies, method="spearman")
############Retro
plot(TEORDERLength$LTR, TEORDERCopy$LTR)
cor.test(TEORDERLength$LTR, TEORDERCopy$LTR, method="spearman")
plot(TEORDERLength$DIRS, TEORDERCopy$DIRS)
cor.test(TEORDERLength$DIRS, TEORDERCopy$DIRS, method="spearman")
plot(TEORDERLength$PLE, TEORDERCopy$PLE)
cor.test(TEORDERLength$PLE, TEORDERCopy$PLE, method="spearman")
plot(TEORDERLength$SINE, TEORDERCopy$SINE)
cor.test(TEORDERLength$SINE, TEORDERCopy$SINE, method="spearman")
plot(TEORDERLength$LINE, TEORDERCopy$LINE)
cor.test(TEORDERLength$LINE, TEORDERCopy$LINE, method="spearman")
RETROLen<- TEORDERLength %>% 
  select(scientific_name, LTR, DIRS, PLE, SINE, LINE) %>%
  mutate(RetroLength = LTR+DIRS+PLE+SINE+LINE)
RETROCopy <- TEORDERCopy %>%   
  select(scientific_name, LTR, DIRS, PLE, SINE, LINE, Lineage) %>%
  mutate(RetroCopy = LTR+DIRS+PLE+SINE+LINE)
plot(RETROLen$RetroLength, RETROCopy$RetroCopy)
cor.test(RETROLen$RetroLength, RETROCopy$RetroCopy, method="spearman")
TEORDERpropTE[is.na(TEORDERpropTE)] <- 0
RETROpropTE <- TEORDERpropTE %>% 
  select(scientific_name, LTR, DIRS, PLE, SINE, LINE, Lineage) %>%
  mutate(RetropropTE = LTR+DIRS+PLE+SINE+LINE)
plot(RETROpropTE$RetropropTE, RETROCopy$RetroCopy)
cor.test(RETROpropTE$RetropropTE, RETROCopy$RetroCopy, method="spearman")


##############DNA
DNALen<- TEORDERLength %>% 
  select(scientific_name, TIR, Helitrons, Mavericks, Cryptons, Lineage) %>%
  mutate(DNALength = TIR+Helitrons+Mavericks+Cryptons)
DNACopy <- TEORDERCopy %>%   
  select(scientific_name, TIR, Helitrons, Mavericks, Cryptons, Lineage) %>%
  mutate(DNACopy = TIR+Helitrons+Mavericks+Cryptons)
DNApropTE <- TEORDERpropTE %>% 
  select(scientific_name, TIR, Helitrons, Mavericks, Cryptons, Lineage) %>%
  mutate(DNApropTE = TIR+Helitrons+Mavericks+Cryptons)
plot(DNALen$DNALength, DNACopy$DNACopy)
plot(DNApropTE$DNApropTE,DNACopy$DNACopy)
cor.test(DNALen$DNALength, DNACopy$DNACopy, method="spearman")
cor.test(DNApropTE$DNApropTE,DNACopy$DNACopy, method="spearman")


########