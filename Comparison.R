library(network)
Argentina <- readRDS("NetworkData/Argentina.RDS")
network::set.vertex.attribute(Argentina, "OECD", c(rep(FALSE, 514), rep(NA, 10)))
Brazil <- readRDS("NetworkData/Brazil.RDS")
network::set.vertex.attribute(Brazil, "OECD", c(rep(FALSE, 922), rep(NA, 10)))
Chile <- readRDS("NetworkData/Chile.RDS")
network::set.vertex.attribute(Chile, "OECD", c(rep(TRUE, 208), rep(NA, 10)))
Colombia <- readRDS("NetworkData/Colombia.RDS")
network::set.vertex.attribute(Colombia, "OECD", c(rep(TRUE, 230), rep(NA, 10)))
CostaRica <- readRDS("NetworkData/CostaRica.RDS")
network::set.vertex.attribute(CostaRica, "OECD", c(rep(TRUE, 120), rep(NA, 10)))
Ecuador <- readRDS("NetworkData/Ecuador.RDS")
network::set.vertex.attribute(Ecuador, "OECD", c(rep(FALSE, 731), rep(NA, 10)))
Mexico <- readRDS("NetworkData/Mexico.RDS")
network::set.vertex.attribute(Mexico, "OECD", c(rep(TRUE, 552), rep(NA, 10)))
Uruguay <- readRDS("NetworkData/Uruguay.RDS")
network::set.vertex.attribute(Uruguay, "OECD", c(rep(FALSE, 147), rep(NA, 10)))
Venezuela <- readRDS("NetworkData/Venezuela.RDS")
network::set.vertex.attribute(Venezuela, "OECD", c(rep(FALSE, 210), rep(NA, 10)))

RegionalNetworks <- list(Argentina, Brazil, Chile, Colombia, CostaRica, Ecuador, Mexico, Uruguay, Venezuela)

RegionalNetworks
country_names <- c("Argentina", "Brazil", "Chile", "Colombia", "CostaRica", "Ecuador", "Mexico", "Uruguay", "Venezuela")
RegionalNetworks <- setNames(RegionalNetworks[1:9],country_names)

saveRDS(RegionalNetworks, file = "NetworkData/RegionalNetworks.RDS")
rm(list=setdiff(ls(), c("RegionalNetworks")))
library(purrr)
library(tibble)
library(knitr)
library(dplyr)
RegionalNetworks <- readRDS("NetworkData/RegionalNetworks.RDS")
RegionalNetworks

library(igraph)
load("LatamNetwork.RData")
#RN <- graph_from_data_frame(RegionNetwork, directed = FALSE)
#RN <- as_adjacency_matrix(RN, attr = "Weight", sparse = FALSE)
library(tnet)
Region.network <- RegionNetwork[1:2]
Region.network$Source <- as.integer(factor(Region.network$Source))
Region.network$Target <- as.integer(factor(Region.network$Target)) 
pave <- as.tnet(Region.network, type = "binary two-mode tnet")
Region.Clustering <- tnet::reinforcement_tm(pave)
ClusteringARG <- tnet::reinforcement_tm(t(Matriz))

length(RegionalNetworks)
RegionalNetworks
RegionalNetworks %>% keep(`%n%`, "OECD")
RegionalNetworks %>% discard(`%n%`, "OECD") %>% map(as_tibble, unit="vertices")
RegionalNetworks  %>% map(as_tibble, unit="vertices")


RegionalNetworks %>%
  imap(~ {
    mnext_value <- .x$gal$mnext
    list(
      Country = .y,
      OECD.Member = .x %n% "OECD",
      n = network.size(.x),
      d = network.density(.x),
      Clustering = get.network.attribute(.x, "Clustering"),
      mnext = ifelse(is.null(mnext_value), NA_integer_, mnext_value)
    )
  }) %>%
  bind_rows() %>%
  group_by(Country) %>%
  summarize(
    OECD.Member = first(OECD.Member),
    Edges = mnext,
    Size = sum(n),
    Density = mean(d),
    Clustering = first(Clustering)
  ) %>%
  kable()

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
load("Curated_Data/AllProgramsSkills.RData")
Skills <- AllPrograms %>% filter(., Partition == "Skill")
Skills$Node <- gsub("^(ARG|BRA|CHL|COL|CR|ECU|MEX|URU|VEN)_", "", Skills$Node)


png(filename = "FS.png", width = 40, height = 18, units = "in", res = 300)
ggplot(Skills, aes(x=reorder(Node, -Eigenvector), y=Eigenvector, fill = Level))+
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual("", values = c("Specialization" = "royalblue4", "Master" = "lightsteelblue3", "PhD" = "lightskyblue2")) +
#  scale_fill_manual(values = c("steelblue1", "slateblue1", "slateblue4")) +
  facet_wrap(. ~ Country) +
  theme_linedraw() +
  coord_flip()+
  theme(axis.text.x = element_text(angle=0, hjust=1, size =40),
        axis.text.y = element_text(size = 40),
        axis.title.x = element_text(size = 30, colour = "black"),
        axis.title.y = element_text(size = 50, colour = "black"),
        legend.text = element_text(size = 30),  
        legend.title = element_text(size = 20), 
        legend.position = c(0.93, 0.93),
        strip.text = element_text(face="bold", size=rel(3.5), colour = "black"),
        strip.background = element_rect(fill="grey", colour="grey",
                                        size=30,))+
  xlab("") + ylab("Normalized centrality degree of basic skills") +
  labs(fill="")
dev.off()


ggplot(Skills, aes(Level, Node, fill= Eigenvector)) + 
  geom_tile() + 
  scale_x_discrete(limits = c("Specialization", "Master", "PhD"))+
  facet_wrap(~Country) +
  theme(axis.text.x = element_text(angle=0, hjust=1, size = 30, colour = "black"),
        axis.text.y = element_text(size = 30, colour = "black"),
        axis.title.x = element_text(size = 30, colour = "black"),
        axis.title.y = element_text(size = 50, colour = "black"),
        legend.text = element_text(size = 15),  
        legend.title = element_text(size = 20), 
        legend.position="right",
        strip.text = element_text(face="bold", size=rel(5.5), colour = "black"),
        strip.background = element_rect(fill="grey", colour="grey",
                                        size=30))+
  xlab("Program Level") + ylab("Basic Skills") +
  scale_fill_gradient(low = "lightblue1", high = "#09419e") +
  labs(fill="Centrality")


library(ergm.multi)
SampledNetworks <- Networks(RegionalNetworks)
# debugging
# head_networks <- head(RegionalNetworks, 9)
# SampledNetworks_head <- tryCatch(Networks(head_networks), error = function(e) e)
# print(SampledNetworks_head)
class(SampledNetworks)
SampledNetworks
# the term b1degree(3) specifies that programs, on average, should connect to 
# three skills (3 is the average degree centrality). If the estimated term is
# negative, that means programs would tend to specialize in fewer skills, if
# the estimated term is positive, that means programs would tend to connect to more skills

mod0 <- ergm(SampledNetworks ~ N(~ edges))
summary(mod0)
mod1 <- ergm(SampledNetworks ~ N(~edges + b1nodematch("Country", diff = FALSE)))
summary(mod1)
mod2 <- ergm(SampledNetworks ~ N(~edges + b1degree(3)))
summary(mod2)
mod3 <- ergm(SampledNetworks ~ N(~edges + b1degree(3) + b1factor("Program")))
summary(mod3)
mod4 <- ergm(SampledNetworks ~ N(~edges + b1degree(3) + b1factor("Program") + b1cov("Brochure.Length")))
summary(mod4)
mod5 <- ergm(SampledNetworks ~ N(~ edges + b1cov("Brochure.Length")))
summary(mod5)
#mcmc.diagnostics(mod1)

GOF1 <- gof(mod1)

mod6 <- ergm(SampledNetworks ~ N(~edges + b1degree(3,by="Program", levels = "Doctorado")))
summary(mod6)
exp(mod1$coefficients)/(1+exp(mod1$coefficients))
# About 28.56% of all possible edges actually exist.