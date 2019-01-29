library(readxl)
comment_data <- read_excel("comment_data.xlsx")
library(dplyr)
library(igraph)
library(Matrix)
library(MASS)
##generate trolltable from comment_data
##calculating network metrics
##identifiy comments reply to trolls
attach(comment_data)
trolltable=data.frame()
#names(trolltable)=c("user","com", "degree","outdegree","uw_outdegree","indegree","betweenness", "closeness","eigen","trollid","density","N","weak_comp","stron_comp","modularity","activity","weight","response")
for (i in 1:23){
    network=filter(comment_data, comment_data$`Community id`==i);
    l=0;
    for (j in 1:nrow(network)) {
        if (l<5 && network$troll[j]==TRUE){
            l=l+1;
            troll=network$user[j];
            network1=network[1:j,];
            activity=count_(network1, vars="user")
            D=cbind(network1$user, network1$Replyuser);
            D=D[complete.cases(D),];
  
            net=graph.data.frame(d=D,directed = T);
            E(net)$weight <- 1;
            #weighted outdegree
            outdegree=degree(net, v=V(net), mode="out");
            outdegree=data.frame(outdegree)
            net <- simplify(net, edge.attr.comb="sum");
            #troll reply
            edge=get.adjacency(net,attr='weight');
            trollreply=as.matrix(edge[troll,]);
            #unweighted outdegree
            uw_outdegree=degree(net,v=V(net),mode="out" );
            uw_outdegree=data.frame(uw_outdegree);
            #indegree
            indegree=degree(net, v=V(net), mode="in");
            indegree=data.frame(indegree)
            #all degree
            degree=degree(net, v=V(net),mode="all")
            degree=data.frame(degree)
            #betweenness
            betweenness=betweenness(net,v=V(net));
            betweenness=data.frame(betweenness);
            #closeness
            closeness=closeness(net,vids=V(net));
            closeness=data.frame(closeness);
            #eigenvector centrality
            eigenvector=eigen_centrality(net, directed=TRUE);
            eigen=data.frame(eigenvector$vector);
            eigen=scale(eigen);
            
            node=cbind(degree,outdegree,uw_outdegree,indegree,betweenness,closeness,eigen,trollreply);
            
            #community number
            com=rep(i,times=nrow(node));
            #density
            density=edge_density(net,loops=FALSE);
            dens=rep(density, times=nrow(node));
            #modularity
            community=edge.betweenness.community(net);
            modularity=modularity(community);
            #weakly-connected component &strongly-connected component
            componentw=count_components(net,mode="weak");
            weak_comp=rep(componentw, times=nrow(node));
            weak_comp=data.frame(weak_comp);
            components=count_components(net,mode="strong");
            stron_comp=rep(components,times=nrow(node));
            stron_comp=data.frame(stron_comp);
            #how many members
            n=nrow(node);
            N=rep(n, times=nrow(node));
            
            community=cbind(dens, N,weak_comp, stron_comp,modularity);
            # troll id
            trollid=rep(l,times=nrow(node));
            
            nodeall=cbind(com, user=rownames(node), node, trollid, community);
            nodeact=merge(nodeall, activity, by.x="user", by.y="user", all.x=TRUE)
            edge=as_edgelist(net);
            edge=data.frame(edge);
            edge=cbind(edge, E(net)$weight);
            trolledge=filter(edge, edge[,2]==troll);
            trolledge=trolledge[-2];
            names(trolledge)=c("user","weight");
            trolltable1=merge(nodeact, trolledge, by.x="user",by.y="user",all.x=TRUE);
            trolltable1[is.na(trolltable1)]=0;
            trolltable1=cbind(trolltable1, trolltable1$weight);
            names(trolltable1)=c("user","com", "degree","outdegree","uw_outdegree","indegree","betweenness", "closeness","eigen","trollreply","trollid","density","N","weak_comp","stron_comp","modularity","activity","weight","response");
            trolltable1$response[trolltable1$response>0]<-1;
            trollcumresp=sum(trolltable1$response);
            trollcumresp=rep(trollcumresp,times=nrow(node));
            trolltable1=cbind(trolltable1,trollcumresp);
            names(trolltable1)=c("user","com", "degree","outdegree","uw_outdegree","indegree","betweenness", "closeness","eigen","trollreply","trollid","density","N","weak_comp","stron_comp","modularity","activity","weight","response","trollcumresp");
            trolltable=rbind(trolltable,trolltable1);
        
        }
        else if (l<5 && network$troll[j]==FALSE) next 
        else if (l>=5) break
        
    }
}


detach(comment_data)

densre=lm(density~N, data=trolltable)
dens.re=densre$residuals
trolltable=cbind(trolltable,dens.re)
modre=lm(trolltable$modularity~trolltable$N)
mod.re=modre$residuals
trolltable=cbind(trolltable,mod.re)

group=group_by(trolltable,com)
comcum=data.frame(summarise(group,sum=sum(response)))
num=data.frame(table(trolltable$com))
comcumresp=data.frame()
for (i in 1:23){
  comcumresp1=rep(comcum[i,2], times=num[i,2]);
  comcumresp=data.frame(c(comcumresp,comcumresp1));
}
comcumresp=data.frame(t(comcumresp))
trolltable=cbind(trolltable,comcumresp)


write.csv(trolltable, "trolltable.csv")

trolltable=read.csv("trolltable.csv",header = TRUE)
names(trolltable)[24]='comcumresp'
names(trolltable)[26]='trollrespbef'
trolltable[is.na(trolltable)]=0

network_test=comment_data[1:87,]
D_test=cbind(network_test$user,network_test$Replyuser)
D_test=D_test[complete.cases(D_test),]
net_test=graph.data.frame(d=D_test,directed = T)
E(net_test)$weight=1
net_test=simplify(net_test, edge.attr.comb = "sum")
edge_test=as_edgelist(net_test)
edge_test=data.frame(edge_test)
trolledge_test=rbind(filter(edge_test,edge_test[,2]=='Krista Burmasa'), filter(edge_test,edge_test[,2]=='Fiby Q'),filter(edge_test,edge_test[,2]=='Angel Conejo'),filter(edge_test,edge_test[,2]=='Gulfwulf'),filter(edge_test,edge_test[,2]=='Terry R'))
subgraoh_test=induced_subgraph(net_test,v=trolledge_test[,1])
plot(subgraoh_test)
as_edgelist(subgraoh_test)
E(subgraoh_test)$weight





library(lme4)
attach(trolltable)
##control:activity,community level:N density residuals
sdens.re=scale(trolltable$dens.re)
M1=glmer(formula=response~degree+activity+N+sdens.re+(1|com/trollid), family=binomial(link="logit"),data=trolltable)
summary(M1)

M2=glmer(formula=response~uw_outdegree+indegree+uw_outdegree:indegree+activity+N+sdens.re+(1|com/trollid), family=binomial(link="logit"),data=trolltable)
summary(M2)

#M3=glmer(formula=response~indegree+activity+density+(1|trollid)+(1|com), family=binomial(link="logit"),data=trolltable)
#summary(M3)
#sindegree=scale(trolltable$indegree)


M4=glmer(formula=response~betweenness+activity+N+sdens.re+(1|com/trollid), family=binomial(link="logit"),data=trolltable)
summary(M4)

#still have problem with closeness
sclose=scale(trolltable$closeness)
M5=glmer(formula=response~sclose+activity+N+sdens.re+(1|com/trollid), family=binomial(link="logit"),data=trolltable)
summary(M5)
M5=glmer(formula=response~comcumresp+trollcumresp+comrespbef+trollrespbef+inteactiontroll+interactioncom+activity+N+mod.re+(1|com/trollid), family=binomial(link="logit"),data=trolltable)

##control:activity,N,weak_comp
M6=glmer(formula=response~degree+activity+N+weak_comp+(1|com/trollid), family=binomial(link="logit"),data=trolltable)
summary(M6)

#problem one.
M7=glmer(formula=response~uw_outdegree+indegree+uw_outdegree:indegree+activity+N+weak_comp+(1|com/trollid), family=binomial(link="logit"),data=trolltable)
summary(M7)

#soutdegree=scale(trolltable$outdegree)
#M7_1=glmer(formula=response~soutdegree+activity+N+(1|trollid)+(1|com), family=binomial(link="logit"),data=trolltable)
#summary(M7_1)

#M8=glmer(formula=response~indegree+activity+N+(1|trollid)+(1|com), family=binomial(link="logit"),data=trolltable)
#summary(M8)
#sindegree=scale(trolltable$indegree)
#M8=glmer(formula=response~sindegree+activity+indegree:activity+N+(1|trollid)+(1|com), family=binomial(link="logit"),data=trolltable)
#summary(M8)

M9=glmer(formula=response~betweenness+activity+N+weak_comp+(1|com/trollid), family=binomial(link="logit"),data=trolltable)
summary(M9)

M10=glmer(formula=response~sclose+activity+N+weak_comp+(1|com/trollid), family=binomial(link="logit"),data=trolltable)
summary(M10)

#M10_1=glmer(formula=response~sclose+activity+N+(1|trollid)+(1|com), family=binomial(link="logit"),data=trolltable)
#summary(M10_1)

M11=glmer(formula=response~degree+activity+N+stron_comp+(1|com/trollid), family=binomial(link="logit"),data=trolltable)
summary(M11)

sdegree=scale(trolltable$degree)
#M11_1=glmer(formula=response~sdegree+activity+N+density+(1|trollid)+(1|com), family=binomial(link="logit"),data=trolltable)
#summary(M11_1)

#problem
M12=glmer(formula=response~uw_outdegree+indegree+uw_outdegree:indegree+activity+N+stron_comp+(1|com/trollid), family=binomial(link="logit"),data=trolltable)
summary(M12)

#M13=glmer(formula=response~indegree+activity+N+density+(1|trollid)+(1|com), family=binomial(link="logit"),data=trolltable)
#summary(M13)

M14=glmer(formula=response~betweenness+activity+N+stron_comp+(1|com/trollid), family=binomial(link="logit"),data=trolltable)
summary(M14)

#problem model failed to converge 
M15=glmer(formula=response~sclose+activity+N+stron_comp+(1|com/trollid), family=binomial(link="logit"),data=trolltable)
summary(M15)


###negative binomial 
M16=glmer(formula=weight~degree+activity+N+dens.re+(1|com/trollid), family=poisson(link="log"), data=trolltable, subset= weight>0)
summary(M16)

##problem
M17=glmer(formula=weight~uw_outdegree+indegree+uw_outdegree:indegree+activity+N+sdens.re+(1|com/trollid), family=poisson(link="log"), data=trolltable, subset= weight>0)
summary(M17)

#problem
sbetween=scale(trolltable$betweenness)
M18=glmer(formula=weight~sbetween+activity+N+sdens.re+(1|com/trollid), family=poisson(link="log"), data=trolltable, subset= weight>0)
summary(M18)

M19=glmer(formula=weight~sclose+activity+N+sdens.re+(1|com/trollid), family=poisson(link="log"), data=trolltable, subset= weight>0)
summary(M19)

#problem
M20=glmer(formula=weight~degree+activity+N+weak_comp+(1|com/trollid), family=poisson(link="log"), data=trolltable, subset= weight>0)
summary(M20)

M21=glmer(formula=weight~uw_outdegree+indegree+eigen+activity+N+weak_comp+(1|com/trollid), family=poisson(link="log"), data=trolltable, subset= weight>0)
summary(M21)

M22=glmer(formula=weight~sbetween+activity+N+weak_comp+(1|com/trollid), family=poisson(link="log"), data=trolltable, subset= weight>0)
summary(M22)

M23=glmer(formula=weight~sclose+activity+N+weak_comp+(1|com/trollid), family=poisson(link="log"), data=trolltable, subset= weight>0)
summary(M23)

##five emtions IBM tone analyizer
trolltone=data.frame(mat.or.vec(23*5, 7))
for (i in 1:23){
  network=filter(comment_data, comment_data$`Community id`==i);
  l=0;
  for (j in 1:nrow(network)) {
    if (l<5 && network$troll[j]==TRUE){
      l=l+1;
      result=text_tone(network$commentText[j],SERVICE_USERNAME_PASSWORD);
      result=unlist(result);
      trolltone[(i-1)*5+l,1]=network$user[j];
      trolltone[(i-1)*5+l,2]=result[1];
      trolltone[(i-1)*5+l,3]=result[2];
      trolltone[(i-1)*5+l,4]=result[3];
      trolltone[(i-1)*5+l,5]=result[4];
      trolltone[(i-1)*5+l,6]=result[5];
      trolltone[(i-1)*5+l,7]=network$`Level 1 probability`[j]
     
    }
    else if (l<5 && network$troll[j]==FALSE) next 
    else if (l>=5) break
  }  
}

trollid=rep(1:5,times=23)
trolltone=cbind(trollid,trolltone)
com=rep(1:23,each=5)
trolltone=cbind(com,trolltone)

tone=data.frame(mat.or.vec(3692,6))
trolltable=cbind(trolltable,tone)
names(trolltable)[29]='anger'
names(trolltable)[30]='disgust'
names(trolltable)[31]='fear'
names(trolltable)[32]='joy'
names(trolltable)[33]='sadness'
names(trolltable)[34]='negativity'
for (i in 1:3692){
  for (j in 1:23){
    if (trolltable[i,3]==j){
      for (l in 1:5){
        if (trolltable[i,12]==l){
          trolltable[i,29]=trolltone[(j-1)*5+l,4]
          trolltable[i,30]=trolltone[(j-1)*5+l,5]
          trolltable[i,31]=trolltone[(j-1)*5+l,6]
          trolltable[i,32]=trolltone[(j-1)*5+l,7]
          trolltable[i,33]=trolltone[(j-1)*5+l,8]
          trolltable[i,34]=trolltone[(j-1)*5+l,9]
          break
        }
      } 
      break
    }
  }
}
anger1=as.numeric(trolltable$anger)
disgust1=as.numeric(trolltable$disgust)
fear1=as.numeric(trolltable$fear)
joy1=as.numeric(trolltable$joy)
sadness1=as.numeric(trolltable$sadness)
negativity1=as.numeric(trolltable$negativity)

M5=glmer(formula=response~uw_outdegree+indegree+log_comcumresp+log_comrespbef+anger+disgust+fear+joy+sadness+log_act+N+sdens.re+(1|trollid)+(1|com), family=binomial(link="logit"),data=trolltable)



