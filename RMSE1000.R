# ����RMSE���Ʊ�
RMSE1000=function(dat,a,b){
  a1=a;b1=b
  # �������޲Ź��Ƴ����Ĳ�ɾ��������RMSE
  NEW.2NGQ3.1000=dat$E.2NGQMLE
  NEW.AtQ3.1000=dat$E.AtQ
  NEW.GQ3.1000=dat$E.GQ
  NEW.tQMLE3.1000=dat$E.tQMLE
  len=length(NEW.AtQ3.1000[,4])
  avgdf=mean(NEW.AtQ3.1000[,4])
  # ����2NQMLE������������RMSE
  mean(NEW.2NGQ3.1000)
  RMSE.2NGQ.w3=sqrt(sum((NEW.2NGQ3.1000[,1]-a1[1])^2)/len)
  RMSE.2NGQ.a3=sqrt(sum((NEW.2NGQ3.1000[,2]-a1[2])^2)/len)
  RMSE.2NGQ.b3=sqrt(sum((NEW.2NGQ3.1000[,3]-b1[1])^2)/len)
  # ����adaptive tQMLE������������RMSE
  mean(NEW.AtQ3.1000[,4])
  RMSE.w3=sqrt(sum((NEW.AtQ3.1000[,1]-a1[1])^2)/len)
  RMSE.a3=sqrt(sum((NEW.AtQ3.1000[,2]-a1[2])^2)/len)
  RMSE.b3=sqrt(sum((NEW.AtQ3.1000[,3]-b1[1])^2)/len)
  # ����GQ��RMSE
  # GQMLE�Ĳ���RMSE
  RMSE.GQ.w3=sqrt(sum((NEW.GQ3.1000[,1]-a1[1])^2)/len)
  RMSE.GQ.a3=sqrt(sum((NEW.GQ3.1000[,2]-a1[2])^2)/len)
  RMSE.GQ.b3=sqrt(sum((NEW.GQ3.1000[,3]-b1[1])^2)/len)
  # tQMLE�Ĳ���RMSE
  RMSE.tQ.w3=sqrt(sum((NEW.tQMLE3.1000[,1]-a1[1])^2)/len)
  RMSE.tQ.a3=sqrt(sum((NEW.tQMLE3.1000[,2]-a1[2])^2)/len)
  RMSE.tQ.b3=sqrt(sum((NEW.tQMLE3.1000[,3]-b1[1])^2)/len)
  ## ���������GQ�ı�ֵ
  ratio.2NGQ=c(RMSE.2NGQ.w3,RMSE.2NGQ.a3,RMSE.2NGQ.b3)/c(RMSE.GQ.w3,RMSE.GQ.a3,RMSE.GQ.b3)
  ratio.AtQ=c(RMSE.w3,RMSE.a3,RMSE.b3)/c(RMSE.GQ.w3,RMSE.GQ.a3,RMSE.GQ.b3)
  ratio.tQ=c(RMSE.tQ.w3,RMSE.tQ.a3,RMSE.tQ.b3)/c(RMSE.GQ.w3,RMSE.GQ.a3,RMSE.GQ.b3)
  RMSE.GQ=c(RMSE.GQ.w3,RMSE.GQ.a3,RMSE.GQ.b3)
  tableRMSE=rbind(avgdf,ratio.AtQ,ratio.2NGQ,RMSE.GQ,ratio.tQ)
  return(tableRMSE)
  ## �������( ƽ��bestdf AtQ���� 2NGQ���� RMSEofGQ tQ���� )������
}