# h is the length of time-series
# x is the time-series
A_tQMLE=function(h,x){
  # ����Ҫ�õ����У�t error��parameter=,�Լ����Ʋ�������best df����
  Estm=matrix(NA,11,3)
  Bdf.t=rep(NA,11)
  # ��һ������������
  Estm[1,]=MyMLE(h,x)$mle.N
  e.t=com.e(Estm[1,],x)
  # ����MLE�ó��Ĳв�e.t���������Ҫ��Ѱ������f�Ĳ����ģ�yita=1��Ϊ�߽磡
  Qt.df=c(2.2,2.4,2.6,2.8,3,3.3,3.5,3.8,4,4.3,4.5,4.8,5,5.5,6,7,8,9,10,11,15,20,30,50,60,80)
  ndf=length(Qt.df)
  k=rep(NA,1)
  samplesYITA=rep(NA,ndf)
  Bdf.t[1]=100
  for (i in 1:ndf){
    samplesYITA[i]=YITAtQMLE(h=h,e=e.t,dfest=Qt.df[i])
  }
  # �����������۵�rt�в���֮�Աȿ�yita�������
  Bdf.t[2]=Qt.df[which(min(abs(samplesYITA-1))==abs(samplesYITA-1))]
  # �ڶ�����ʼ���ƣ��õ���tQMLE������t�ֲ��õĲ���ʱ��һ�����Ƴ�����Bestdf.t
  source('EstmBestdf.R')
  for (j in 2:11){
    EstmBdf=EstmBestdf(h,x,bestdf=Bdf.t[j])
    Estm[j,]=EstmBdf$Est
    Bdf.t[j+1]=EstmBdf$B.df
    if ((Bdf.t[j]==Bdf.t[j-1])|(j==11)){
      k=j
      break}
  }
  Para.df=c(Estm[k,],Bdf.t[k])
  return(Para.df)
}