% 逐点进行邹致庄检验（Chow test）的函数。



% 目的：扩展了邹检验的方法，逐点算F统计量以探测断点.
% 调用时要输入的变量有3个：
%（1）被解释变量y
%（2）解释变量x（在调用之前，必须用Eviews确定正确的模型设定，尤其是含不含常数项）
%（3）n，表示从x的第n个观测开始检验。

% 注意：
%（1）假设[nobs nvar]=size(x)，x不含常数项。则y对(c,x)回归并做Chow检验的F统计量是：
% F=((rsqsum-rsqsum1-rsqsum2)/(nvar+1))/((rsqsum1+rsqsum2)/(nobs-2*nvar-2));
% 换句话说，若[nobs nvar]=size(x)且x中有一个常数项c，则y对x回归并做Chow检验的F统计量是：
% F=((rsqsum-rsqsum1-rsqsum2)/(nvar))/((rsqsum1+rsqsum2)/(nobs-2*nvar));
%（2）假设[nobs nvar]=size(x)，x不含常数项。则y对x回归并做Chow检验的F统计量是：
% F=((rsqsum-rsqsum1-rsqsum2)/(nvar))/((rsqsum1+rsqsum2)/(nobs-2*nvar));
%即，只要设定x中解释变量是nvar个，不管里面是不是有常数项，则y对x回归并做Chow检验的F统计量都是
% F=((rsqsum-rsqsum1-rsqsum2)/(nvar))/((rsqsum1+rsqsum2)/(nobs-2*nvar));
%因此我们只要在调用本函数之前设定x（包括或者而不包括常数项）即可。

function Chow = function_of_Chow_test_point_by_point(y,x,n)
[nobs nvar] = size(x);                   %nobs为向量x的行数（观测数），nvar为向量x的列数（变量数）。
beta=((x'*x)\eye(nvar))*(x'*y);          %beta为y对x和一个常数项回归得到的系数向量。
      yhat=x*beta;                             %yhat为y的拟合值。
      resid = y - yhat;                        %残差。
      rsqsum=resid'*resid;                     %回归的残差平方和。
      
      Fstat=[];                                %准备存放F统计量
      p=[];                                    %准备存放每个p值对应的p值。
      for i=n:nobs-n;                          %从第n个观测开始检验，到第nobs-n个为止，共检验nobs-2n+1次(最前面n-1个和最后n个没检验)。
      y1=y(1:i);                            %第一个回归的被解释变量
      x1=x(1:i,:);                          %第一个回归的解释变量
      nvar1=size(x1,2);                     %第一个回归的观测数目
      beta1=((x1'*x1)\eye(nvar1))*(x1'*y1); %第一个回归的系数估计量
                                   y1hat=x1*beta1;                       %第一个回归的拟合值
                                   resid1 = y1-y1hat;                    %第一个回归的残差
                                   rsqsum1=resid1'*resid1;               %第一个回归的残差平方和
  
   y2=y(i+1:nobs);                       %第二个回归的被解释变量
   x2=x(i+1:nobs,:);                     %第二个回归的解释变量
   nvar2=size(x2,2);                     %第二个回归的观测数目
   beta2=((x2'*x2)\eye(nvar2))*(x2'*y2); %第二个回归的系数估计量
   y2hat=x2*beta2;                       %第二个回归的拟合值
   resid2 = y2-y2hat;                    %第二个回归的残差
   rsqsum2=resid2'*resid2;               %第二个回归的残差平方和
          
          Fstat(i-n+1)=((rsqsum-rsqsum1-rsqsum2)/(nvar))/((rsqsum1+rsqsum2)/(nobs-2*nvar));
          p(i-n+1)=1-fcdf(Fstat(i-n+1),(nvar),(nobs-2*nvar));
          %注意i是n开始的循环，因此i-n+1是从1开始的。
          end;
          Chow = [Fstat' p'];                     %报出每个观测的序号，F统计量和p值。
          