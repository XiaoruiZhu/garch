## Write an ASCII version of mean to the file "foo"
dput(mean, "foo")
## And read it back into 'bar'
bar <- dget("foo")
unlink("foo")
## Create a function with comments
baz <- function(x) {
  # Subtract from one
  1-x
}
mean
y=list(a=1,b=c(1:10),d=c('adsf','asdf'))
save(y, file = "Forbes2000.rda")
# ����뿴����ǰ����Ŀ¼����Щʲô�ļ�������������Ҳ���鿴�¸ղű�����ļ��Ƿ���ı����ˣ�
list.files(pattern = ".rda")
[1] "Forbes2000.rda"
# ����Ϊ.Rda��R��׼�ļ������ļ�����������ƽʱ�������ļ������򿪣���򿪸�word��˫���ʹ��ˣ������ļ��Ķ�ȡ����Ҫ

# �������������R�д򿪡�

load("Forbes2000.rda")
# ������R��׼���ݱ�����ļ���
y
write.table(y,file='y.txt')