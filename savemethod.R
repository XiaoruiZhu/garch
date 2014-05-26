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
# 如果想看看当前工作目录中有些什么文件，用下面的命令，也即查看下刚才保存的文件是否真的保存了！
list.files(pattern = ".rda")
[1] "Forbes2000.rda"
# 保存为.Rda的R标准文件，此文件不能如我们平时打开其它文件那样打开，如打开个word，双击就打开了，而此文件的读取，需要

# 用下面的命令在R中打开。

load("Forbes2000.rda")
# 载入以R标准数据保存的文件。
y
write.table(y,file='y.txt')