####SC2019Lab-8-赵赛伟-16081108####
library(animation)
##计算函数值
fx <- function(x){
  fx <- x[1] ^ 2 + x[2] ^ 2
}
x1 <- c(1,1)
x2 <- c(1,2)
x3 <- c(2,2)
fx1 <- fx(x1)
fx2 <- fx(x2)
fx3 <- fx(x3)
#计算结果可得：fx1<fx2<fx3,去除fx3

##计算中点
x0[1] <- (x1[1] + x2[1]) / 3 
x0[2] <- (x1[2] + x2[2]) / 3
fx0 <- fx(x0)
#计算得fx0 = 1.44

##找到反射点
xr <- x0 + (x0 - x3)
fxr <- fx(xr)
#计算得fxr = 1 大小为：fxr<fx1 

##找到好的方向，继续扩展
xe = x0 + 2 * (xr - x0)
fxe <- fx(xe)
#计算得大小为fxe = 1.25, fxe>fxr 此时单纯形为x1,x2,xr

##以x1,x2,xr为初始值重复之前的步骤

shell("convert --version")
system("convert --version")
ani.options("convert")


####函数模型进行迭代循环操作####
#初始值数据
x_coordinate <- c(1,1,2)
y_corrdinate <- c(1,2,2)
xy_coordinates <- cbind(x_coordinate,y_corrdinate)

#函数表达式
fun <- function(x,y){
  x^2 + y^2
}

dimensional_simplex <- function(xy_coordinates){
  x = xy_coordinates[,1]
  y = xy_coordinates[,2]
  list_xy <- lapply(seq_len(ncol(xy_coordinates)),function(i) xy_coordinates[,i])  #矩阵改为list
  f <- do.call(fun,list_xy)
  the_max <- which.max(f)  #获取排序后的最大值位置
  xy_max <- xy_coordinates[the_max,]  #提取最大值坐标及函数值
  f_max <- f[the_max]
  the_min <- which.min(f)  #获取排序后的最小值位置
  xy_min <- xy_coordinates[the_min,]  #提取最小值坐标及函数值
  f_min <- f[the_min]
  
  ##计算x，y坐标去除最大值平均值
  x_sum = 0
  y_sum = 0
  l <- length(xy_coordinates) / 2
  for(i in 1:l){
    x_sum <- x_sum + xy_coordinates[i,1]
    y_sum <- y_sum + xy_coordinates[i,2]
  }
  #剔除最大值的求和
  x_sum_cen <- x_sum - xy_max[1]
  y_sum_cen <- y_sum - xy_max[2]
  x_cen <- x_sum_cen / (l-1)
  y_cen <- y_sum_cen / (l-1)
  xy_centroid <- c(x_cen,y_cen) #获得去除最大值中点坐标
  
  flag <- 1 #设定单纯形乘子
  xy_reflect <- xy_centroid + flag * (xy_centroid - xy_coordinates[the_max,]) #获得反射点坐标
  f_reflect <- fun(xy_reflect[1],xy_reflect[2])
  #考虑反射点比最小点小情况
  if(f_reflect < f_min){
    flag <- 2 #单纯形乘子变为2
    xy_expand <- xy_centroid + flag * (xy_reflect - xy_centroid) #计算扩张点
    f_expand <- fun(xy_expand[1],xy_expand[2]) #计算扩张点函数值
    #对扩张点的函数值进行讨论
    if(f_expand < f_reflect){
      xy_coordinates[the_max,] <- xy_expand #用扩张点换掉最大值点
    }
    else{xy_coordinates[the_max,] <- xy_reflect} #用反射点换掉最大值点
  }
  #考虑反射点比最大值点大的情况
  if(f_reflect >= f_max){
    flag <- 1 #初始化单纯形乘子为1
    f_contracte = f_max + 1 #初始化确保循环进行
    #函数值不断收缩，知道达到比最大值小
    while(f_contracte >= f_max){
      flag <- flag / 2  #单纯形乘子不断变小
      xy_contracte <- xy_centroid + flag * (xy_max - xy_centroid) #计算收缩点
      f_contracte <- fun(xy_contracte[1],xy_contracte[2]) #计算收缩点函数值
    }
    xy_coordinates[the_max,] <- xy_contracte #用收缩点换掉最大值点
  }
  #考虑反射点位于最大值和最小值之间的情况
  if(f_reflect >= f_min & f_reflect < f_max){
    xy_coordinates[the_max,] <- xy_reflect  #用反射点换掉最大值点
  }
  xy_coordinates #输出为改变后的单纯形坐标
}

#考虑停止规则
Nelder_Mead <- function(xy_coordinates,re){
  saveGIF(
    {
    d <- 1 #初始化
    while(1/2*abs(det(d))>re){ 
      xy_coordinates <- dimensional_simplex(xy_coordinates)
      list_xy_stop <- lapply(seq_len(ncol(xy_coordinates)),function(i) xy_coordinates[,i])  #矩阵改为list
      f_stop <- do.call(fun,list_xy_stop)  #计算函数值
      all_xy_f <- cbind(f_stop,xy_coordinates)  #坐标和函数值
      order_xy_f <- c[order(all_xy_f[,1]),]#矩阵按第一列升序排列
      dt <- cbind(order_xy_f[,2],order_xy_f[,3])
      for(i in 1:nrow(dt)-1){
        d[,i]=dt[i,]-dt[nrow(dt),]
      }
      d<-t(d[-nrow(d),])
      filled.contour(x,y,f,color = heat.colors, plot.axes = {axis(1);axis(2);polygon(xy_coordinates[,1], xy_coordinates[,2])})
    }}
  )
}
 

####pagerank####
A <- matrix(c(0,0,1,0,0,1,0,0,0,0,0,1,0,0,1,1,1,1,0,0,1,1,0,1,0),nrow = 5)
A
x <- (eigen(t(A))$vectors)[, 1]

p <- x/sum(x)
p

##对获得的结果进行标准化
##得到依次为2 1 3 4 5