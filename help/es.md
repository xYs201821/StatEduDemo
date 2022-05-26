
## <center>**假设检验1**</center>

处理单样本$x_1,x_2,x_3,...,x_n$情况的假设检验。



#### observed-value  控制样本的真实分布，**建议设置好参数后再进行检验**

  + level $ \alpha $ 控制检验水平，默认值0.05

  + Sample 1 Size 样本容量

  + Population 选择真实分布，选择后会出现参数控制面板

#### 简单检验
  
  **控制逻辑是通用的**
    
  + Type of $H_0$ 选择假设种类，双边/单边

  + $\mu_0$ 控制$\Theta_0$边界值

  + test0 选择$\Theta_0$中的测试值

  + test1 选择$\Theta_1$中的测试值

##### Z

>单样本Z检验，当正态总体方差$\sigma^2$已知时，对均值$\mu$的检验。

参数 $\sigma^2$ , 检验中“已知”的总体方差

##### Normal Variance

>正态总体方差检验，对正态总体方差$\sigma^2$的检验。

参数 $\mu \, known$ , 总体均值是否已知，影响卡方分布的自由度(检验统计量也应变化，~~还没加上去~~)

##### t
    
>单样本 *t* 检验，正态总体方差未知时，对均值$ \mu $的检验

参数 $\sigma$ ， 检验中未知的总体标准差(影响test1曲线的生成)

---
<h6 id='anchor4'></h6>

![OverView4](image/4-2.png)

### 主面板

例图为Z检验  $H_0: \mu_0 = 0$ <-----> $H_1: \mu_0 \neq 0$

样本为$X_1,X_2,...,X_{15} \sim N(0,1) \ \ i.i.d$  

检验水平$\alpha = 0.05$,作图选取了 $test0 = 0,test1 = 1$

+ Generate Real Data 生成真实分布的样本(每次点击都会重新生成)

+ Show Type1 error,显示会发生<font color="#660000">Ⅰ型错</font>的区域(图中<font color="#660000">深红</font>部分)

+ Show Type2 error,显示会发生<font color="#0000dd">Ⅱ型错</font>的区域(图中<font color="#0000dd">深蓝</font>部分)

+ Show Rejection Area 显示<font color="orange">拒绝域</font> (<font color="orange">橙色</font>箭头下方) 与<font color="green">接受域</font> (<font color="green">绿色</font>线段下方)
  |test_Statistic |Accept_Area_Lower| Accept_Area_Upper|p_value|
  |:----------:|:------:|:------:|:------:|
  |当前样本所得到的检验统计量的值|接受域的下界|接受域的上界|当前样本的p值|

+ 图片中<font color="#000066">淡蓝区域</font>边界为零假设下检验统计量的理论概率密度曲线，<font color="#dd0000">浅红区域</font>边界为对立假设下检验统计量的理论概率密度曲线。**黑色虚线**为直线x=test_Statistic。

+ 解读:当前抽样样本的样本均值为-0.1330，落在拒绝域$R: (-\infty,-0.5061) \cup (0.5061,\infty)$外，故不能拒绝零假设

---