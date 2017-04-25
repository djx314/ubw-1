# 项目构想

## 总览

&emsp;&emsp;`fsn`最初的灵感来源于 @jilen 对我写的 [slick](https://github.com/slick/slick) 动态 insert 功能的一个思路整理 [Pull Request](https://github.com/scalax/hf/pull/5) ，现在无论是 api 还是内部实现（内部目前用 ListAnyShape 实现，简单高效）都已经面目全非了，但核心的东西一直没变，此后一年的业余时间都在这方面工作，现在把到目前的成果分享一下。

&emsp;&emsp;针对于目前的简单 MVC Web 开发，数据都是经过相似的链路，由服务器到 View，再经过用户操作，由 View 到服务器进行持久化。

1. 数据库 → View
>Sql  (Object Relation Mapping)→  Model  (Object Json Mapping)→  Json  (Json View Mapping)→  View

2. View -> 数据库
>View  (Input Data)→  Json  (Object Json Mapping)→  Model  (Object Relation Mapping)→  Sql

`fsn`在`Object Relation Mapping`、`Object Json Mapping`、`Json View Mapping`都有发挥它应有的作用，但做`fsn`的初衷是解决 [slick](https://github.com/slick/slick) 在`ORM`或者更准确的说`FRM`中遇到的一些不和谐的地方，让`slick`真正地纯粹地发挥所谓的`FRM`作用。

## slick 之不和谐

&emsp;&emsp;`slick`满足了我认识他之前对数据库操作框架的诸多幻想：

1. 类型安全，在不断迭代的`Query`运算中可以保持类型不丢失；

1. 框架本身几乎没有运行时消耗（尤其在对象关系映射方面）；

1. 设计严密，几乎可以映射所有形式十分复杂的 sql 语句；

1. `slick` 3.1.0 以后生成的 sql 语句几乎可以与手写的相媲美。

但为了类型安全和映射对象`slick`也做出了一些牺牲，下面通过一些简单的例子重点说明一下这些不和谐的地方，这跟`fsn` `slick`部分的模块的设计目标有很大的关系。