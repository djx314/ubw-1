# 项目构想

&emsp;&emsp;`fsn`最初的灵感来源于 @jilen 对我写的 [slick](https://github.com/slick/slick) 动态 insert 功能的一个思路整理 [Pull Request](https://github.com/scalax/hf/pull/5) ，现在无论是 api 还是内部实现（内部目前用 ListAnyShape 实现，简单高效）都已经面目全非了，但核心的东西一直没变，此后一年的业余时间都在这方面工作，现在把到目前的成果分享一下。

&emsp;&emsp;针对于目前的简单 MVC Web 开发，数据都是经过相似的链路，由服务器到 View，再经过用户操作，由 View 到服务器进行持久化。

1. 数据库 → View
>Sql  (Object Relation Mapping)→  Model  (Object Json Mapping)→  Json  (Json View Mapping)→  View

2. View -> 数据库
>View  (Input Data)→  Json  (Object Json Mapping)→  Model  (Object Relation Mapping)→  Sql

`fsn`在`Object Relation Mapping`、`Object Json Mapping`、`Json View Mapping`都有发挥它应有的作用，但做`fsn`的初衷是解决 [slick](https://github.com/slick/slick) 在`ORM`或者更准确的说`FRM`中遇到的一些不和谐的地方，让`slick`真正地纯粹地发挥所谓的`FRM`作用。