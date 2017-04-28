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

1. 类型安全，在不断迭代的`Query`运算中可以保持类型信息不丢失；

1. 框架本身几乎没有运行时消耗（尤其在对象关系映射方面）；

1. 设计严密，几乎可以映射所有形式十分复杂的 sql 语句；

1. `slick` 3.1.0 以后生成的 sql 语句的简洁程度几乎可以与手写的相媲美。

但为了类型安全和映射对象`slick`也做出了一些牺牲，下面通过一些简单的例子重点说明一下这些不和谐的地方，这跟`fsn` `slick`部分的模块的设计目标有很大的关系。

1. 烦人的 sortBy

`slick` `Query` 的 `sortBy` 由于需要类型安全的原因，在需要动态 sortBy 的情况下代码略为臃肿。例如我在前端以列标识（string）和 isDesc 参数（boolean）为参数 parse 成 json 传到服务器要求查询时先对某一列进行排序
```javascript
{ sortColumn: "name", isDesc: true }
```

由于类型安全的限制，你只能编写以下代码：
```scala
def sortByName(query: Query[FriendTable, FriendTable#TableElementType, Seq], colName: String, isDesc: Boolean): Query[FriendTable, FriendTable#TableElementType, Seq] = {
  import slick.lifted.{ Ordered => SlickOrdered }

  val repToOrder = { friend: FriendTable =>
    val order = colName match {
      case "id" => friend.id: SlickOrdered
      case "name" => friend.name: SlickOrdered
      case "nick" => friend.nick: SlickOrdered
      case "age" => friend.age: SlickOrdered
      case "grade" => friend.grade: SlickOrdered
      case _ => throw new IllegalArgumentException("没有匹配的数据库列")
    }
    if (isDesc) {
      new SlickOrdered(order.columns.map(s => s.copy(_2 = s._2.desc)))
    } else {
      new SlickOrdered(order.columns.map(s => s.copy(_2 = s._2.asc)))
    }
  }
  query.sortBy(repToOrder)
}
```

而且这种处理对于复杂列（例如列相加）的处理需要编写更多的 case 分支并且要自己定义命名规则需要前端匹配，`Query`类型信息发生变化后需要重新编写匹配函数（`groupBy`）。有个朋友自己写了个 `macro`，放在 table 代码中可以自动根据字符串匹配所有的列，但毕竟治标不治本，复杂列和复杂类型依然无法处理，而且部分不能排序的列又需要做特殊处理。

不仅`slick`对于`sortBy`的处理表现欠佳，其他 java 的 orm 框架（`hibernate`、`mybatis`）对排序和分页的官方支持也是一般，例如`hibernate`在多层对象嵌套的情况下几乎不能简单（甚至通过反射）读取到列的信息用作自动匹配排序逻辑。