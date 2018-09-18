# 困境

## 总览

&emsp;&emsp;`fsn`最初的灵感来自 @jilen 对 [slick](https://github.com/slick/slick) 动态 insert 扩展 [hf](https://github.com/scalax/hf) 的一个思路整理 [Pull Request](https://github.com/scalax/hf/pull/5) ，现在无论是 api 还是内部实现（内部实现目前用 ListAnyShape，简单高效）都已经面目全非了，但核心思想一直没变，此后一年的业余时间都在这方面工作，现在把到目前的成果分享一下。

&emsp;&emsp;针对目前简单的 MVC Web 开发，数据都是经过相似的链路，由服务器响应到 View，再经过用户操作，由 View 提交到服务器进行持久化。

1. 数据库 → View
>Sql  (Object Relation Mapping)→  Model  (Object Json Mapping)→  Json  (Json View Mapping)→  View

2. View -> 数据库
>View  (Input Data)→  Json  (Object Json Mapping)→  Model  (Object Relation Mapping)→  Sql

`fsn`在`Object Relation Mapping`、`Object Json Mapping`、`Json View Mapping`都有发挥它应有的作用，但做`fsn`的初衷是解决 [slick](https://github.com/slick/slick) 在`ORM`或者更准确的说`FRM`中遇到的一些不和谐的地方，让`slick`真正地纯粹地发挥所谓的`FRM`作用。

## slick 之不和谐

&emsp;&emsp;`slick`满足了我认识它之前对数据库操作框架的诸多幻想：

1. 类型安全，在不断迭代的`Query`运算中可以保持类型信息不丢失；

1. 框架本身几乎没有运行时消耗（尤其在对象关系映射方面）；

1. 设计严密，几乎可以映射所有形式十分复杂的 sql 语句；

1. `slick` 3.1.0 以后生成的 sql 语句的简洁程度几乎可以与手写相媲美。

但为了类型安全和映射对象`slick`也做出了一些牺牲，下面通过一些简单的例子重点说明这些不和谐的地方，这跟`fsn`中`slick`相关部分的设计有很大关系。

### 1. 烦人的 sortBy

`slick`中的`Query`在需要动态`sortBy`的情况下代码略为臃肿。例如我在前端以列标识（string）和 isDesc （boolean）为参数以 json 格式传到服务器作为数据库查询排序条件时

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

而且这种匹配在遇到复杂列（如列与列相加）时会需要更多的 case 分支，并且要自己定义命名规则与前端匹配，而`Query`类型信息发生变化后也要重新编写匹配函数（如经过`groupBy`操作）。有个朋友 @烟流 自己写了个 macro 放在 table 代码中，可以自动根据字符串匹配所有的列，但复杂列和复杂类型依然无法优雅处理，而且部分不允许排序的列需要程序再做特殊处理。

不仅`slick`对于`sortBy`的默认处理表现欠佳，其他 Java 的 ORM 框架`hibernate`、`mybatis`等对排序的官方支持也是一般，如`hibernate`在多层对象嵌套的情况下已不能简单地读取列信息用作排序逻辑匹配。而`Quill`这类在编译时就已经生成好 sql 语句的框架要实现这个需求的难度更大。

### 2. 动态列支持欠佳

由于`slick`的查询结果必须是明确的类型，所以无法在运行时动态决定需要查询的列。很多时候用`code gen`生成一个超过 30 列的实体后，如果实际只需要获取其中的 16 列以忽略大字符串类型数据，你通常会忍痛割爱地直接 query.result，而不是
```scala
def toJSON(query: Query[FriendTable, FriendTable#TableElementType, Seq]): DBIO[Seq[Map[String, Json]]] = {
  query.map { s =>
    (s.name, s.grade, s.age)
  }.result.map { list =>
    list.map { s =>
      Map(
        "name" -> s._1.asJson,
        "grade" -> s._2.asJson,
        "age" -> s._3.asJson
      )
    }
  }
}
```

而且即便是这种写法，也不能根据 List[String] 这类参数动态缩减查询输出的列。纵观其他的 ORM，对此的支持也是相当弱，而且在代码臃肿方面也与`slick`类似，需要针对每一类需求定制查询数据缩减的逻辑，在遇到某些大表的时候你简直可以看到眼前地狱的熔岩会如何吞噬你的时间。

在前些日子，facebook 发布了 GraphQL，可以动态定义自己的请求所需要的字段。如果只从 JSON 层面过滤输出信息，那只是一种减轻流量压力的办法，不能从数据库层面减少交互的数据量就没有真正的发挥 GraphQL 的作用。而 ORM 似乎是 GraphQL 的对立面，几乎没法让两者沟通。

### 3. 无法传递列信息

`slick`查询结果的数据容器都是 case class 或者 Tuple，至于数据实体中各个属性的含义，则需要文档补救。于是你会发现，在后端程序员扔给你一个接口之后，还要附上该接口的详细文档描述各属性的含义。而且前端在渲染一个 Grid 的时候：

```javascript
var layout = [[
    { "name": "姓名", "field": "name", "type": "string" },
    { "name": "年级", "field": "grade", "type": "int", "nullable": true },
    { "name": "年龄", "field": "age", "type": "int" }
]];
```

这个 layout 似乎是无法避免的。以至于目前的开发模式，是数据库列信息一份文档，后端建立模型一份文档，到了前端又要手写一份字段映射代码。代码是分层了，但一些列信息（尤其是列类型信息，如 data type，nullable，scale，maxLength 等）无法在不同的模块之中顺利传递，导致这些关键信息在每个模块中重复出现。

### 4. DTO 支持欠佳

DTO 支持的缺陷类似第二点动态列支持，但 DTO 还需要对模型的属性起一个别名，而某些属性不可能只把值传过就完成任务。例如我需要把提交数据中的 username 分解成 firstName 和 lastName 再持久化进数据库，但是取出展示的时候又需要把两列合并成 username 输出，并且这一步需要对外屏蔽，不能在前端做。又或者传输字段的数量，我需要把 js 端的 float 标准化成两位小数后再持久化进数据库。这些需求，目前而言是需要添加诸多代码，并且需要重复地建立类似的模型，有时候只有两三个字段不同，你就需要重新编写一个上 30 个字段的模型类。

### 5. 多类视图

数据库的查询结果一般是渲染成 Json 在网页展示。但如果需要输出到 Excel 的话事情就有点复杂了。`slick`的查询结果输出成 Json 只需要 asJson 即可。但要渲染成 Excel 条件限制就大了，Excel 的列宽，小数位数，formatter 等信息使得你几乎需要为每一个 model 量身定做一个导出方案。即使你使用注解和反射解决了这个问题，但万一碰到生成 Excel 某一列的小数位数需要由网页传入的参数决定这类问题，输出方案又要重构了。

### 6. 多端输入

同第 5 点，如果你的数据来源不仅仅是 Json，而是 Excel，CSV，甚至是其他数据库，你也需要分别为这些数据源定制持久化逻辑或展示逻辑。

### 6. 动态 filter

数据库的查询多带限制条件（`slick`的`filter`），但如果查询的条件是在运行时才决定的，你可能需要下面一段代码来掩盖`slick`的不足：

```scala
def queryFilter(query: Query[FriendTable, FriendTable#TableElementType, Seq], age: Option[Int], grade: Option[Int]): Query[FriendTable, FriendTable#TableElementType, Seq] = {
  val query1 = age match {
    case Some(s) => query.filter(_.age === s)
    case _ => query
  }
  grade match {
    case Some(s) => query1.filter(_.grade === s)
    case _ => query1
  }
}
```

如果我有 10 个参数需要这样动态决定呢？

### 7. 更动态的 sql 语句生成

需求是：我只有一个通用的 SimpleTable

```scala
class SimpleTable(tag: Tag) extends Table[Unit](tag, "student") {
  def * = ()
}
```

我需要根据一段很复杂的包含数据库表信息的 Json 生成一个 sql 生成方案：

1. 可以根据输入的 Json 过滤某些列的值

1. 可以根据字段名决定某些列的排序规则

1. 第 1 点和第 2 点的列都有限制，超出范围的过滤条件和排序条件会自动忽略

1. 表间关联和多维度统计等功能要齐全

1. 甚至可以处理有多表关联数据的插入和更新

这些功能你可以用市面上的开源框架做到么？

## 希望之光

&emsp;&emsp;上面的这几点不足，其实只是你不够了解`slick`，`slick`是 FRM 框架，对象映射只是它的一个附属功能，只不过因为其应用范围广，覆盖了其他功能的光辉而已。我可以肯定地告诉你，在真正的`slick`面前，上面说的这些缺点都不是缺点，只是`slick`的功能之一，`fsn`正是为了把这些功能挖掘出来的一个框架。但`fsn`不单单只是一个针对`slick`做扩展的框架，他是一个数据转换的抽象，可以在不同的数据源和数据目标间构建数据通道，实现数据在不同领域间的转换。

&emsp;&emsp;好了，说了这么多废话，有兴趣的话进入[第二章-初探（一）](doc-02.md)吧。