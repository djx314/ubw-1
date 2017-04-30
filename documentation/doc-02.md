# 初探

## 概况

有了第一章的了解，现在可以初步介绍`fsn`的 API 了。

目前`fsn`的 API 分三层，分别是：底层代码、中层代码、表层代码。

`fsn`暂不对这三层代码作支持，只提供数据转换的抽象和一些已经基本完善的样例代码，大家可以根据自己的业务逻辑编写属于自己的`fsn`。

1. 底层代码需要由对涉及到的框架十分熟悉的程序员编写。实现对某一领域的数据读入和输出。

1. 中层代码是对各模块底层代码的连接，实现数据由一个领域到另一个领域的转换，并对外屏蔽数据内部结构。

1. 表层代码是业务逻辑层对中层代码的调用，主要是列信息的标记和数据的转换。

## 准备工作

本章主要介绍的是表层代码，在编写业务逻辑前我们需要做一些准备，由于项目目前还没处于 stable 阶段，很多底层代码 API 都没有经过完全的锤炼，目前一些脚手架代码还是不可省略的。

1. 混入`SlickCRUDImplicits`、`StrFSSelectAtomicHelper`、`Slick2JsonFsnImplicit`三个 trait。

`SlickCRUDImplicits`是对`Query`的一些扩展

`StrFSSelectAtomicHelper`是针对`slick`每一个查询列的扩展

`Slick2JsonFsnImplicit`是对查询结果的扩展

2. 引入所需模块的扩展方法，这里需要 4 个模块

```scala
implicit def fPilesOptionImplicit[D](path: FPathImpl[D]): FJsonAtomicHelper[D] with FStrSelectExtAtomicHelper[D] with FPropertyAtomicHelper[D] with FDefaultAtomicHelper[D] = {
  val path1 = path
  new FJsonAtomicHelper[D] with FStrSelectExtAtomicHelper[D] with FPropertyAtomicHelper[D] with FDefaultAtomicHelper[D] {
    override val path = path1
  }
}
```

3. 引入引用模块所对应库（框架）的 API。

由于`fsn`并没有对引用库的内部行为进行入侵，因此必须引入原库的 API

```scala
import io.circe.syntax._
import io.circe.generic.auto._
import slick.jdbc.H2Profile.api._
```

## 业务代码

1. 定义一个 fQuery

```scala
val fQuery = for {
  friend <- FriendTable.out
} yield {
  List(
    "id" ofPile friend.id.out.order.describe("自增主键").writeJ,
    "name" ofPile friend.name.out.orderTarget("nick").describe("昵称").writeJ,
    "nick" ofPile friend.nick.out.order.describe("昵称").writeJ,
    "ageOpt" ofPile friend.age.out.order.writeJ
  )
}
```

2. 选择渲染方式

```scala
val result1: JsonOut = fQuery.strResult
```

3. 传入 Json 友好的参数并获取结果

```scala
val view1: DBIO[JsonView] = result1.toView(SlickParam())
```

4. 打印结果

```scala
Await.result(Sample01.db.run {
  Sample01.initData
    .flatMap { _ =>
      view1.map { s =>
        prettyPrint(s)
      }
    }
}, duration.Duration.Inf)
```

## 效果

让我们看一下这个简单查询的输出：

json data:

```json
[
 { "id" : 1, "name" : "魔理沙", "nick" : "小莎莎", "ageOpt" : 2333 },
 { "id" : 2, "name" : "jilen", "nick" : "jilen 酱", "ageOpt" : 30 },
 { "id" : 3, "name" : "品神", "nick" : "kerr", "ageOpt" : 28 },
 { "id" : 4, "name" : "廖师虎", "nick" : "shihu", "ageOpt" : null }
]
```

properties:
```json
[
 { "property" : "id", "typeName" : "Long", "inRetrieve" : true, "canOrder" : true, "isDefaultDesc" : true, "describe" : "自增主键" },
 { "property" : "name", "typeName" : "java.lang.String", "inRetrieve" : true, "canOrder" : false, "isDefaultDesc" : true, "describe" : "昵称" },
 { "property" : "nick", "typeName" : "java.lang.String", "inRetrieve" : true, "canOrder" : true, "isDefaultDesc" : true, "describe" : "昵称" },
 { "property" : "ageOpt", "typeName" : "Int", "inRetrieve" : true, "canOrder" : true, "isDefaultDesc" : true, "describe" : null }
]
```

`JsonView`有 2 个字段，`properties`和`data`，properties 包含了`fQuery`声明的所有列的详细信息，包括 Json 键、类型名称、是否展示，能否排序、详细描述等。这些信息可以直接响应给前端，前端可以根据这些信息作出对应列的数据和操作的逻辑处理。只要稍加判断，即可轻松弥补[无法传递列信息](doc-01.md#3-无法传递列信息)的缺陷。

大家还留意到`ageOpt`这个属性么，它在数据库的字段名是`age`，但由于声明的时候做了映射，所以输出的 Json 中已经转化成`ageOpt`这个属性了，这个特性部分弥补了[DTO支持欠佳](doc-01.md#4-dto-支持欠佳)这个缺陷，至于`DTO`中数据值的转换再稍后会进行进一步说明。

现在我们稍微改动一下传入的参数（第 3 步），

```scala
val view2: DBIO[JsonView] = result1.toView(SlickParam(orders = List(ColumnOrder("name", true), ColumnOrder("id", false))))
```

根据传入参数的不同，日志的 sql 语句发生了变化：

```sql
 select "id", "name", "nick", "age" from "firend" order by "id" nulls last, "nick" desc nulls last
```

`id`字段根据根据传入参数进行了 asc 排序，`name`字段由于声明时做了`orderTarget`处理，排序逻辑自动跳转到`nick`字段并进行 desc 排序。`orderTarget`这个特性在前端处理一些排序逻辑的时候十分有用（例如有一个既有的 grid 组件，在展示年级成绩数据的时候，班级列有一个排序组件，现在需要当点击班级列使其降序排序的时候按各班级平均分降序排序）。至此，`fsn`在创作之初的需求[排序](doc-01.md#1-烦人的-sortby)和分页得以轻松解决，其中分页的实现较为复杂，有 drop、take、pageIndex、pageSize 四个属性其中三个可以为空，传入`SlickParam`后即可达到分页效果，不同的数据量和参数可以导致不同的 sql 查询策略，但分页是`slick`内置的功能，在此不做详述。