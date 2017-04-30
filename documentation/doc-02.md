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
implicit def fPilesOptionImplicit[D](path: FPathImpl[D]): FJsonAtomicHelper[D] with FSelectExtAtomicHelper[D] with FPropertyAtomicHelper[D] with FDefaultAtomicHelper[D] = {
  val path1 = path
  new FJsonAtomicHelper[D] with FSelectExtAtomicHelper[D] with FPropertyAtomicHelper[D] with FDefaultAtomicHelper[D] {
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

让我们看一下这个简单查询的输出：

```json
json data:
[
 { "id" : 1, "name" : "魔理沙", "nick" : "小莎莎", "ageOpt" : 2333 },
 { "id" : 2, "name" : "jilen", "nick" : "jilen 酱", "ageOpt" : 30 },
 { "id" : 3, "name" : "品神", "nick" : "kerr", "ageOpt" : 28 },
 { "id" : 4, "name" : "廖师虎", "nick" : "shihu", "ageOpt" : null }
]

properties:
[
 { "property" : "id", "typeName" : "Long", "inRetrieve" : true, "canOrder" : true, "isDefaultDesc" : true, "describe" : "自增主键" },
 { "property" : "name", "typeName" : "java.lang.String", "inRetrieve" : true, "canOrder" : false, "isDefaultDesc" : true, "describe" : "昵称" },
 { "property" : "nick", "typeName" : "java.lang.String", "inRetrieve" : true, "canOrder" : true, "isDefaultDesc" : true, "describe" : "昵称" },
 { "property" : "ageOpt", "typeName" : "Int", "inRetrieve" : true, "canOrder" : true, "isDefaultDesc" : true, "describe" : null }
]
```

`JsonView`有 2 个字段，`properties`和`data`，properties 包含了`fQuery`声明的所有列的详细信息，包括 Json 键、类型名称、是否展示，能否排序、详细描述等。这些信息可以直接响应给前端，前端可以根据这些信息作出对应列的数据和操作的逻辑处理。只要稍加判断，即可轻松弥补[无法传递列信息](../doc-01.md#3-无法传递列信息)的缺陷。