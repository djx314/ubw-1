# 初探

## 概况

有了第一章的了解，现在可以初步介绍`fsn`的 API 了。

目前`fsn`分三层代码，底层代码、中层代码、表层代码。

`fsn`暂时不对这三层代码作支持，只提供数据转换的抽象和一些已经基本完善的样例代码，大家可以根据自己的业务逻辑编写属于自己的`fsn`。

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