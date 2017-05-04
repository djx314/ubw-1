# 初探（二）

## 效果（二）

上一节我们已经看过一些初步的特性。现在让我们来探究一下`fsn`的一些高级特性吧，这些特性有部分尚未稳定（多为中层代码不稳定）。

现在我有一个需求，需要把`name`和`nick`合并显示，没有`age`信息的和`age`大于 200 岁的都不允许显示他的`nick`，只能显示`name`信息，

观察以下代码：

```scala
val fQuery = for {
  friend <- FriendTable.out
} yield {
  List(
    "id" ofPile friend.id.out.order.describe("自增主键").writeJ,
    (
      ("name" ofPile friend.name.out.orderTarget("nick").describe("昵称")) ::
      ("nick" ofPile friend.nick.out.order.describe("昵称")) ::
      ("age" ofPile friend.age.out) ::
      HNil
    ).poly(
        "name" ofPile FPathImpl.empty[String].writeJ
      ).transform {
          case Some(name) :: Some(nick) :: Some(Some(age)) :: HNil if age < 200 =>
            Option(s"$name-$nick")
          case Some(name) :: _ :: _ :: HNil =>
            Option(name)
          case _ =>
            None
        },
    "ageOpt" ofPile friend.age.out.writeJ
  )
}
```

注意这里的三个字段在 poly 后需要重新指定 Json 的字段名称，并且数据库排序规则仍然是依照`slick`声明段的代码，在生成`properties`信息时`fsn`会根据`slick`段的信息判定该列是否可以排序。

传入的 SlickParam 参数不变，让我们看看结果如何？

json data:
```json
[
 { "id" : 1, "name" : "魔理沙", "ageOpt" : 2333 },
 { "id" : 2, "name" : "jilen-jilen 酱", "ageOpt" : 30 },
 { "id" : 3, "name" : "品神-kerr", "ageOpt" : 28 },
 { "id" : 4, "name" : "廖师虎", "ageOpt" : null }
]
```

properties:
```json
[
 { "property" : "id", "typeName" : "Long", "inRetrieve" : true, "canOrder" : true, "isDefaultDesc" : true, "describe" : "自增主键" },
 { "property" : "name", "typeName" : "String", "inRetrieve" : true, "canOrder" : true, "isDefaultDesc" : true, "describe" : null },
 { "property" : "ageOpt", "typeName" : "Int", "inRetrieve" : true, "canOrder" : false, "isDefaultDesc" : true, "describe" : null }
]
```

可以看到`age`小于 200 的数据的`name`字段都已被类型安全地转换成的新`name`，而不符合要求的字段则只显示原来的`name`。在`properties`信息中，`name`字段也有正确地标示可以排序的信息。

在引入 poly 方法后，你会发现，[DTO 支持欠佳](doc01.md#4-dto-支持欠佳)的缺陷已经被完全弥补。poly 方法是`fsn` `core`模块提供的几个重要功能之一，使你在编写底层代码时无需理会表层代码中列与列的数据交互逻辑，并且提供了一个基于 HList 的类型安全的列间数据交互方式。

至于[多类视图](doc-01.md#5-多类视图)和[多端输入](doc-01.md#6-多端输入)也是`fsn`设计的最初目标之一，会连同[动态 filter](doc-01.md#6-动态-filter)在后面的文档中详细描述。

现在我们观察一下`slick`，上述样例代码中，`friend.name`的作用其实与`column[String]("name")`是一样的，于是最初的例子我们可以改成以下的方式：

```scala
val fQuery = for {
  friend <- SimpleTable.tq("friend").out
} yield {
  List(
    "id" ofPile friend.column[Long]("id").out.order.describe("自增主键").writeJ,
    "name" ofPile friend.column[String]("name").out.orderTarget("nick").describe("昵称").writeJ,
    "nick" ofPile friend.column[String]("nick").out.order.describe("昵称").writeJ,
    "ageOpt" ofPile friend.column[Option[Int]]("age").out.writeJ
  )
}
```

输出的结果与第一个例子一样。可以发现，这个`fQuery`所需要的信息并不是`slick`中的`table`，而是字段名称，一些描述性信息和字段类型，如果把类型用 case 等方式处理，你会发现，所有的信息都是 Json 友好的 String，Boolean 等类型。完全可以根据 Json 或者 XML 等易用于模块间交互数据描述方式生成一个 fQuery。[更动态的 sql 语句生成](doc-01.md#7-更动态的-sql-语句生成)这个问题似乎也不难解决，但实际上要花费的功夫会比想象中大。不过比起人工拼接 sql，处理数据库间方言的差异，以及渲染视图中各种类型不安全的代码来说，基于`slick`的解决方案实现起来明显要轻松得多。

另外我在底层样例代码中实现了一个可以使用`slick`的`groupBy`功能的`fQuery`，可以根据不同的参数动态进行多维度统计，再配合`slick`自身的数据库信息检索接口（可以参照`codegen`的示例代码）辅助生成上述例子所需要的字段信息（column name, nullable, scala type 等），用[slick-migration-api](https://github.com/nafg/slick-migration-api "slick-migration-api")修改数据库表结构。而传往前端的数据中，已经自带字段信息，只要加上部分样式信息，则可在 Excel，Echarts，Grid 等多种终端展示。并可轻易加上各类参数以过滤数据和排序，也可从多个数据源中获取数据以持久化。说了这么多，是不是有一种可以单枪匹马做出一个`BI`的错觉？

可以感觉到`slick`的魅力和威力所在了么？

## 你可以做得更好

前面说过暂不对所有底层、中层、表层代码作支持，一部分原因是代码还不稳定和成熟，而最大的原因是：`fsn`不是提供这些模块的框架，而是不同模块之间的粘合剂，每个团队都可以依照自己的需求用`fsn-core`写出自己的底层代码、中层代码，实现各种五花八门的、绚丽的效果，而不是为某一个功能的不完善而抱怨。

[总结](doc-04.md)