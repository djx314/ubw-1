# 初探（二）

## 效果（二）

上一节我们已经看过一些初步的特性。现在让我们来探究一下`fsn`的一些高级特性吧，这些特性有部分还不稳定（多为中层代码不稳定）。

现在我有一个需求，`name`和`nick`合并显示，没有`age`信息的和`age`大于 200 岁的都不允许显示他的`nick`，`person`项只显示`name`信息，

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
        "name" ofPath FPathImpl.empty[String].writeJ
      ).apply {
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

注意这里的三个字段在 poly 后需要重新指定 Json 的 key，并且数据库排序规则仍然是依照`slick`声明段的代码，在生成`properties`信息时`fsn`会根据`slick`段的信息判定该列是否可以排序。

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

可以看到`age`小于 200 数据的`name`字段都已经被类型安全地转换成对应的新`name`，而不符合要求的字段则只显示原来的`name`，在`properties`信息中，`name`字段也有正确地标示可以排序的信息。

在引入 poly 方法以后，你会发现，[DTO 支持欠佳](doc01.md#4-dto-支持欠佳)的缺陷已经完全被弥补。poly 方法是`fsn`的`core`模块提供的几个重要功能之一，帮助在你编写底层代码的时候无需理会表层代码中列与列的数据交互逻辑，并且基于 HList 提供了一个类型安全的列间数据交互方式。