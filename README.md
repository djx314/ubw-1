# fsn
A data transform abstract.
#创作灵感

吾不知其名，强名曰道。夫道者：有清有浊，有动有静；天清地浊，天动地静；<br /> 降本流末，而生万物。清者，浊之源，动者，静之基；人能常清静，天地悉皆归。

吾不知其名，强名曰道<br />
我不知道这个库该叫什么名字,暂时叫她 fsn

夫道者<br />
fsn 是什么?

有清有浊，有动有静<br />
既有统一的抽象数据类型,又有不可预知的实际数据类型.既有 List 的动态行声明方式,又有静态的列声明方式.

天清地浊<br />
统一的数据抽象是为了可以在非逻辑层方便地传输数据,但每个抽象的实现都装载着具体的数据类型.

天动地静<br />
用 List 声明一行数据的结构操作起来十分灵活和动态,但每一列的声明都能享受静态类型的安全.

降本流末，而生万物<br />
每个处理都有一个 source 和 target,这个库就是为你写好了 source 和 target,只要你定义好中间的列,就可以方便地实现各种各样的功能,甚至不用写界面.

清者，浊之源<br />
统一的数据抽象是纷繁杂乱的实际数据的载体.

动者，静之基<br />
每一列的静态声明最后也是要汇入到每一行的 List 声明中.

人能常清静，天地悉皆归<br />
如果我们写底层实现的时候只需要处理统一的数据抽象(清)而不需要了解实际的数据类型(浊),只需要关注每一列的类型安全(静),不需要过多地理会每一行数据怎么动态地转换,那就是静态类型的根本了,业务逻辑将是一片坦途.