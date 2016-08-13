> C节点使用Erlang提供的 Erl_Interface 与 Erlang VM进行交互, 因此需要在C文件中包含头文件:

> `#include "erl_interface.h"`

从Erlang的角度看, C节点就像一个普通的Erlang节点一样. 调用C节点中的`foo`和`bar`函数就是给C节点发送消息, 并接收执行结果. 发送消息要求指定一个接收者, 接收者是一个Erlang进程ID, 或由一个元组(`{RegName, Node}`)表示的进程.

如果不知道PID, 那么可以通过下面的方式给接收者发行消息.

```
{RegName, Node} ! Msg
```

节点名称 `Node` 为C节点的名称. 如果节点使用短名称, 必须遵循 `cN`这种命名模式, `N`为整数

## C端

在调用 `Erl_Interface`接口中的其他函数前, 需要初始化内存.

```
erl_init(NULL, 0);
```

现在就可以初始化C节点了. 如果使用短节点名称, 通过调用 `erl_connect_init()` 完成节点的初始化:

```
erl_connect_init(1, "secretcookie", 0);
```

其中:

- 第一个参数为整数, 用于构造节点名称, 此例中节点名称为 `c1`
- 第二个参数为字符串, 用于设置Cookie的值
- 第三个参数为一个整数, 用于标识一个C节点实例.

如果使用长名称, 需要调用 `erl_connect_xinit()` 进行初始化, 而不是 `erl_connect_init()`:

```
erl_connect_xinit(
    "idril", "cnode",    "cnode@idril.du.uab.ericsson.se", &addr,  "secretcookie", 0
);
----------------------------------------------------------------------------------------------
    主机名称  本地节点名称  全称                               地址指针  Cookie值        实例编号
```

其中:

- 第一个参数为主机名称
- 第二个参数为节点`本地名称`(不包含域名部分)
- 第三个参数为节点的`全称`
- 第四个参数为一个指针, 指向一个包含该主机IP地址的 `in_addr` 结构.
- 第五个参数为Cookie的值
- 第六个参数为实例编号

C节点在设置Erlang和C之间的通信的时候, 既可以作为服务器端, 也可以作为客户端. 如果作为客户端, 需要通过调用 `erl_connect()` 连接到Erlang节点, 成功连接后, 返回一个打开的文件描述符:

```
fd = erl_connect("e1@localhost");
```

如果C端作为一个服务器运行, 它必须首先创建一个套接字(调用`bind()`和`listen()`)来监听特定的端口. 然后把名称和端口发布到`epmd`(Erlang端口映射守护进程), 详细信息请参考 [手册](http://erlang.org/doc/man/epmd.html).

```
erl_publish(port);
```

现在C节点服务器可以接受来自Erlang节点的连接了.

```
fd = erl_accept(listen, &conn);
```

`erl_accept` 的第二个参数为一个 `ErlConnect` 结构, 包含连接相关的信息. 例如, Erlang节点的名字.

## 收发消息

C节点可以调用 `erl_receive_msg()` 接收来自 Erlang节点的消息. 该函数从一个打开的文件描述符`fd`中读取数据, 并复制到一个缓冲区(Buffer)中, 接收的消息被存放在名为 `ErlMessage` 的结构 `emsg` 中. ErlMessage 的 `type` 字段表明接收的消息的类型. `ERL_REG_SEND` 指出, Erlang发送了一条消息到C节点中的一个已注册进程. 实际的消息是一个 `ETERM`, 在 `ErlMessage`结构的 `msg` 字段中.

节点事件

- ERL_ERROR 发生了错误
- ERL_TICK  节点心跳
- link
- unlink
- exit

节点心跳事件(`ERL_TICK`)应该被忽略或输出到日志, 错误事件应该被处理

```
 while (loop) {

    got = erl_receive_msg(fd, buf, BUFSIZE, &emsg);
    if (got == ERL_TICK) {
      /* ignore */
    } else if (got == ERL_ERROR) {
      loop = 0; /* exit while loop */
    } else {
      if (emsg.type == ERL_REG_SEND) {
```

因为消息是一个 ETERM 结构, Erl_Interface 接口中的函数操作. 在这个例子中, 消息体为一个三元组,第二个元素为调用者的pid,第三个元素为元组 `{Function,Arg}`, 用于决定要调用的函数. 函数的执行结果被封装成一个 ETERM 结构并调用 `erl_send()` 函数返回给调用者, 它接受三个参数, 分别是: 文件描述符, Pid, 以及一个项式:

```
    fromp = erl_element(2, emsg.msg);
    tuplep = erl_element(3, emsg.msg);
    fnp = erl_element(1, tuplep);
    argp = erl_element(2, tuplep);

    if (strncmp(ERL_ATOM_PTR(fnp), "foo", 3) == 0) {
      res = foo(ERL_INT_VALUE(argp));
    } else if (strncmp(ERL_ATOM_PTR(fnp), "bar", 3) == 0) {
      res = bar(ERL_INT_VALUE(argp));
    }

    resp = erl_format("{cnode, ~i}", res);
    erl_send(fd, fromp, resp);
```

最后, 通过 ETERM 创建函数函数分配的内存必须被释放(包括通过`erl_receive_msg()`函数创建的)


```
    erl_free_term(emsg.from); erl_free_term(emsg.msg);
    erl_free_term(fromp); erl_free_term(tuplep);
    erl_free_term(fnp); erl_free_term(argp);
    erl_free_term(resp);
```

下面是一个使用短节点名称的C节点服务器实现

```
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "erl_interface.h"
#include "ei.h"
#include "complex.h"
#include "listen.h"

#define BUFSIZE 1000

int main(int argc, char **argv) {
  int port;                                /* Listen port number */
  int listen;                              /* Listen socket */
  int fd;                                  /* fd to Erlang node */
  ErlConnect conn;                         /* Connection data */

  int loop = 1;                            /* Loop flag */
  int got;                                 /* Result of receive */
  unsigned char buf[BUFSIZE];              /* Buffer for incoming message */
  ErlMessage emsg;                         /* Incoming message */

  ETERM *fromp, *tuplep, *fnp, *argp, *resp;
  int res;

  port = atoi(argv[1]);

  erl_init(NULL, 0);

  if (erl_connect_init(1, "secretcookie", 0) == -1)
    erl_err_quit("erl_connect_init");

  /* Make a listen socket */
  if ((listen = my_listen(port)) <= 0)
    erl_err_quit("my_listen");

  // Publish to epmd
  if (erl_publish(port) == -1){
    erl_err_quit("erl_publish");
  }

  if ((fd = erl_accept(listen, &conn)) == ERL_ERROR){
    erl_err_quit("erl_accept");
  }
  fprintf(stderr, "Connected to %s\n\r", conn.nodename);

  while (loop) {

    got = erl_receive_msg(fd, buf, BUFSIZE, &emsg);
    if (got == ERL_TICK) {
      /* ignore */
    } else if (got == ERL_ERROR) {
      loop = 0;
    } else {

      if (emsg.type == ERL_REG_SEND) {
        fromp = erl_element(2, emsg.msg);
        tuplep = erl_element(3, emsg.msg);
        fnp = erl_element(1, tuplep);
        argp = erl_element(2, tuplep);

        if (strncmp(ERL_ATOM_PTR(fnp), "foo", 3) == 0) {
          res = foo(ERL_INT_VALUE(argp));
        } else if (strncmp(ERL_ATOM_PTR(fnp), "bar", 3) == 0) {
          res = bar(ERL_INT_VALUE(argp));
        }

        resp = erl_format("{cnode, ~i}", res);
        erl_send(fd, fromp, resp);

        erl_free_term(emsg.from); erl_free_term(emsg.msg);
        erl_free_term(fromp); erl_free_term(tuplep);
        erl_free_term(fnp); erl_free_term(argp);
        erl_free_term(resp);
      }
    }
  } /* while */
}


int my_listen(int port) {
  int listen_fd;
  struct sockaddr_in addr;
  int on = 1;

  if ((listen_fd = socket(AF_INET, SOCK_STREAM, 0)) < 0){
    return (-1);
  }

  setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));

  memset((void*) &addr, 0, (size_t) sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_port = htons(port);
  addr.sin_addr.s_addr = htonl(INADDR_ANY);

  if (bind(listen_fd, (struct sockaddr*) &addr, sizeof(addr)) < 0){
    return (-1);
  }else{
    printf("server is listen on: %d\n", port);
  }

  listen(listen_fd, 5);
  return listen_fd;
}
```

下面是一个使用长节点名的C节点服务器实现


```
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "erl_interface.h"
#include "ei.h"
#include "complex.h"
#include "listen.h"

#define BUFSIZE 1000

int main(int argc, char **argv) {
  struct in_addr addr;                     /* 32-bit IP number of host */
  int port;                                /* Listen port number */
  int listen;                              /* Listen socket */
  int fd;                                  /* fd to Erlang node */
  ErlConnect conn;                         /* Connection data */

  int loop = 1;                            /* Loop flag */
  int got;                                 /* Result of receive */
  unsigned char buf[BUFSIZE];              /* Buffer for incoming message */
  ErlMessage emsg;                         /* Incoming message */

  ETERM *fromp, *tuplep, *fnp, *argp, *resp;
  int res;

  port = atoi(argv[1]);

  erl_init(NULL, 0);

  addr.s_addr = inet_addr("134.138.177.89");
  if (erl_connect_xinit("idril", "cnode", "cnode@idril.du.uab.ericsson.se",
      &addr, "secretcookie", 0) == -1)
    erl_err_quit("erl_connect_xinit");

  /* Make a listen socket */
  if ((listen = my_listen(port)) <= 0)
    erl_err_quit("my_listen");

  if (erl_publish(port) == -1)
    erl_err_quit("erl_publish");

  if ((fd = erl_accept(listen, &conn)) == ERL_ERROR)
    erl_err_quit("erl_accept");
  fprintf(stderr, "Connected to %s\n\r", conn.nodename);

  while (loop) {

    got = erl_receive_msg(fd, buf, BUFSIZE, &emsg);
    if (got == ERL_TICK) {
      /* ignore */
    } else if (got == ERL_ERROR) {
      loop = 0;
    } else {

      if (emsg.type == ERL_REG_SEND) {
        fromp = erl_element(2, emsg.msg);
        tuplep = erl_element(3, emsg.msg);
        fnp = erl_element(1, tuplep);
        argp = erl_element(2, tuplep);

        if (strncmp(ERL_ATOM_PTR(fnp), "foo", 3) == 0) {
          res = foo(ERL_INT_VALUE(argp));
        } else if (strncmp(ERL_ATOM_PTR(fnp), "bar", 3) == 0) {
          res = bar(ERL_INT_VALUE(argp));
        }

        resp = erl_format("{cnode, ~i}", res);
        erl_send(fd, fromp, resp);

        erl_free_term(emsg.from); erl_free_term(emsg.msg);
        erl_free_term(fromp); erl_free_term(tuplep);
        erl_free_term(fnp); erl_free_term(argp);
        erl_free_term(resp);
      }
    }
  }
}


int my_listen(int port) {
  int listen_fd;
  struct sockaddr_in addr;
  int on = 1;

  if ((listen_fd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    return (-1);

  setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));

  memset((void*) &addr, 0, (size_t) sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_port = htons(port);
  addr.sin_addr.s_addr = htonl(INADDR_ANY);

  if (bind(listen_fd, (struct sockaddr*) &addr, sizeof(addr)) < 0)
    return (-1);

  listen(listen_fd, 5);
  return listen_fd;
}

```

最后是C节点客户端代码实现

```
#include <stdio.h>
#include <string.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "erl_interface.h"
#include "ei.h"
#include "complex.h"

#define BUFSIZE 1000

int main(int argc, char **argv) {
  int fd;                                  /* fd to Erlang node */
  int loop = 1;                            /* Loop flag */
  int got;                                 /* Result of receive */
  unsigned char buf[BUFSIZE];              /* Buffer for incoming message */
  ErlMessage emsg;                         /* Incoming message */
  ETERM *fromp, *tuplep, *fnp, *argp, *resp;
  int res;
  erl_init(NULL, 0);
  if (erl_connect_init(1, "secretcookie", 0) == -1){
    erl_err_quit("erl_connect_init");
  }
  if ((fd = erl_connect("e1@localhost")) < 0){
    erl_err_quit("erl_connect");
  }
  fprintf(stderr, "Connected to e1@localhost\n\r");
  while (loop) {
    got = erl_receive_msg(fd, buf, BUFSIZE, &emsg);
    if (got == ERL_TICK) {
      /* ignore */
    } else if (got == ERL_ERROR) {
      loop = 0;
    } else {
      if (emsg.type == ERL_REG_SEND) {
        fromp  = erl_element(2, emsg.msg);
        tuplep = erl_element(3, emsg.msg);
        fnp    = erl_element(1, tuplep);
        argp   = erl_element(2, tuplep);
        if (strncmp(ERL_ATOM_PTR(fnp), "foo", 3) == 0) {
          res = foo(ERL_INT_VALUE(argp));
        } else if (strncmp(ERL_ATOM_PTR(fnp), "bar", 3) == 0) {
          res = bar(ERL_INT_VALUE(argp));
        }
        resp = erl_format("{cnode, ~i}", res);
        erl_send(fd, fromp, resp);
        erl_free_term(emsg.from); erl_free_term(emsg.msg);
        erl_free_term(fromp); erl_free_term(tuplep);
        erl_free_term(fnp); erl_free_term(argp);
        erl_free_term(resp);
      }
    }
  }
}
```

> **本文中的源码经过修改, 遵循C99标准.**

## 运行这个例子

![图片描述][1]

下面是两个节点启动后, EPMD的注册名称

![图片描述][2]


> 启动服务器(短名称)

```
./bin/c_node_server 3456
```

> 启动Erlang节点

```
# 进入src目录

cd src

# 编译

erlc *.erl

# 启动节点并调用C节点的函数

➜  src erl -sname e1 -setcookie secretcookie

Eshell V7.3  (abort with ^G)
(e1@localhost)1> c_node_short:bar(4).
Result: 8
ok
(e1@localhost)2> c_node_short:bar(5).
Result: 10
ok
(e1@localhost)3>
```

## 代码库

```
git clone https://github.com/developerworks/c_node.git
```


  [1]: https://segmentfault.com/img/bVAqti
  [2]: https://segmentfault.com/img/bVAq3i
