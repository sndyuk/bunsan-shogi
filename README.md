# Scala Shogi Engine
Shogi library for Scala.

### Try sample applications

```sh
$ sbt "project sample" run
```

### Performance
Scala 2.12.6 / 2.9 GHz Intel Core i7

* 700K NPS / 1 thread
* 1,700K NPS / 2 threads
* 2,400K NPS / 4 threads

### Sbt
```scala
"com.sndyuk" %% "bunsan-shogi-core" % "0.1.1"
```

### Maven
```xml
<dependency>
  <groupId>com.sndyuk</groupId>
  <artifactId>bunsan-shogi-core_2.12</artifactId>
  <version>0.1.1</version>
</dependency>
```

### License
[Apache License, Version 2.0](LICENSE)
