package jp.sndyuk.shogi.algorithm.core

import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.reflect.ClassTag

import com.orientechnologies.orient.core.db.document.ODatabaseDocumentTx
import com.orientechnologies.orient.core.db.record.OTrackedList
import com.orientechnologies.orient.core.metadata.schema.OClass
import com.orientechnologies.orient.core.metadata.schema.OType
import com.orientechnologies.orient.core.record.impl.ODocument
import com.orientechnologies.orient.core.sql.query.OResultSet
import com.orientechnologies.orient.core.sql.query.OSQLSynchQuery
import com.orientechnologies.orient.server.OServerMain
import scala.collection.JavaConverters._

import jp.sndyuk.shogi.core.config

class HashDocument[K, V](docName: String)(implicit kTag: ClassTag[K], vTag: ClassTag[V]) {
  import HashStorage._

  val db: ODatabaseDocumentTx = new ODatabaseDocumentTx(s"remote:localhost/databases/shogi_$version").open("root", "1234")
  val queryFind: OSQLSynchQuery[ODocument] = new OSQLSynchQuery[ODocument](s"select from $docName where k = ? limit 1")
  val array = vTag.runtimeClass.isArray()
  val componentType = vTag.runtimeClass.getComponentType

  def onStart(): Unit = {
    var schema = db.getMetadata().getSchema().getClass(docName)

    if (schema == null) {
      schema = db.getMetadata().getSchema().createClass(docName)
      schema.createProperty("k", OType.getTypeByClass(kTag.runtimeClass))
      schema.createProperty("v", OType.getTypeByClass(vTag.runtimeClass))
    }
    if (!schema.areIndexed("k")) {
      schema.createIndex(s"$docName-kIdx", OClass.INDEX_TYPE.UNIQUE_HASH_INDEX, "k")
    }
  }

  def close(): Unit = {
    db.close()
  }

  sys.ShutdownHookThread {
    close()
  }
  onStart()

  def put(k: K, v: V): Unit = {
    val iter = getResult(k).iterator()
    if (iter.hasNext()) {
      val doc = iter.next()
      doc.field("v", v)
      doc.save()
    } else {
      val doc = new ODocument(docName)
      doc.field("k", k)
      doc.field("v", v)
      doc.save()
    }
  }

  def commit(): Unit = {
    db.commit()
  }

  private def getResult(key: K): OResultSet[ODocument] = db.query(queryFind, key.asInstanceOf[Object])

  def contains(key: K): Boolean = {
    getResult(key).iterator().hasNext()
  }

  def get(key: K): Option[V] = {
    val iter = getResult(key).iterator()
    if (iter.hasNext()) {
      val doc = iter.next().field[V]("v")
      if (array) {
        if (componentType == classOf[Long]) {
          Some(Array(doc.asInstanceOf[OTrackedList[Long]].asScala: _*).asInstanceOf[V])
        } else if (componentType == classOf[Int]) {
          Some(Array(doc.asInstanceOf[OTrackedList[Int]].asScala: _*).asInstanceOf[V])
        } else {
          Some(Array(doc.asInstanceOf[OTrackedList[Double]].asScala: _*).asInstanceOf[V])
        }
      } else Some(doc)
    } else {
      None
    }
  }
}

object HashStorage {
  val version = config.getString("shogi.struct-version")

  def start: Unit = {
    import ExecutionContext.Implicits.global
    val latch = new CountDownLatch(1)
    val f = Future {
      val server = OServerMain.create()
      server.startup(s"""
      <orient-server>
        <network>"
          <protocols>
            <protocol name="binary" implementation="com.orientechnologies.orient.server.network.protocol.binary.ONetworkProtocolBinary"/>
            <protocol name="http" implementation="com.orientechnologies.orient.server.network.protocol.http.ONetworkProtocolHttpDb"/>
          </protocols>"
          <listeners>"
            <listener ip-address="0.0.0.0" port-range="2424-2430" protocol="binary"/>
            <listener ip-address="0.0.0.0" port-range="2480-2490" protocol="http"/>
          </listeners>"
        </network>"
        <users>
          <user name="root" password="1234" resources="*"/>
        </users>"
        <properties>
          <entry name="server.cache.staticResources" value="false"/>
          <entry name="log.console.level" value="info"/>
          <entry name="plugin.dynamic" value="false"/>
        </properties>
        <storages>
          <storage
              name="shogi_board"
              path="plocal:./databases/shogi_$version"
              loaded-at-startup="true" />
        </storages>
      </orient-server>
    """)
      server.activate()

      latch.countDown()
      sys.ShutdownHookThread {
        server.shutdown()
      }
    }
    latch.await(30, TimeUnit.SECONDS)
    println()
  }
}
