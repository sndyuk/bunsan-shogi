package jp.sndyuk.shogi.algorithm.core

import com.orientechnologies.orient.server.OServerMain
import com.orientechnologies.orient.`object`.db.OObjectDatabaseTx
import com.orientechnologies.orient.core.db.document.ODatabaseDocumentTx
import com.orientechnologies.orient.core.record.impl.ODocument
import com.orientechnologies.orient.core.sql.query.OSQLSynchQuery
import com.orientechnologies.orient.core.sql.query.OResultSet
import com.orientechnologies.orient.core.record.ORecord
import com.orientechnologies.orient.core.record.ORecordStringable
import com.orientechnologies.orient.core.record.impl.ORecordBytes
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import com.orientechnologies.orient.core.metadata.schema.OClass

class HashStorage[K, V] {
  val db: ODatabaseDocumentTx = new ODatabaseDocumentTx("remote:localhost/databases/shogi_v2").open("root", "1234")
  val docName = "Board"
  val queryFindSingleResult: OSQLSynchQuery[ODocument] = new OSQLSynchQuery[ODocument](s"select v from $docName where k = ? limit 1")

  private[core] def onStart(): Unit = {
    val schema = db.getMetadata().getSchema().getClass(docName)
    if (!schema.areIndexed("k")) {
      schema.createIndex("k", OClass.INDEX_TYPE.UNIQUE_HASH_INDEX)
    }
  }

  def close(): Unit = {
    db.close()
  }
  sys.ShutdownHookThread {
    close()
  }

  def +=(kv: (K, V)) = {
    if (!contains(kv._1)) {
      val doc = new ODocument(docName)
      doc.field("k", kv._1)
      doc.field("v", kv._2)
      doc.save()
    }
  }

  def contains(key: K): Boolean = {
    val result: OResultSet[ODocument] = db.command(queryFindSingleResult).execute(key.asInstanceOf[Object])
    result.iterator().hasNext()
  }

  def apply(key: K): Option[V] = {
    val result: OResultSet[ODocument] = db.command(queryFindSingleResult).execute(key.asInstanceOf[Object])
    val iter = result.iterator()
    if (iter.hasNext()) {
      val doc = iter.next()
      Some(doc.field("v"))
    } else {
      None
    }
  }
}

object HashStorage {

  def start(): Unit = {
    import ExecutionContext.Implicits.global
    val latch = new CountDownLatch(1)
    val f = Future {
      val server = OServerMain.create()
      server.startup("""
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
              path="plocal:./databases/shogi_v2"
              loaded-at-startup="true" />
        </storages>
      </orient-server>
    """)
      server.activate()

      val db = new HashStorage()
      db.onStart()
      db.close()

      latch.countDown()
      sys.ShutdownHookThread {
        server.shutdown()
      }
    }
    latch.await(30, TimeUnit.SECONDS)
  }
}
