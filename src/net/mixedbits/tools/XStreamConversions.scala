/* 
 * This file is downloaded from
 * 
 * https://code.google.com/p/mixedbits-webframework/source/browse/trunk/src/main/scala/net.mixedbits.tools/XStreamConversions.scala?r=270
 * 
 * and is licensed under the MIT License.
 */

package net.mixedbits.tools

import com.thoughtworks.xstream.converters._
import com.thoughtworks.xstream.converters.collections._
import com.thoughtworks.xstream._
import com.thoughtworks.xstream.mapper._
import com.thoughtworks.xstream.io._  
import scala.collection.mutable.ListBuffer
  
class XStreamListConverter( _mapper : Mapper ) extends AbstractCollectionConverter(_mapper) {
  def canConvert( clazz: Class[_]) = {
    // "::" is the name of the list class, also handle nil
    classOf[::[_]] == clazz || Nil.getClass == clazz  
  }  
    
  def marshal( value: Any, writer: HierarchicalStreamWriter, context: MarshallingContext) = {
    val list = value.asInstanceOf[List[_]]  
    for ( item <- list ) {
      writeItem(item, context, writer)  
    }  
  }
    
  def unmarshal( reader: HierarchicalStreamReader, context: UnmarshallingContext ) = {
    val list = new scala.collection.mutable.ListBuffer[Any]()
    while (reader.hasMoreChildren()) {
      reader.moveDown()
      val item = readItem(reader, context, list)
      list += item
      reader.moveUp()  
    }  
    list.toList
  }
}

class XStreamSeqConverter[T <: Seq[Any]](fromSeq: Seq[Any] => T)(implicit manifest:ClassManifest[T],_mapper:Mapper) extends AbstractCollectionConverter(_mapper) {
  val seqClass = manifest.erasure
  def canConvert( clazz: Class[_]) = {
    seqClass == clazz
  }  
    
  def marshal( value: Any, writer: HierarchicalStreamWriter, context: MarshallingContext) = {
    val list = value.asInstanceOf[Seq[_]]
    for ( item <- list ) {
      writeItem(item, context, writer)  
    }
  }
    
  def unmarshal( reader: HierarchicalStreamReader, context: UnmarshallingContext ) = {
    val list = new ListBuffer[Any]()
    while (reader.hasMoreChildren()) {
      reader.moveDown()
      val item = readItem(reader, context, list)
      list += item
      reader.moveUp()  
    }
    fromSeq(list)
  }
}

class XStreamTupleConverter( _mapper : Mapper ) extends AbstractCollectionConverter(_mapper) {
  
  def canConvert( clazz: Class[_]) = {
    clazz.getName.startsWith("scala.Tuple")
  }
  
  def marshal( value: Any, writer: HierarchicalStreamWriter, context: MarshallingContext) = {
    val product = value.asInstanceOf[Product]
    for ( item <- product.productIterator ) {
      writeItem(item, context, writer)  
    }
  }
    
  def unmarshal( reader: HierarchicalStreamReader, context: UnmarshallingContext ) = {
    val list = new scala.collection.mutable.ListBuffer[AnyRef]()
    while (reader.hasMoreChildren()) {
      reader.moveDown()
      val item = readItem(reader, context, list)
      list += item
      reader.moveUp()
    }
    constructors(list.size).newInstance(list:_*)
  }
  
  val constructors = 0 to 22 map {
    case 0 => null
    case i => Class.forName("scala.Tuple"+i).getConstructors.head.asInstanceOf[java.lang.reflect.Constructor[AnyRef]]
  }
}


class XStreamSymbolConverter extends SingleValueConverter {
  def toString(value:Any) = 
    value.asInstanceOf[Symbol].name

  def fromString(name:String) = 
    Symbol(name)

  def canConvert(clazz:Class[_]) = 
    classOf[Symbol] == clazz
}
  
object XStreamConversions {
  private lazy val jsonConverter = XStreamConversions(new XStream(new com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver()))
  def toJson(item:AnyRef) = jsonConverter.toXML(item)

  private lazy val xmlConverter = XStreamConversions(new XStream())
  def toXml(item:AnyRef) = xmlConverter.toXML(item)
  
  def apply( stream: XStream ):XStream = {
    implicit val mapper = stream.getMapper
    
    //list
    stream.alias("list", classOf[::[_]])
    stream.alias("list", Nil.getClass)
    stream.registerConverter( new XStreamListConverter(stream.getMapper) )
    
    //tuples
    for(i <- 1 to 22) stream.alias("tuple",Class.forName("scala.Tuple"+i))  
    stream.registerConverter( new XStreamTupleConverter(stream.getMapper) )
  
    //symbols
    stream.alias("symbol", classOf[Symbol])
    stream.registerConverter( new XStreamSymbolConverter() )

    //various seq implementations    
    stream.alias("arrayBuffer",classOf[scala.collection.mutable.ArrayBuffer[_]])
    stream.registerConverter(new XStreamSeqConverter[scala.collection.mutable.ArrayBuffer[Any]](x => new scala.collection.mutable.ArrayBuffer[Any] ++= x))
    
    stream.alias("listBuffer",classOf[scala.collection.mutable.ListBuffer[_]])
    stream.registerConverter(new XStreamSeqConverter[scala.collection.mutable.ListBuffer[Any]](x => new scala.collection.mutable.ListBuffer[Any] ++= x))
    
    stream
  }
  
  def test(){
    implicit val stream = XStreamConversions(new XStream)
    testType(new scala.collection.mutable.ArrayBuffer[Int] ++= List(1,2,3))
    testType(new scala.collection.mutable.ListBuffer[Int] ++= List(1,2,3))
  }
  
  def testType[T](x:T)(implicit stream:XStream,manifest:ClassManifest[T]){
    val xml = stream.toXML(x) 
    println("Test for type: "+manifest.erasure)
    println(xml)
    println(stream.fromXML(xml))
  }
}
