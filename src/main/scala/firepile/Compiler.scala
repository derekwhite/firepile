package firepile

import firepile.util.BufferBackedArray._
import firepile.Marshaling._
import firepile.Spaces._
import firepile.tree.Trees._
import firepile.Implicits._

import scala.reflect.NamedType

import soot.{Type => SootType}

import com.nativelibs4java.opencl.CLMem
import com.nativelibs4java.opencl.CLKernel
import com.nativelibs4java.opencl.CLEvent
import com.nativelibs4java.opencl.CLByteBuffer

import compiler.JVM2Reflect.compileRoot
// import compiler.JVM2CL.compileMethod
import compiler.JVM2Reflect.mangleName
import compiler.JVM2Reflect.methodName

import java.util.ArrayList
import java.nio.ByteBuffer
import scala.collection.JavaConversions._

import scala.collection.mutable.ArrayBuffer
import firepile.Marshaling._

import com.nativelibs4java.opencl.JavaCL
import com.nativelibs4java.opencl.library.OpenCLLibrary
import com.nativelibs4java.opencl.library.OpenCLLibrary._
import com.nativelibs4java.util.JNAUtils.toNS
import com.sun.jna.Native
import com.ochafik.lang.jnaerator.runtime.NativeSize


// TODO: remove most of this.

object Compiler {
  var numIterations = 16

  def typeSig(t: java.lang.Class[_]): String = t match {
    case t if t == java.lang.Boolean.TYPE => "Z"
    case t if t == java.lang.Byte.TYPE => "B"
    case t if t == java.lang.Short.TYPE => "S"
    case t if t == java.lang.Character.TYPE => "C"
    case t if t == java.lang.Integer.TYPE => "I"
    case t if t == java.lang.Long.TYPE => "L"
    case t if t == java.lang.Float.TYPE => "F"
    case t if t == java.lang.Double.TYPE => "D"
    case t if t == java.lang.Void.TYPE => "V"
    case t if t.isArray => "[" + typeSig(t.getComponentType)
    case t => "L" + t.getName.replace('.', '/') + ";"
  }

  def signature(m: java.lang.reflect.Method) =
    m.getName + "(" + m.getParameterTypes.toList.map(t => typeSig(t)).mkString("") + ")" + typeSig(m.getReturnType)

  def findAllMethods(src: AnyRef, arity: Int, argMarshals: List[Marshal[_]], dev: Device): Option[(String, List[Tree])] = {

    val gMethod = findGlobalMethod(src.getClass.getName, arity)

    gMethod match {

      case Some(x: java.lang.reflect.Method) => {
        Some((methodName(x), compileRoot(src.getClass.getName, src, Compiler.signature(x), argMarshals, dev).reverse))
      }
      case None => { println(" Not able to find the method with Global variables !!!"); return None }
    }

  }

  def findGlobalMethod(cname1: String, arity: Int): Option[java.lang.reflect.Method] = {
    val k1 = Class.forName(cname1)
    for (m <- k1.getDeclaredMethods) {
      if (m.getReturnType.getName.startsWith("scala.Tuple" + arity)) {  // Should no longer be needed
        return Some(m)
      }
    }
    //println("findGlobalMethod end")
    None
  }

  def findLocalMethod(c1: String): Option[(java.lang.reflect.Method, String)] = {
    var i = 1
    while (true) {
      try {
        val cname = c1 + "$$anonfun$apply$" + i.toString
        val k2 = Class.forName(cname)
        for (m <- k2.getDeclaredMethods) {
          //println(" m.getName::" + m.getName + "  :: arity::" + m.getParameterTypes.length)
          val pars = m.getParameterTypes
          if (pars.length > 0)
            if (pars(0).getName.startsWith("firepile.Group"))
              return Some((m, cname))
        }
      } catch {
        case e: ClassNotFoundException => { if (i >= 100) return None }
        case e: SecurityException => { if (i >= 100) return None }

      }
      i += 1
    }

    None
  }

  def findKernelMethod(c2: String): Option[(java.lang.reflect.Method, String)] = {
    println("==========\nfindKernelMethod start\n==========")
    //println(" Generating Kernel Code ::")
    var i = 1
    while (true) {
      try {

        val cname = c2 + "$$anonfun$apply$" + i.toString
        val k3 = Class.forName(cname)
        for (m <- k3.getDeclaredMethods) {
          //println(" m.getName::" + m.getName + "  :: arity::" + m.getParameterTypes.length)
          val pars = m.getParameterTypes
          if (pars.length > 0)
            if (pars(0).getName.startsWith("firepile.Item"))
              return Some((m, cname))
        }

      } catch {
        case e: ClassNotFoundException => { if (i >= 100) return None }
        case e: SecurityException => { if (i >= 100) return None }

      }
      i += 1
    }

    //println("findKernelMethod end")
    None
  }

  def compileNew[A1, A2](tuple: Tuple2[A1, A2], kernName: String, tree: String, dev: Device)(implicit ma1: Marshal[A1], ma2: Marshal[A2]) = {
    val marshalInfo = new ArrayList[(Marshal[_], ByteBuffer, Int, Int, Int)]()

    val (a1, a2) = tuple

    a1 match {
        case a: BBArray[_] => marshalInfo.add((ma1, a.buffer, a.marshal.size * a.length, a.marshal.size, a.length))
        case a => marshalInfo.add((ma1, ma1.toBuffer(a1).head, ma1.sizes(a1).head, ma1.sizes(1).head, ma1.sizes(a1).head / ma1.sizes(1).head))
    }
    a2 match {
        case a: BBArray[_] => marshalInfo.add((ma2, a.buffer, a.marshal.size * a.length, a.marshal.size, a.length))
        case a => marshalInfo.add((ma2, ma2.toBuffer(a2).head, ma2.sizes(a2).head, ma2.sizes(1).head, ma2.sizes(a2).head / ma2.sizes(1).head))
    }

    compileN_BB((tuple.productIterator.toList zip marshalInfo.toList), kernName, tree, dev)
  }

  def compileNew[A1, A2, A3](tuple: Tuple3[A1, A2, A3], kernName: String, tree: String, dev: Device)(implicit ma1: Marshal[A1], ma2: Marshal[A2], ma3: Marshal[A3]) = {
    val marshalInfo = new ArrayList[(Marshal[_], ByteBuffer, Int, Int, Int)]()

    println("=======\ncompileNew: " + kernName)

    val (a1, a2, a3) = tuple

    a1 match {
        case a: BBArray[_] => marshalInfo.add((ma1, a.buffer, a.marshal.size * a.length, a.marshal.size, a.length))
        case a => marshalInfo.add((ma1, ma1.toBuffer(a1).head, ma1.sizes(a1).head, ma1.sizes(1).head, ma1.sizes(a1).head / ma1.sizes(1).head))
    }
    a2 match {
        case a: BBArray[_] => marshalInfo.add((ma2, a.buffer, a.marshal.size * a.length, a.marshal.size, a.length))
        case a => marshalInfo.add((ma2, ma2.toBuffer(a2).head, ma2.sizes(a2).head, ma2.sizes(1).head, ma2.sizes(a2).head / ma2.sizes(1).head))
    }
    a3 match {
        case a: BBArray[_] => marshalInfo.add((ma3, a.buffer, a.marshal.size * a.length, a.marshal.size, a.length))
        case a => marshalInfo.add((ma3, ma3.toBuffer(a3).head, ma3.sizes(a3).head, ma3.sizes(1).head, ma3.sizes(a3).head / ma3.sizes(1).head))
    }

    compileN_BB((tuple.productIterator.toList zip marshalInfo.toList), kernName, tree, dev)
  }

  def compileNew[A1, A2, A3, A4](tuple: Tuple4[A1, A2, A3, A4], kernName: String, tree: String, dev: Device)(implicit ma1: Marshal[A1], ma2: Marshal[A2], ma3: Marshal[A3], ma4: Marshal[A4]) = {
    val marshalInfo = new ArrayList[(Marshal[_], ByteBuffer, Int, Int, Int)]()

    val (a1, a2, a3, a4) = tuple

    a1 match {
        case a: BBArray[_] => marshalInfo.add((ma1, a.buffer, a.marshal.size * a.length, a.marshal.size, a.length))
        case a => marshalInfo.add((ma1, ma1.toBuffer(a1).head, ma1.sizes(a1).head, ma1.sizes(1).head, ma1.sizes(a1).head / ma1.sizes(1).head))
    }
    a2 match {
        case a: BBArray[_] => marshalInfo.add((ma2, a.buffer, a.marshal.size * a.length, a.marshal.size, a.length))
        case a => marshalInfo.add((ma2, ma2.toBuffer(a2).head, ma2.sizes(a2).head, ma2.sizes(1).head, ma2.sizes(a2).head / ma2.sizes(1).head))
    }
    a3 match {
        case a: BBArray[_] => marshalInfo.add((ma3, a.buffer, a.marshal.size * a.length, a.marshal.size, a.length))
        case a => marshalInfo.add((ma3, ma3.toBuffer(a3).head, ma3.sizes(a3).head, ma3.sizes(1).head, ma3.sizes(a3).head / ma3.sizes(1).head))
    }
    a4 match {
        case a: BBArray[_] => marshalInfo.add((ma4, a.buffer, a.marshal.size * a.length, a.marshal.size, a.length))
        case a => marshalInfo.add((ma4, ma4.toBuffer(a4).head, ma4.sizes(a4).head, ma4.sizes(1).head, ma4.sizes(a4).head / ma4.sizes(1).head))
    }

    compileN_BB((tuple.productIterator.toList zip marshalInfo.toList), kernName, tree, dev)
  }


  def compileNew[A1, A2, A3, A4, A5](tuple: Tuple5[A1, A2, A3, A4, A5], kernName: String, tree: String, dev: Device)(implicit ma1: Marshal[A1], ma2: Marshal[A2], ma3: Marshal[A3], ma4: Marshal[A4], ma5: Marshal[A5]) = {
    val marshalInfo = new ArrayList[(Marshal[_], ByteBuffer, Int, Int, Int)]()

    val (a1, a2, a3, a4, a5) = tuple

    a1 match {
        case a: BBArray[_] => marshalInfo.add((ma1, a.buffer, a.marshal.size * a.length, a.marshal.size, a.length))
        case a => marshalInfo.add((ma1, ma1.toBuffer(a1).head, ma1.sizes(a1).head, ma1.sizes(1).head, ma1.sizes(a1).head / ma1.sizes(1).head))
    }
    a2 match {
        case a: BBArray[_] => marshalInfo.add((ma2, a.buffer, a.marshal.size * a.length, a.marshal.size, a.length))
        case a => marshalInfo.add((ma2, ma2.toBuffer(a2).head, ma2.sizes(a2).head, ma2.sizes(1).head, ma2.sizes(a2).head / ma2.sizes(1).head))
    }
    a3 match {
        case a: BBArray[_] => marshalInfo.add((ma3, a.buffer, a.marshal.size * a.length, a.marshal.size, a.length))
        case a => marshalInfo.add((ma3, ma3.toBuffer(a3).head, ma3.sizes(a3).head, ma3.sizes(1).head, ma3.sizes(a3).head / ma3.sizes(1).head))
    }
    a4 match {
        case a: BBArray[_] => marshalInfo.add((ma4, a.buffer, a.marshal.size * a.length, a.marshal.size, a.length))
        case a => marshalInfo.add((ma4, ma4.toBuffer(a4).head, ma4.sizes(a4).head, ma4.sizes(1).head, ma4.sizes(a4).head / ma4.sizes(1).head))
    }
    a5 match {
        case a: BBArray[_] => marshalInfo.add((ma5, a.buffer, a.marshal.size * a.length, a.marshal.size, a.length))
        case a => marshalInfo.add((ma5, ma5.toBuffer(a5).head, ma5.sizes(a5).head, ma5.sizes(1).head, ma5.sizes(a5).head / ma5.sizes(1).head))
    }


    compileN_BB((tuple.productIterator.toList zip marshalInfo.toList), kernName, tree, dev)
  }


  def compileN_BB(tuple: List[(_,(Marshal[_], ByteBuffer, Int, Int, Int))], kernName: String, tree: String, dev: Device) = {
    val kernBin = firepile.gpu.buildProgramSrc(kernName, tree)

    val outputBuffers = new ArrayList[(CLByteBuffer, Int, Marshal[_], Int, Int)]()
    var maxInputItems: Int = 0
    var maxInputSize: Int = 0
    var maxOutputItems: Int = 0
    var maxOutputSize: Int = 0
    var numArrays: Int = 0

    for (i <- 0 until Kernel.globalArgs.size) {
      var output = false

      Kernel.globalArgs.get(i) match {
        case (name: String, typ: SootType, index: Int) => {
          println("Setting global arg " + i + " of type " + typ + " with name " + name)
          val (data, marshalInfo) = tuple(index)
		      if (data.isInstanceOf[BBArray[_]] && !marshalInfo._2.isDirect)
		          throw new RuntimeException("Use direct NIO buffers for better copy performance")

          for (j <- 0 until Kernel.outputArgs.size)
            if (name.startsWith(Kernel.outputArgs.get(j)))
              output = true

          if (output) {
            val clBuf = dev.context.createByteBuffer(CLMem.Usage.Output, marshalInfo._3)
            outputBuffers.add((clBuf, marshalInfo._3, marshalInfo._1, index, marshalInfo._5))
            val nItems = marshalInfo._5

            if (nItems > maxOutputItems) {
              maxOutputItems = nItems
              maxOutputSize = marshalInfo._4
            }

            firepile.compiler.JVM2Reflect.translateType(typ, index) match {
              case NamedType(typeName) if typeName.endsWith("Array") => {
                kernBin.setArg(i+numArrays, clBuf)
                numArrays += 1

                kernBin.setArg(i+numArrays, nItems)
              }
              case _ => {
                kernBin.setArg(i+numArrays, clBuf)
              }
            }

          } else {
            val nItems = marshalInfo._5

            if (nItems > maxInputItems) {
              maxInputItems = nItems
              maxInputSize = marshalInfo._4
            }

//            time({
              val copyToGPU = System.nanoTime


              firepile.tree.Reflect2CL.translateType(firepile.compiler.JVM2Reflect.translateType(typ, index)) match {
                case ValueType("int") => {
                  kernBin.setArg(i+numArrays, data.asInstanceOf[Int])
                }
                case ValueType("float") => kernBin.setArg(i+numArrays, data.asInstanceOf[Float])
                case ValueType("long") => kernBin.setArg(i+numArrays, data.asInstanceOf[Long])
                case ValueType("double") => kernBin.setArg(i+numArrays, data.asInstanceOf[Double])
                case ValueType(typName) if typName.endsWith("Array") => {
                  println("Setting array arg of type: " + typName)
                  kernBin.setArg(i+numArrays, dev.context.createBuffer(CLMem.Usage.Input, marshalInfo._2, true))
                  numArrays += 1
                  kernBin.setArg(i+numArrays, nItems)
                  /*
                  firepile.tree.Reflect2CL.translateType(typName.replace("Array", "")) match {
                  case "int" => {
                    kernBin.setArg(i+numArrays, dev.context.createBuffer(CLMem.Usage.Input, marshalInfo._2, true))
                    numArrays += 1
                    kernBin.setArg(i+numArrays, nItems)
                  }
                  case "float" => {
                    kernBin.setArg(i+numArrays, dev.context.createBuffer(CLMem.Usage.Input, marshalInfo._2, true))
                    numArrays += 1
                    kernBin.setArg(i+numArrays, nItems)
                  }
                  case "long" => {
                    kernBin.setArg(i+numArrays, dev.context.createBuffer(CLMem.Usage.Input, marshalInfo._2, true))
                    numArrays += 1
                    kernBin.setArg(i+numArrays, nItems)
                  }
                  case "double" => {
                    kernBin.setArg(i+numArrays, dev.context.createBuffer(CLMem.Usage.Input, marshalInfo._2, true))
                    numArrays += 1
                    kernBin.setArg(i+numArrays, nItems)
                  }
                  case t => throw new RuntimeException("Trying to setArg with unsupported array type: " + t + " arg number = " + (i+numArrays) + " with name " + name)
                }
                */
                }

                case x => {
                  throw new RuntimeException("Setting unknown arg type") // kernBin.setArg(i+numArrays, dev.context.createByteBuffer(CLMem.Usage.Input, marshalInfo._2, true))
                }

              }

//            }, "Copy to GPU")
              Kernel.setTime("Copy to GPU", (System.nanoTime-copyToGPU))
          }

          }
          case _ => {}
        }
     }

   //println(" output Buffer size::"+ outputBuffers.size)
   //println(" max Input size ::"+ maxInputSize + " max Input items ::" + maxInputItems)
   //println(" max Output size ::"+ maxOutputSize + " max Output items ::" + maxOutputItems)


    val threads = (if (maxInputItems < dev.maxThreads * 2) scala.math.pow(2, scala.math.ceil(scala.math.log(maxInputItems) / scala.math.log(2))) else dev.maxThreads).toInt

    // WARM UP

    // END WARM UP

//      time({
      // println("Number of kernel localArgs = " + Kernel.localArgs.size)  BUG HERE?

//        val gpuTime = System.nanoTime
//        var x = 0
//        while (x < numIterations) {

    // Set local arg (TODO: needs to be rewritten to support multiple local arguments)
    val (localArgPos, localArgSize, localArgLen, globalSize, localSize) = if (dev.memConfig == null && Kernel.localArgs.size > 0)
      (Kernel.globalArgs.size + numArrays, threads * maxOutputSize, threads, Array[Int](maxOutputItems * threads), Array[Int](threads))
    else
      (Kernel.globalArgs.size + numArrays, dev.memConfig.localMemSize * maxOutputSize, dev.memConfig.localMemSize, dev.memConfig.globalSize, dev.memConfig.localSize)

    kernBin.setLocalArg(localArgPos, localArgSize)
    kernBin.setArg(localArgPos + 1, localArgLen)

    // Set heap space args
    for ((localName, typ, pIndex, size, arrayLen) <- Kernel.heapArgs) {
      println("Setting heap parameter for: " + localName)

      val typeSize = 4       // TODO: Use a real calculation of typeSize

      // kernBin.setArg(localArgPos + 1 + pIndex, new NativeSize(typeSize * size * globalSize(0)))
      val heapSize = typeSize * size * globalSize(0)
      Kernel.heapMemory = heapSize
      Kernel.globalSize = globalSize(0)
      kernBin.setArg(localArgPos + 1 + pIndex, dev.context.createByteBuffer(CLMem.Usage.Input, heapSize))

    }

    // Execute the kernel
    kernBin.enqueueNDRange(dev.queue, globalSize, localSize)

/*
    if (dev.memConfig == null) {
      if (Kernel.localArgs.size > 0) {
        kernBin.setLocalArg(Kernel.globalArgs.size + numArrays, threads * maxOutputSize)
        kernBin.setArg(Kernel.globalArgs.size + numArrays + 1, threads)
      }
      kernBin.enqueueNDRange(dev.queue, Array[Int](maxOutputItems * threads), Array[Int](threads))

    } else {
      //println(" Setting default arguments ")
      if (Kernel.localArgs.size > 0) {
        kernBin.setLocalArg(Kernel.globalArgs.size + numArrays, dev.memConfig.localMemSize * maxOutputSize)
        kernBin.setArg(Kernel.globalArgs.size + numArrays + 1, dev.memConfig.localMemSize)
      }
      kernBin.enqueueNDRange(dev.queue, dev.memConfig.globalSize, dev.memConfig.localSize)
    }
*/
//         x += 1
//        }
//      }, "GPU", numIterations)

    dev.queue.finish
//      Kernel.setTime("GPU", (System.nanoTime - gpuTime)/numIterations)

//      time({

    val copyFromGPU = System.nanoTime
      // println("Number of output buffers: " + outputBuffers.size)
      for (i <- 0 until outputBuffers.size) {
        outputBuffers.get(i) match {
          case (clBuf: CLByteBuffer, totalSize : Int, marshal: Marshal[_], index: Int, items: Int) => {
            val (data, _) = tuple(index)
            // println("total size of output buffer: " + totalSize)
            data match {
              case d: BBArray[_] => {
                              val clOut = CLEvent.new_event_out(null)
                              val clArray = CLEvent.to_cl_event_array(null)
                              JavaCL.CL.clEnqueueReadBuffer(dev.queue.getEntity(),clBuf.getEntity(),CL_TRUE,toNS(0),toNS(totalSize),Native.getDirectBufferPointer(d.buffer),0 ,clArray,clOut)
                              //d.buffer = bufOut
                   }
              case _ => {
                  val bufOut = allocDirectBuffer(totalSize)
            clBuf.read(dev.queue, bufOut, true)
            bufOut.rewind
            Array.copy(bufOut, 0, data.asInstanceOf[AnyRef], 0, items)
                    Array.copy(marshal.fromBuffer(List(bufOut)), 0, data.asInstanceOf[AnyRef], 0, items)
        }
            }
          }
          case _ => {}
        }
      }
//      }, "From GPU")
    Kernel.setTime("From GPU", (System.nanoTime - copyFromGPU))

  }

  def compileN(tuple: List[(_,(Marshal[_], ByteBuffer, Int, Int, Int))], kernName: String, tree: String, dev: Device) = {
    val kernBin = firepile.gpu.buildProgramSrc(kernName, tree)

    val outputBuffers = new ArrayList[(CLByteBuffer, Int, Marshal[_], Int, Int)]()
    var maxInputItems: Int = 0
    var maxInputSize: Int = 0
    var maxOutputItems: Int = 0
    var maxOutputSize: Int = 0
    var numArrays: Int = 0

    for (i <- 0 until Kernel.globalArgs.size) {
      var output = false

      Kernel.globalArgs.get(i) match {
        case (name: String, typ: SootType, index: Int) => {
          val (data, marshalInfo) = tuple(index)
          for (j <- 0 until Kernel.outputArgs.size)
            if (name.startsWith(Kernel.outputArgs.get(j)))
              output = true

          if (output) {
            val clBuf = dev.context.createByteBuffer(CLMem.Usage.Output, marshalInfo._3)
            outputBuffers.add((clBuf, marshalInfo._3, marshalInfo._1, index, marshalInfo._5))
            val nItems = marshalInfo._5

            if (nItems > maxOutputItems) {
              maxOutputItems = nItems
              maxOutputSize = marshalInfo._4
            }

            firepile.compiler.JVM2CL.translateType(typ, index) match {
              case StructType(typeName) if typeName.endsWith("Array") => {
                kernBin.setArg(i+numArrays, clBuf)
                numArrays += 1

                kernBin.setArg(i+numArrays, nItems)
              }
              case _ => {
                kernBin.setArg(i+numArrays, clBuf)
              }
            }

          } else {
            val nItems = marshalInfo._5

            if (nItems > maxInputItems) {
              maxInputItems = nItems
              maxInputSize = marshalInfo._4
            }

            time({

              firepile.compiler.JVM2CL.translateType(typ, index) match {
                case ValueType("int") => {
                  kernBin.setArg(i+numArrays, data.asInstanceOf[Int])
                }
                case ValueType("float") => kernBin.setArg(i+numArrays, data.asInstanceOf[Float])
                case ValueType("long") => kernBin.setArg(i+numArrays, data.asInstanceOf[Long])
                case ValueType("double") => kernBin.setArg(i+numArrays, data.asInstanceOf[Double])
                case StructType(typName) => typName.replace("Array", "") match {
                  case "int" => {
                    kernBin.setArg(i+numArrays, data.asInstanceOf[Array[Int]])
                    numArrays += 1
                    kernBin.setArg(i+numArrays, nItems)
                  }
                  case "float" => {
                    kernBin.setArg(i+numArrays, dev.context.createBuffer(CLMem.Usage.Input, marshalInfo._2, true))
//                    println("First item of float buffer: " + marshalInfo.get(1)._2.asFloatBuffer.get(0).asInstanceOf[Float])
                    numArrays += 1
                    kernBin.setArg(i+numArrays, nItems)
                  }
                  case "long" => {
                    kernBin.setArg(i+numArrays, dev.context.createBuffer(CLMem.Usage.Input, marshalInfo._2, true))
                    numArrays += 1
                    kernBin.setArg(i+numArrays, nItems)
                  }
                  case "double" => {
                    kernBin.setArg(i+numArrays, dev.context.createBuffer(CLMem.Usage.Input, marshalInfo._2, true))
                    numArrays += 1
                    kernBin.setArg(i+numArrays, nItems)
                  }
                  case _ => throw new RuntimeException("Trying to setArg with unsupported array type")
                }

                case x => {
                  kernBin.setArg(i+numArrays, dev.context.createByteBuffer(CLMem.Usage.Input, marshalInfo._2, true))
                }

              }

            }, "Copy to GPU")
          }

          }
          case _ => {}
        }
      }

   //println(" output Buffer size::"+ outputBuffers.size)
   //println(" max Input size ::"+ maxInputSize + " max Input items ::" + maxInputItems)
   //println(" max Output size ::"+ maxOutputSize + " max Output items ::" + maxOutputItems)


      val threads = (if (maxInputItems < dev.maxThreads * 2) scala.math.pow(2, scala.math.ceil(scala.math.log(maxInputItems) / scala.math.log(2))) else dev.maxThreads).toInt

      time({
        // println("Number of kernel localArgs = " + Kernel.localArgs.size)  BUG HERE?

        if (dev.memConfig == null) {

          if (Kernel.localArgs.size > 0) {
            kernBin.setLocalArg(3 + numArrays, threads * maxOutputSize)
            kernBin.setArg(3 + numArrays + 1, threads)
          }
          kernBin.enqueueNDRange(dev.queue, Array[Int](maxOutputItems * threads), Array[Int](threads))

        } else {
          //println(" Setting default arguments ")
          if (Kernel.localArgs.size > 0) {
            kernBin.setLocalArg(3 + numArrays, dev.memConfig.localMemSize * maxOutputSize)
            kernBin.setArg(3 + numArrays + 1, dev.memConfig.localMemSize)
          }
          kernBin.enqueueNDRange(dev.queue, dev.memConfig.globalSize, dev.memConfig.localSize)
        }
        dev.queue.finish
      }, "GPU", numIterations)

      time({
        for (i <- 0 until outputBuffers.size) {
          outputBuffers.get(i) match {
            case (clBuf: CLByteBuffer, totalSize : Int, marshal: Marshal[_], index: Int, items: Int) => {
              val (data, _) = tuple(index)
              val bufOut = allocDirectBuffer(totalSize)
              clBuf.read(dev.queue, bufOut, true)
              bufOut.rewind
              Array.copy(marshal.fromBuffer(List(bufOut)), 0, data.asInstanceOf[AnyRef], 0, items)
              // Array.copy(marshal.fromBuffer(List(bufOut)), 0, get(tuple,index).asInstanceOf[AnyRef], 0, items)
            }
            case _ => {}
          }
        }
      }, "From GPU")

  }


  def compileNew[A1, A2, A3, A4](a: A1, b: A2, c: A3, d: A4, kernName: String, tree: String)(implicit ma1: Marshal[A1], ma2: Marshal[A2], ma3: Marshal[A3], ma4: Marshal[A4], dev: Device) = {

    val transA1 = implicitly[Marshal[A1]]
    val transA2 = implicitly[Marshal[A2]]
    val transA3 = implicitly[Marshal[A3]]
    val transA4 = implicitly[Marshal[A4]]
    val sizeA1 = transA1.sizes(1).head
    val sizeA2 = transA2.sizes(1).head
    val sizeA3 = transA3.sizes(1).head
    val sizeA4 = transA4.sizes(1).head

    val kernBin = firepile.gpu.buildProgramSrc(kernName, tree)

    var bufA1: ByteBuffer = transA1.toBuffer(a).head
    var bufA2: ByteBuffer = null
    var bufA3: ByteBuffer = null
    var bufA4: ByteBuffer = null

    var bufA1CLBuf: CLByteBuffer = null
    var bufA2CLBuf: CLByteBuffer = null
    var bufA3CLBuf: CLByteBuffer = null
    var bufA4CLBuf: CLByteBuffer = null

    time({
      bufA2 = transA2.toBuffer(b).head
      bufA3 = transA3.toBuffer(c).head
      bufA4 = transA4.toBuffer(d).head

      bufA2CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA2, true)
      bufA3CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA3, true)
      bufA4CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA4, true)

    }, "Copy to GPU")

    val numItemsA1 = transA1.sizes(a).head / sizeA1
    val numItemsA2 = transA2.sizes(b).head / sizeA2
    val numItemsA3 = transA3.sizes(c).head / sizeA3
    val numItemsA4 = transA4.sizes(d).head / sizeA4

    val bufA1capacity = transA1.sizes(a).head

    bufA1CLBuf = dev.context.createByteBuffer(CLMem.Usage.Output, bufA1capacity)

    // println("Output buffer capacity: " + bufA1capacity)

    val threads = (if (numItemsA2 < dev.maxThreads * 2) scala.math.pow(2, scala.math.ceil(scala.math.log(numItemsA2) / scala.math.log(2))) else dev.maxThreads).toInt

    // START TIMING CODE

    time({
      kernBin.setArg(0, bufA1CLBuf) // InvalidArgSize when passing straight ByteBuffer but ok with CLByteBuffer
      kernBin.setArg(1, bufA2CLBuf)
      kernBin.setArg(2, c.asInstanceOf[Int])
      kernBin.setArg(3, d.asInstanceOf[Int])

      //kernBin.setLocalArg(3, threads * sizeA1)

      if (dev.memConfig == null) {

        println(" Dev memConfig is null")
        // kernBin.setLocalArg(3, threads * sizeA1)
        kernBin.enqueueNDRange(dev.queue, Array[Int](numItemsA1 * threads), Array[Int](threads))
      } else {

        // println(" Setting default arguments ")
        // kernBin.setLocalArg(3, dev.memConfig.localMemSize * sizeA2)
        kernBin.enqueueNDRange(dev.queue, dev.memConfig.globalSize, dev.memConfig.localSize)
      }

      //kernBin.enqueueNDRange(dev.queue, Array[Int](threads * numItemsA1 ), Array[Int](threads))
      dev.queue.finish
    }, "GPU", numIterations)

    val bufOut = allocDirectBuffer(bufA1capacity)

    time({
      bufA1CLBuf.read(dev.queue, bufOut, true)

      bufOut.rewind

      // [NN] maybe need to copy?  but, probably not
      Array.copy(transA1.fromBuffer(List(bufOut)).asInstanceOf[AnyRef], 0, a.asInstanceOf[AnyRef], 0, numItemsA1)
    }, "From GPU")
    a
  }

  def compileNew[A1, A2, A3, A4, A5](a: A1, b: A2, c: A3, d: A4, e: A5, kernName: String, tree: String)(implicit ma1: Marshal[A1], ma2: Marshal[A2], ma3: Marshal[A3], ma4: Marshal[A4], ma5: Marshal[A5], dev: Device) = {
    val kernBin = firepile.gpu.buildProgramSrc(kernName, tree)

    a
  }

  def findApplyMethod(src: AnyRef, arity: Int): java.lang.reflect.Method = {
    // println(" Here::" + src)

    val cname = src.getClass.getName + "$$anonfun$apply$1"
    val k = Class.forName(cname)

    for (m <- k.getDeclaredMethods) {
      // println(" m.getName::" + m.getName + "  :: arity::" + m.getParameterTypes.length)
      if (m.getParameterTypes.length == arity)
        if (m.getName.startsWith("apply$mc") && m.getName.endsWith("$sp"))
          return m
    }

    for (m <- k.getDeclaredMethods) {
      if (m.getParameterTypes.length == arity)
        if (m.getName.equals("apply"))
          return m
    }
    throw new RuntimeException("Could not find apply/" + arity + " method in " + k.getName)
  }

  var next = 0
  def freshName(base: String = "tmp") = {
    next += 1
    base + next
  }

  trait Kernel
  trait Kernel1[A] extends Function1[A, Unit] with Kernel
  trait Kernel2[A1, A2] extends Function2[A1, A2, Unit] with Kernel
  trait Kernel3[A1, A2, A3] extends Function3[A1, A2, A3, Unit] with Kernel
  trait Kernel4[A1, A2, A3, A4] extends Function4[A1, A2, A3, A4, Unit] with Kernel
  trait Kernel5[A1, A2, A3, A4, A5] extends Function5[A1, A2, A3, A4, A5, Unit] with Kernel
  trait Kernel6[A1, A2, A3, A4, A5, A6] extends Function6[A1, A2, A3, A4, A5, A6, Unit] with Kernel
  trait Kernel7[A1, A2, A3, A4, A5, A6, A7] extends Function7[A1, A2, A3, A4, A5, A6, A7, Unit] with Kernel

  def compile[A](f: A => Unit)(implicit ma: Marshal[A], dev: Device): Kernel1[A] = throw new RuntimeException("unimplemented")
  // e.g., reduce(input: Array[Int], output: Array[Int])
  // e.g., map(input: Array[Int], output: Array[Float])

  import scala.collection.mutable.HashMap
  val kernelCache = new HashMap[AnyRef, Kernel]
  // [NN] move to Device?
  def memoize[A1, A2](f: (A1, A2) => Unit)(k: => Kernel2[A1, A2]) = {
    val key = f.getClass
    kernelCache.get(key) match {
      case None =>
        val kCompiled = k
        kernelCache(key) = kCompiled
        kCompiled
      case Some(k2: Kernel2[A1, A2]) =>
        println("found kernel in cache")
        k2
    }
  }

  def memoize[A1, A2, A3](f: (A1, A2, A3) => Unit)(k: => Kernel3[A1, A2, A3]) = {
    val key = f.getClass
    kernelCache.get(key) match {
      case None =>
        val kCompiled = k
        kernelCache(key) = kCompiled
        kCompiled
      case Some(k2: Kernel3[A1, A2, A3]) =>
        println("found kernel in cache")
        k2
    }
  }

  def memoize[A1, A2, A3, A4](f: (A1, A2, A3, A4) => Unit)(k: => Kernel4[A1, A2, A3, A4]) = {
    val key = f.getClass
    kernelCache.get(key) match {
      case None =>
        val kCompiled = k
        kernelCache(key) = kCompiled
        kCompiled
      case Some(k2: Kernel4[A1, A2, A3, A4]) =>
        println("found kernel in cache")
        k2
    }
  }

  def memoize[A1, A2, A3, A4, A5](f: (A1, A2, A3, A4, A5) => Unit)(k: => Kernel5[A1, A2, A3, A4, A5]) = {
    val key = f.getClass
    kernelCache.get(key) match {
      case None =>
        val kCompiled = k
        kernelCache(key) = kCompiled
        kCompiled
      case Some(k2: Kernel5[A1, A2, A3, A4, A5]) =>
        println("found kernel in cache")
        k2
    }
  }

  def memoize[A1, A2, A3, A4, A5, A6](f: (A1, A2, A3, A4, A5, A6) => Unit)(k: => Kernel6[A1, A2, A3, A4, A5, A6]) = {
    val key = f.getClass
    kernelCache.get(key) match {
      case None =>
        val kCompiled = k
        kernelCache(key) = kCompiled
        kCompiled
      case Some(k2: Kernel6[A1, A2, A3, A4, A5, A6]) =>
        println("found kernel in cache")
        k2
    }
  }

  def memoize[A1, A2, A3, A4, A5, A6, A7](f: (A1, A2, A3, A4, A5, A6, A7) => Unit)(k: => Kernel7[A1, A2, A3, A4, A5, A6, A7]) = {
    val key = f.getClass
    kernelCache.get(key) match {
      case None =>
        val kCompiled = k
        kernelCache(key) = kCompiled
        kCompiled
      case Some(k2: Kernel7[A1, A2, A3, A4, A5, A6, A7]) =>
        println("found kernel in cache")
        k2
    }
  }

/*

  def compile[A1, A2, A3](f: (A1, A2, A3) => Unit)(implicit ma1: Marshal[A1], ma2: Marshal[A2], ma3: Marshal[A3], dev: Device): Kernel3[A1, A2, A3] = memoize(f) {
    val transA1 = implicitly[Marshal[A1]]
    val transA2 = implicitly[Marshal[A2]]
    val transA3 = implicitly[Marshal[A3]]
    val sizeA1 = transA1.sizes(1).head
    val sizeA2 = transA2.sizes(1).head
    val sizeA3 = transA3.sizes(1).head
    val kernStr = new StringBuffer()

    val (kernName: String, tree: List[Tree]) = time({ firepile.Compose.compileToTreeName(f, 3) }, "Compile")

    for (t: Tree <- tree.reverse)
      kernStr.append(t.toCL)

    val kernBin = dev.buildProgramSrc(kernName, kernStr.toString)

//    class Arg[A](val value: A, val marshal: Marshal[A]) {
//        def toBuffers = marshal.toBuffer(value)
//    }
//    def applyKernel(args: Array[Arg[_]], output: Arg[_]): Unit = ...


    new Kernel3[A1, A2, A3] {
      def apply(a1: A1, a2: A2, a3: A3): Unit = {
        var bufA1: ByteBuffer = null
        var bufA2: ByteBuffer = null
        // val bufA3: ByteBuffer = transA3.toBuffer(a3).head
        var bufA1CLBuf: CLByteBuffer = null
        var bufA2CLBuf: CLByteBuffer = null
        var bufA3CLBuf: CLByteBuffer = null

        time({
          bufA1 = transA1.toBuffer(a1).head
          bufA2 = transA2.toBuffer(a2).head
          // val bufA3: ByteBuffer = transA3.toBuffer(a3).head

          bufA1CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA1, true)
          bufA2CLBuf = dev.context.createByteBuffer(CLMem.Usage.Input, bufA2, true)
        }, "Copy to GPU")

        val numItemsA1 = transA1.sizes(a1).head / sizeA1
        val numItemsA2 = transA2.sizes(a2).head / sizeA2
        val numItemsA3 = transA3.sizes(a3).head / sizeA3
        val bufA3capacity = transA3.sizes(a3).head

        bufA3CLBuf = dev.context.createByteBuffer(CLMem.Usage.Output, bufA3capacity)

        println("Output buffer capacity: " + bufA3capacity)

        val threads = (if (numItemsA1 < dev.maxThreads * 2) scala.math.pow(2, scala.math.ceil(scala.math.log(numItemsA1) / scala.math.log(2))) else dev.maxThreads).toInt

        // START TIMING CODE

        time({
          kernBin.setArg(0, bufA1CLBuf) // InvalidArgSize when passing straight ByteBuffer but ok with CLByteBuffer
          kernBin.setArg(1, numItemsA1)
          kernBin.setArg(2, bufA2CLBuf)
          kernBin.setArg(3, numItemsA2)
          kernBin.setArg(4, bufA3CLBuf)
          kernBin.setArg(5, numItemsA3)

          if (dev.memConfig == null) {
            kernBin.setLocalArg(6, threads * sizeA1)
            kernBin.setArg(7, threads)
            println("Executing with global work size = " + (numItemsA3 * threads) + " and local work size = " + threads)
            kernBin.enqueueNDRange(dev.queue, Array[Int](numItemsA3 * threads), Array[Int](threads))
          } else {
            // We don't really know if the local item types are the same as the global item types

            println("Executing with global work size = " + dev.memConfig.globalSize + " and local work size = " + dev.memConfig.localSize)
            kernBin.setLocalArg(6, dev.memConfig.localMemSize * sizeA1)
            kernBin.setArg(7, dev.memConfig.localMemSize)
            kernBin.enqueueNDRange(dev.queue, Array[Int](dev.memConfig.globalSize), Array[Int](dev.memConfig.localSize))
          }

          dev.queue.finish
        }, "GPU", numIterations)

        val bufOut = allocDirectBuffer(bufA3capacity)

        time({
          bufA3CLBuf.read(dev.queue, bufOut, true)

          bufOut.rewind

          // [NN] maybe need to copy?  but, probably not
          Array.copy(transA3.fromBuffer(List(bufOut)).asInstanceOf[AnyRef], 0, a3.asInstanceOf[AnyRef], 0, numItemsA3)
        }, "From GPU")
      }
    }
  }

*/
  // ...

  // TODO:
  // Write:
  // object GPUArray {
  //   // This will be compiled into a kernel specialized on f and A and B.
  //   def map(a: BBArray[A], b: BBArray[B], f: A => B) = { ... }
  //   def blockReduce(a: BBArray[A], b: BBArray[B], f: (A,A) => A) = { ... }
  //
  //   def mapKernel(f: A=>B): Kernel2[BBArray[A], BBArray[B]]
  // }
  //
  // class GPUArray[A](a: BBArray[A]) {
  //   def map(f: A => B)(implicit dev: Device) = {
  //     val k = /* memoize */ dev.compile( (a:BBArray[A], b:BBArray[B]) => GPUArray.map(a, b, f) )
  //     val that = BBArray.ofDim[B](a.length)
  //     k(this, that)
  //     new GPUArray(that)
  //   }
  //   def reduce(f: (A,A) => A)(implicit dev: Device) /* ??? (implicit blockSize: Int) */ = {
  //     val that = blockReduce(f)
  //     that.reduceLeft(f)
  //   }
  //   def blockReduce(f: (A,A) => A)(implicit dev: Device) /* ??? (implicit blockSize: Int) */ = {
  //     val k = /* memoize */ dev.compile( (a:BBArray[A], b:BBArray[A]) => GPUArray.blockReduce(a, b, f) )
  //     val that = BBArray.ofDim[B](a.length / blockSize)
  //     k(this, that)
  //     new GPUArray(that)
  //   }
  // }
  //
  // kinda want this:
  // trait Kernel1[A,B] extends Function1[A,B]
  // val k = mapk(_*2) compose reducek(_+_)
  // k(a, b)
  //
  // val a = BBArray.tabulate[Float](1000000)(_.toFloat)
  // val g = GPUArray(a, dev)
  // val b = g.map(_*2).reduce(_+_)
  //
  // g.map returns a MapKernel1
  //
  //
  // val k1 = dev.compile( ... GPUArray.map(.., _*2) )
  // val k2 = dev.compile( ... GPUArray.blockReduce(.., _+_) )
  // 

}

/*
object Compose {
  /*
  (x,y).zipWith(f).reduce(g)
  =>
  Arg2(x,y).zipWith(f).reduce(g) : Future[B]

  k = zipWith(f).reduce(g): Arg => Future[B]
*/

  val varNames = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

  def genVarNames(xs: List[_]): List[String] = {
    // TODO: what happens when i >= 52 ?
    xs.indices.toList.map(i => varNames(i).toString)
  }

  // TODO: move to Tree
  object Prototype {
    def apply(typ: Tree, name: Id, formals: List[Tree]): Prototype = Prototype(typ, name.name, formals)
  }
  case class Prototype(typ: Tree, name: String, formals: List[Tree]) extends Tree {
    def toCL = typ.toCL + " " + name + formals.map((t: Tree) => t.toCL).mkString("(", ", ", ");\n\n")
  }

  def compileToTreeName(src: AnyRef, arity: Int): (String, List[Tree]) = {
    val k = src.getClass
    val apply = Compiler.findApplyMethod(src, arity)
    val trees = compileRoot(k.getName, Compiler.signature(apply), List[Marshal[_]]()).reverse
    (methodName(apply), trees)
  }

  def compileToTree(src: AnyRef, arity: Int): (Tree, List[Tree]) = {
    val k = src.getClass
    val apply = Compiler.findApplyMethod(src, arity)
    val trees = compileRoot(k.getName, Compiler.signature(apply), List[Marshal[_]]()).reverse
    (Call(Id(methodName(apply)), (0 until arity).map(i => Id(varNames(i).toString)).toList), trees)
  }

  trait KernelLike {
    def trees: List[Tree]

    lazy val src = header + structs + prototypes + functions + kernelSrc(trees)

    private def header: String = ("\n" +
      "typedef char jbyte;                                  \n" +
      "typedef short jshort;                                \n" +
      "typedef ushort jchar;                                \n" +
      "typedef int jint;                                    \n" +
      "typedef long jlong;                                  \n" +
      "typedef float jfloat;                                \n" +
      "typedef double jdouble;                              \n" +
      "typedef char jboolean;                               \n" +
      "typedef union {                                      \n" +
      "  jbyte b;                                           \n" +
      "  jshort s;                                          \n" +
      "  jchar c;                                           \n" +
      "  jint i;                                            \n" +
      "  jlong l;                                           \n" +
      "  jfloat f;                                          \n" +
      "  jdouble d;                                         \n" +
      "  __global void *gp;                                 \n" +
      "  __local void *lp;                                  \n" +
      "} __any__;                                           \n" +
      "struct Tuple2 {                                      \n" +
      "  __any__ _1;                                        \n" +
      "  __any__ _2;                                        \n" +
      "};                                                   \n" +
      "struct Tuple3 {                                      \n" +
      "  __any__ _1;                                        \n" +
      "  __any__ _2;                                        \n" +
      "  __any__ _3;                                        \n" +
      "};                                                   \n" +
      "struct Tuple4 {                                      \n" +
      "  __any__ _1;                                        \n" +
      "  __any__ _2;                                        \n" +
      "  __any__ _3;                                        \n" +
      "  __any__ _4;                                        \n" +
      "};                                                   \n" +
      "struct Tuple5 {                                      \n" +
      "  __any__ _1;                                        \n" +
      "  __any__ _2;                                        \n" +
      "  __any__ _3;                                        \n" +
      "  __any__ _4;                                        \n" +
      "  __any__ _5;                                        \n" +
      "};                                                   \n" +
      "struct Tuple6 {                                      \n" +
      "  __any__ _1;                                        \n" +
      "  __any__ _2;                                        \n" +
      "  __any__ _3;                                        \n" +
      "  __any__ _4;                                        \n" +
      "  __any__ _5;                                        \n" +
      "  __any__ _6;                                        \n" +
      "};                                                   \n" +
      "\n")

    private def structs = trees.map {
      case t@StructDef(name, fields) => t.toCL + "\n"
      case t => ""
    }.mkString("")

    private def prototypes = trees.map {
      case t@FunDef(returnType, name, formals, _) => Prototype(returnType, name, formals).toCL + "\n"
      case t => ""
    }.mkString("")

    private def functions = trees.map {
      case t@FunDef(_, _, _, _) => t.toCL + "\n\n"
      case t => ""
    }.mkString("")

    protected def kernelSrc(trees: List[Tree]): String
  }
}
*/
