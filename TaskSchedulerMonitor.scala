import de._
import scala.collection.mutable.ArrayBuffer


case class Data(val t:() => Unit, val period: Int, var remaining: Int)

class Monitor {
  
  var task = new ArrayBuffer[Data]()
  var numTask = 0
  
  def admission( t:() => Unit, period: Int) : Unit = synchronized {
    
    task.insert(numTask, Data(t, period, period))
    numTask += 1
  
    
    }
   
    
  def updateTime (seconds: Int): Unit = synchronized {
    
   task.foreach(a => {
     
     a.remaining -= seconds
    
     if(a.remaining <= 0){
       
       a.remaining = a.period //reseteo el tiempo
       a.t()
     }
   
   
 
  })
  
}
  
}

object TaskSchedulerMonitor extends App {
  
  val myMonitor = new Monitor
  val clock = new Thread {
    
    var passedTime: Int = 0
    
    override def run() = {
      while (passedTime < 10){
        
        Thread.sleep(1000)
        passedTime += 1
        println(s"Actual time: $passedTime")
        myMonitor.updateTime(1)
      }
    }
  }
  
  def task1 = println("I'm task1 and execute myself every 2 s")
  myMonitor.admission(() => task1, 2)
  def task2 = println("I'm task2 and execute myself every 3 s")
  myMonitor.admission(() => task2, 3)
  
  clock.start()
  clock.join()
  
  
  Thread.sleep(12000)
  log("End")
  
  
  
}