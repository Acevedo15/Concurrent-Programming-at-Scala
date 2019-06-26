class PutSpaceActor (fe: String, out: ActorRef) extends Actor {
  
  def receive = PartialFunction.empty 
  
  
 var file = new File(fe)
 var fr: FileReader = new FileReader(file)
 val br: BufferedReader = new BufferedReader(fr)
 
 var line = br.readLine();
 var new_line= new StringBuilder(line)
 
 fr.close()
 br.close()
 
 var i = 1
 var times = 0
 
 while(10*i + times < line.length()){
   
  
   new_line.setCharAt(10*i+times, ' ')
   i = i + 1
   times = times +1 
   
 }
 
 line = new_line.toString();

  
  out ! line
  
}
object PutSpaceActor { 
  def props(fe: String, out: ActorRef) = Props(new PutSpaceActor(fe, out))
}

class PutArrowActor (out: ActorRef) extends Actor{ 
  
  var previous, actual: Char = ' '
 
  
  def receive = {
    
    case line => 
      
      var new_line = line.toString()
      var modified_line = new StringBuilder(new_line)
      
      var counting = 0
      var counting_next = -1;
      
      for(c <- new_line) {
        
        
        if(counting == 0){
          previous = c
        }
          
        else {
          
          actual = c
          
          if(previous == '*' && actual =='*'){
              
              modified_line.setCharAt(counting, '^')
              modified_line.deleteCharAt(counting_next)        
              actual = '^'
              counting = counting - 1 // al haber eliminado un elemento me tengo que quedar en la misma posicion
              counting_next = counting_next - 1
          }
          
          previous = actual
      
        }
           
          counting = counting + 1;
          counting_next = counting_next + 1
          
        
      }
      
      new_line = modified_line.toString()
      
      
      
      out ! new_line
      
  }
  
  
}
object PutArrowActor { 
  def props(out: ActorRef) = Props(new PutArrowActor(out))
}

class PutCRActor (fs: String) extends Actor{
  
  var path: String = "C:/Users/AcevedoAngel/PracticasPCA/Practica_VIII/"+fs+".txt"
  var file = new File(path)
  

  
  

 
  def receive = {
    
    case new_line: String =>
      
      var line = new StringBuilder(new_line)
      var bw = new BufferedWriter(new FileWriter(file))
      
      println("Sentence before introducing carriage return: "+"\n"+new_line)
      println("---")
      var i = 1
      var times = 0;
      
       while(13*i + times < new_line.length()){ 
    
         line.insert(13*i + times , '\n') 
                                            
         i = i + 1
         
         times = times + 1
       }
      
      bw.write(line.toString)
 
      bw.close()
    
     
  }
  

}

object PutCRActor { 
  def props(fs: String) = Props(new PutCRActor(fs))
}
 
object ExeActors801 extends App {
  
  val fe = "in.txt"
  val fs = "out"
  val ourSystem = ActorSystem("CharacterTransformer")
  val pCL: ActorRef = ourSystem.actorOf(PutCRActor.props(fs))
  val pFl: ActorRef = ourSystem.actorOf (PutArrowActor.props (pCL))
  val pBl: ActorRef = ourSystem.actorOf (PutSpaceActor.props (fe, pFl))
  Thread.sleep (5000)
  println ("END")
  ourSystem.terminate
  
}