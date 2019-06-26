object Ordenacion extends App {
  
  def msort[T](less: (T,T) => Boolean)(xs: List[T]): List[T] ={
    
    def merge (xs: List[T], ys: List[T]): List[T] = {
      
    
    (xs, ys) match {
      
      case (Nil, _) => ys 
      case (_, Nil) => xs 
      case (x::xs1, y::ys1) =>
        
        if (less(x,y)) 
          x::merge(xs1,ys) 
        else 
          y::merge(xs, ys1) 
        
      }
    
    
    }
    
    val n = xs.length/2
    if(n==0) xs
    else{
      val (ys ,zs) = xs splitAt n 
      merge (msort(less)(ys), msort(less)(zs))
    }
    
  }
  
  
  def msortConcurrent[T](less: (T,T) => Boolean)(xs: List[T]): List[T] ={
    
       def merge (xs: List[T], ys: List[T]): List[T] = {
      
    
        (xs, ys) match {
      
        case (Nil, _) => ys 
        case (_, Nil) => xs 
        case (x::xs1, y::ys1) =>
        
          if (less(x,y)) 
            x::merge(xs1,ys) 
          else 
            y::merge(xs, ys1) 
        
        }
    
    
    }
    
    
     val m = xs.length/2
        if (m == 0) xs
        
        else{
          val (ys, zs) = xs splitAt(m)
          
          
          var list1  = List[T](); 
          var list2 = List[T](); 
              
          val Thread1 = new Thread { //The first Thread sort the first part of the list
      
            override def run(): Unit = {
              
                val m1 = ys.length/2             
                
                if (m1 == 0) list1 = ys
                else{
                  val(l1,l2) = ys.splitAt(m1)
                  list1 = merge(msort(less)(l1), msort(less)(l2))
                }
                
              
            }
           
           
            
          } //Thread1
          
          Thread1.setName("Thread1")
          Thread1.setDaemon(true)
          Thread1.start()
          Thread1.join()
            
           val Thread2 = new Thread {
             
             override def run(): Unit = {
               
               val m2 = zs.length/2
               
               if(m2 == 0) list2 = zs
               else{      
                  val(l3,l4) = zs.splitAt(m2)
                  list2 = merge(msort(less)(l3), msort(less)(l4))
               }
             }
          
             
          
        } //Thread2
           
            
             
       Thread2.setName("ThreadList2")
       Thread2.setDaemon(true)
       Thread2.start()
       Thread2.join() 
       
       println(list1)
       println(list2)
       merge(list1,list2)
       
       
      } //else
           
         
      
  } //msort
  
  
  val l = msortConcurrent((x:Int, y: Int)=> x < y)(List(8,10,12,7,5,2,3))

  
  println(l)
  
  
 
}