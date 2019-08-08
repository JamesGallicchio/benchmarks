object BrainFuck {
  def run(prog: String): Unit = {
    var idx = 0
    var tape = new ArrayBuffer[Byte]()
    var ptr = 0
    var brackets = List[Int]()
    
    while (idx < prog.length) {
      prog(idx) match {
        case '+' => tape(ptr) += 1
        case '-' => tape(ptr) += -1
        case '>' => ptr += 1
        case '<' => ptr -= 1
        case '[' => brackets = idx :: brackets
        case ']' => idx = brackets.head; brackets = brackets.tail
        case '.' => print(tape(ptr).toChar)
        case _   =>
      }
      idx += 1
    }
  }
  
  def time(f: => Any):  = {
    val s = System.nanoTime
    val ret = f
    val e = System.nanoTime
    System.err.println("time: "+(e-s)/1e9+"s")
    ret
  }

  def main(args: Array[String]): Unit = {
    val text = scala.io.Source.fromFile(args(0)).mkString

    //warmup
    System.err.println("warmup")
    time {
      new Program(">++[<+++++++++++++>-]<[[>+>+<<-]>[<+>-]++++++++[>++++++++<-]>[-]<<>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[>++++++++++[-]<-]<-]<-]<-]<-]<-]<-]++++++++++").run
    }
    //

    System.err.println("run")
    time {
      run(text)
    }
  }
}
