class A{
  def b(c: => Unit){}
  b{
//    e("f")
    e
    new G()(){}
 }
}
class G(h:String="i")()
