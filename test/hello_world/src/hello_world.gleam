pub type Test0 {
  Constructor1(a: Int, b: String, t: Test2)
}
pub type Test2 {
  Constructor2(a: Int)
 }
pub fn main() {
  Constructor2(1)
}
