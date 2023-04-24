pub type Test0 {
  Constructor1(a: Int, b: String, t: Test2)
}
pub type Test2 {
  Constructor2(a: Int)
}
pub fn main() {
  let a = Constructor2(1)
//  let assert (m_module, name) = typeof(a)
//
//  to_json(typeof(a), a)
}


