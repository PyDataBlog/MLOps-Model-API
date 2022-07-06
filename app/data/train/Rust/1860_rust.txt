//! Assignment operators (`+=`, `-=`, `*=`, etc) traits
//!
//! Note that no operator sugar is provided, i.e. `x += y` won't use these traits.
//!
//! This crate is only  a convenience for me, as I've the need of using these traits as bounds in
//! some of my libraries, and I'd rather not duplicate the definitions in each library.
//!
//! This crate will be deprecated once these traits land in stdlib. (See rust-lang/rfcs#393)

#![deny(missing_docs)]
#![deny(warnings)]

/// The `+=` operator (without the sugar)
pub trait AddAssign<Rhs=Self> {
    /// `a += b` -> `AddAssign::add_assign(&mut a, b)`
    fn add_assign(&mut self, Rhs);
}

/// The `/=` operator (without the sugar)
pub trait DivAssign<Rhs=Self> {
    /// `a /= b` -> `DivAssign::div_assign(&mut a, b)`
    fn div_assign(&mut self, Rhs);
}

/// The `*=` operator (without the sugar)
pub trait MulAssign<Rhs=Self> {
    /// `a *= b` -> `Mulssign::mul_assign(&mut a, b)`
    fn mul_assign(&mut self, Rhs);
}
/// The `-=` operator (without the sugar)
pub trait SubAssign<Rhs=Self> {
    /// `a -= b` -> `SubAssign::sub_assign(&mut a, b)`
    fn sub_assign(&mut self, Rhs);
}
