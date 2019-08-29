# OOP-Fortran-Examples

Examples of using the object-oriented features in Fortran, taken from
[my presentation at RSECon
2019](https://rseconuk2019.sched.com/event/QKqu/2d1-revitalising-legacy-languages-teaching-an-old-dog-new-tricks-object-oriented-programming-in-fortran).

## Contents

- `01-derived-type.f90`: Demonstration of the (non-OOP) derived types
  available since Fortran 90
- `02-oop-features.f90`: Demonstration of the OOP features
  (encapsulation, inheritance, polymorphism) available since Fortran
  2003
- `03-abstract-types.f90`: An example of using an abstract type to
  define an interface (here demonstrating the [Strategy
  Pattern](https://en.wikipedia.org/wiki/Strategy_pattern))
- `04-lapack-wrapper.f90`: An example of using OOP to wrap cumbersome
  legacy Fortran routines (in this case,
  [dgesvx](http://www.netlib.org/lapack/explore-html/d7/d3b/group__double_g_esolve_ga9d90ccf6e340cacd08b7bbbb502ceb21.html#ga9d90ccf6e340cacd08b7bbbb502ceb21)
  from LAPACK)
