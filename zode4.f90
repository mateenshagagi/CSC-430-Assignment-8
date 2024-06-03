module Expressions
  implicit none
  private
  public ExprC, IdC
  ! Note another way of using the public attribute:
  ! gathering all public data types in one place.

  type :: ExprC
  end type

  type, extends(ExprC) :: IdC
    character(10) :: name
  end type


end module Expressions

program main
  use Expressions
  implicit none
  type(IdC) :: hello

  hello%name = "Hello"

  print *, hello%name



end program main