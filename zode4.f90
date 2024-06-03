module Expressions
  implicit none
  private
  public ExprC, IdC, NumC
  ! Note another way of using the public attribute:
  ! gathering all public data types in one place.

  type :: ExprC
  end type

  type, extends(ExprC) :: IdC
    character(10) :: name
  end type

  type, extends(ExprC) :: NumC
    integer :: n
  end type


end module Expressions

program main
  use Expressions
  implicit none
  type(IdC) :: hello
  type(NumC) :: num

  hello%name = "Hello"
  print *, hello%name

  num%n = 100
  print *, num%n



end program main