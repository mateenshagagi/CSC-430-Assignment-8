module Expressions
    implicit none
    private
    public :: ExprC, IdC, NumC, BinOp, BoolC, IfC, LamC, AppC, interp

    type :: ExprC
    end type ExprC

    type, extends(ExprC) :: IdC
      character(len=10) :: name
    end type IdC

    type, extends(ExprC) :: NumC
      integer :: n
    end type NumC

    type, extends(ExprC) :: BoolC
      logical :: value
    end type BoolC

    type, extends(ExprC) :: BinOp
      character(len=1) :: operation
      class(ExprC), allocatable :: l, r
    end type BinOp

    type, extends(ExprC) :: IfC
      class(ExprC), allocatable :: condition, trueBranch, falseBranch
    end type IfC

    type, extends(ExprC) :: LamC
      class(ExprC), allocatable :: param, body
    end type Lamc

    type, extends(ExprC) :: AppC
      class(ExprC), allocatable :: function, argument
    end type AppC

    !(struct AppC ([fun : ExprC] [arg : (Listof ExprC)]) #:transparent)
    !(struct LamC ([params : (Listof IdC)] [body : ExprC]) #:transparent)

contains

    recursive function interp(expr, error) result(value)
        class(ExprC), intent(in) :: expr
        integer, intent(out) :: error
        integer :: value

        error = 0  ! Initialize error as no error

        select type (e => expr)
        type is (NumC)
            value = e%n
        type is (BoolC)
            value = merge(1, 0, e%value)  ! Convert logical to integer (true to 1, false to 0)
        type is (BinOp)
            if (allocated(e%l) .and. allocated(e%r)) then
                select case (trim(e%operation))
                case ('+')
                    value = interp(e%l, error) + interp(e%r, error)
                case ('-')
                    value = interp(e%l, error) - interp(e%r, error)
                case ('*')
                    value = interp(e%l, error) * interp(e%r, error)
                case ('/')
                    value = interp(e%l, error) / interp(e%r, error)
                case ('<')
                    value = merge(1, 0, interp(e%l, error) < interp(e%r, error))
                case default
                    print *, 'Error: Unknown operation.'
                    error = 1
                    value = 0
                end select
            else
                print *, 'Error: Incomplete operation.'
                error = 1
                value = 0
            end if
        type is (IfC)
            if (allocated(e%condition) .and. allocated(e%trueBranch) .and. allocated(e%falseBranch)) then
                value = interp(e%condition, error)
                if (error == 0) then
                    if (value == 0) then
                        value = interp(e%falseBranch, error)
                    else
                        value = interp(e%trueBranch, error)
                    end if
                end if
            else
                print *, 'Error: IfC expression not fully specified.'
                error = 1
                value = 0
            end if
        type is (IdC)
            print *, 'Error: Identifier cannot be interpreted as a value.'
            error = 1
            value = 0
        class default
            print *, 'Error: Unknown type.'
            error = 1
            value = 0
        end select
    end function interp

end module Expressions


program main
    use Expressions
    implicit none
    type(NumC) :: ten, num1, num2
    type(BoolC) :: boolExpr
    type(IfC) :: ifTest
    type(BinOp) :: greaterThan10
    integer :: result, error_flag

    ten%n = 10
    boolExpr%value = .false.

    greaterThan10%operation = '<'
    greaterThan10%l = ten
    num1%n = 100
    num2%n = -10
    greaterThan10%r = num2

    ifTest%condition = greaterThan10
    ifTest%trueBranch = num1
    ifTest%falseBranch = num2

    result = interp(ifTest, error_flag)
    if (error_flag == 0) then
        print *, 'The result of the conditional operation is:', result
    else
        print *, 'An error occurred during interpretation.'
    end if

end program main