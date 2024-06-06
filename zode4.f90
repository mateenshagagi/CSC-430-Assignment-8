! TODO:
! add Value class
! Type Value holds 2 fields: an integer and a string
! change interp to return a Value



module Expressions
    implicit none
    private
    public :: ExprC, IdC, NumC, BinOp, BoolC, IfC, LamC, AppC, Value, interp

    type :: Value
        integer :: number
        character(len=50) :: str
        logical :: bool
        integer :: flag  ! 0: number, 1: boolean, 2: string
    end type Value
    
    type :: ExprC
    end type ExprC

    type, extends(ExprC) :: IdC
      character(len=10) :: name
    end type IdC

    type, extends(ExprC) :: NumC
      integer :: n
    end type NumC

    type, extends(ExprC) :: BoolC
      logical :: bool
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

    recursive function interp(expr, error) result(result_value)
        implicit none
        class(ExprC), intent(in) :: expr
        type(NumC) :: num
        integer, intent(out) :: error
        type(Value) :: result_value
        type(Value) :: temp
        type(Value) :: temp2
        result_value%number = 0
        result_value%str = ''
        result_value%bool = .false.
        result_value%flag = -1

        error = 0  ! Initialize error as no error
        num%n = 1

        select type (e => expr)
        type is (NumC)
          result_value%number = e%n
          result_value%flag = 0
          print *, 'Value is a numC.'
        type is (BoolC)
          result_value%bool = e%bool
          result_value%number = merge(1, 0, e%bool)  
          result_value%str = merge('true!', 'false', e%bool)
          result_value%flag = 1
        type is (BinOp)
            if (allocated(e%l) .and. allocated(e%r)) then
                select case (trim(e%operation))
                case ('+')
                  temp = interp(e%l, error)
                  temp2 = interp(e%r, error)
                  result_value%number = temp%number + temp2%number

                  result_value%flag = 0
                case ('-')
                  temp = interp(e%l, error)
                  temp2 = interp(e%r, error)
                  result_value%number = temp%number - temp2%number
                  
                  result_value%flag = 0
                case ('*')
                  temp = interp(e%l, error)
                  temp2 = interp(e%r, error)
                  result_value%number = temp%number * temp2%number
                  
                  result_value%flag = 0
                case ('/')
                  temp = interp(e%l, error)
                  temp2 = interp(e%r, error)
                  result_value%number = temp%number / temp2%number
                  
                  result_value%flag = 0
                case ('<')
                  temp = interp(e%l, error)
                  temp2 = interp(e%r, error)
                  
                  result_value%bool = temp%number < temp2%number
                  result_value%number = merge(1, 0, result_value%bool)
                  
                  result_value%flag = 1
                case default
                  print *, 'Error: Unknown operation.'
                  error = 1
                  result_value%number = 0
                  result_value%str = 'error'
                  result_value%flag = -1
                end select
            else
                print *, 'Error: Incomplete operation.'
                error = 1
                result_value%flag = -1
            end if
        type is (IfC)
            if (allocated(e%condition) .and. allocated(e%trueBranch) .and. allocated(e%falseBranch)) then
              temp = interp(e%condition, error)
                if (error == 0) then
                    if (temp%bool) then
                        result_value = interp(e%falseBranch, error)
                    else
                        result_value = interp(e%trueBranch, error)
                    end if
                end if
            else
                print *, 'Error: IfC expression not fully specified.'
                error = 1
                !value = 0
            end if
        type is (IdC)
            print *, 'Error: Identifier cannot be interpreted as a value.'
            error = 1
            !value = 0
        class default
            print *, 'Error: Unknown type.'
            error = 1
            !value = 0
        end select
    end function interp

    ! function substitute(body, param, arg, error) result(value)
    !     class(ExprC), intent(in) :: body, arg
    !     class(IdC), intent(in) :: param
    !     type(BinOp) :: binOpBody
    !     type(IfC) :: ifCBody

    !     integer, intent(out) :: error
    !     integer :: value

    !     error = 0  ! Initialize error as no error

    !     select type (e => body)
    !     type is (NumC)
    !         value = e%n
    !     type is (BoolC)
    !         value = merge(1, 0, e%value)  ! Convert logical to integer (true to 1, false to 0)
    !     type is (BinOp)
    !         if (allocated(e%l) .and. allocated(e%r)) then 
    !             binOpBody%l = e%l
    !             binOpBody%r = e%r
    !             binOpBody%operation = e%operation
    !             ! check if the interp of e%l is a string equal to the interped value of param

    !             if(interp(e%l, error) == param) then
    !                 binOpBody%l = arg
    !             end if
    !             if(interp(e%r, error) == param) then
    !                 binOpBody%r = arg
    !             end if
    !             value = interp(binOpBody, error)
                
    !         else
    !             print *, 'Error: Incomplete operation.'
    !             error = 1
    !             !value = 0
    !         end if
    !     type is (IfC)
    !         if (allocated(e%condition) .and. allocated(e%trueBranch) .and. allocated(e%falseBranch)) then
                
    !         else
    !             print *, 'Error: IfC expression not fully specified.'
    !             error = 1
    !             !value = 0
    !         end if
    !     type is (IdC)
    !         print *, 'Error: Identifier cannot be interpreted as a value.'
    !         error = 1
    !         !value = 0
    !     class default
    !         print *, 'Error: Unknown type.'
    !         error = 1
    !         !value = 0
    !     end select
    ! end function substitute
end module Expressions


program main
    use Expressions
    implicit none
    type(NumC) :: ten, num1, num2
    type(BoolC) :: boolExpr
    type(IfC) :: ifTest
    type(BinOp) :: greaterThan10
    type(Value) :: retValue
    integer :: error_flag

    ten%n = 10
    boolExpr%bool = .false.

    allocate(greaterThan10%l, source=ten)
    allocate(greaterThan10%r, source=num2)
    greaterThan10%operation = '<'
    !greaterThan10%l = ten
    num1%n = 100
    num2%n = -10
    !greaterThan10%r = num2

    allocate(ifTest%condition, source=greaterThan10)
    allocate(ifTest%trueBranch, source=num1)
    allocate(ifTest%falseBranch, source=num2)
    ! ifTest%condition = greaterThan10
    ! ifTest%trueBranch = num1
    ! ifTest%falseBranch = num2

    retValue = interp(ifTest, error_flag)
    if (error_flag == 0) then
        print *, 'The result of the conditional operation is:', retValue%number
    else
        print *, 'An error occurred during interpretation.'
    end if

end program main