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
      class(IdC), allocatable :: param
      class(ExprC), allocatable :: body
    end type Lamc

    type, extends(ExprC) :: AppC
      class(LamC), allocatable :: function
      class(ExprC), allocatable :: argument
    end type AppC

    
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
            result_value%str = e%name
            result_value%flag = 2
            !value = 0
        type is (AppC)
            result_value = substitute(e%function%body, e%function%param, e%argument, error)
            !value = 0
        class default
            print *, 'Error: Unknown type!'
            error = 1
            !value = 0
        end select
    end function interp

    function substitute(body, param, arg, error) result(result_value)
        implicit none
        class(ExprC), intent(in) :: body, arg
        class(IdC), intent(in) :: param
        integer, intent(out) :: error
        type(BinOp) :: binOpBody
        type(IfC) :: IfCBody
        type(Value) :: result_value
        type(Value) :: temp
        type(Value) :: temp2
        result_value%number = 0
        result_value%str = ''
        result_value%bool = .false.
        result_value%flag = -1

        error = 0  ! Initialize error as no error

        select type (e => body)
        type is (NumC)
          result_value = interp(e, error)
        type is (BoolC)
            result_value = interp(e, error)
        type is (BinOp)
            if (allocated(e%l) .and. allocated(e%r)) then
                binOpBody%l = e%l
                binOpBody%r = e%r
                binOpBody%operation = e%operation

                temp = interp(e%l, error)
                temp2 = interp(param, error)
                if(temp%str == temp2%str) then
                    binOpBody%l = arg
                end if
                temp = interp(e%r, error)
                if(temp%str == temp2%str) then
                    binOpBody%r = arg
                end if
                result_value = interp(binOpBody, error)
            else
                print *, 'Error: Incomplete operation.'
                error = 1
                result_value%flag = -1
            end if
        type is (IfC)
            if (allocated(e%condition) .and. allocated(e%trueBranch) .and. allocated(e%falseBranch)) then
                IfCBody%condition = e%condition
                IfCBody%trueBranch = e%trueBranch
                IfCBody%falseBranch = e%falseBranch

                temp = interp(e%condition, error)
                temp2 = interp(param, error)
                if(temp%str == temp2%str) then
                    IfCBody%condition = arg
                end if
                temp = interp(e%trueBranch, error)
                if(temp%str == temp2%str) then
                    IfCBody%trueBranch = arg
                end if
                temp = interp(e%falseBranch, error)
                if(temp%str == temp2%str) then
                    IfCBody%falseBranch = arg
                end if
                result_value = interp(IfCBody, error)
            else
                print *, 'Error: Incomplete operation.'
                error = 1
                result_value%flag = -1
            end if
        type is (IdC)
            result_value%str = e%name
            result_value%flag = 2
            !value = 0
        class default
            print *, 'Error: Unknown type.'
            error = 1
            !value = 0
        end select
    end function substitute

end module Expressions


program main
    use Expressions
    implicit none
    type(NumC) :: ten, num1, num2, one, argument, fifteen, zero
    type(BoolC) :: boolExpr
    type(IdC) :: add, x
    type(IfC) :: ifTest
    type(BinOp) :: add1, lessThan10
    type(AppC) :: add1AppC, ifAppC
    type(LamC) :: add1LamC, ifLamC
    type(Value) :: retValue
    integer :: error_flag

    ! add1: adds 1 to parameter x
    one%n = 1
    argument%n = 50
    x%name = "x"
    add1%operation = "+"
    add1%l = one
    add1%r = x
    add1LamC%param = x
    add1LamC%body = add1
    add1AppC%function = add1LamC
    add1AppC%argument = argument

    ! IfTest: return x < 0x
    


    ten%n = 10
    boolExpr%bool = .false.
    add%name = "add"

    allocate(lessThan10%l, source=ten)
    allocate(lessThan10%r, source=num1)
    lessThan10%operation = '<'
    !lessThan10%l = ten
    num1%n = 0
    num2%n = -10
    !lessThan10%r = num2

    allocate(ifTest%condition, source=lessThan10)
    allocate(ifTest%trueBranch, source=num1)
    allocate(ifTest%falseBranch, source=num2)
    ! ifTest%condition = lessThan10
    ! ifTest%trueBranch = num1
    ! ifTest%falseBranch = num2

    !retValue = interp(ifTest, error_flag)
    retValue = interp(add1AppC, error_flag)
    if (error_flag == 0) then
        if (retValue%flag == 0) then
            print *, 'The result of the conditional operation is:', retValue%number
        else 
            print *, "interp result: ", retValue%str
        end if
    else
        print *, 'An error occurred during interpretation.'
    end if

    ! test appc ifc
    fifteen%n = 15
    zero%n = 0

    ! change this param to test true/false branch in appc body
    lessThan10%r = fifteen

    ifTest%condition = lessThan10
    ifTest%trueBranch = x
    ifTest%falseBranch = zero

    ifLamC%param = x
    ifLamC%body = ifTest

    ifAppC%function = ifLamC
    ifAppC%argument = one

    ! (AppC (LamC x (if < 10)) 
    !       1)
    retValue = interp(ifAppC, error_flag)
    if (error_flag == 0) then
        if (retValue%flag == 0) then
            print *, 'The result of the conditional operation is:', retValue%number
        else 
            print *, "interp result: ", retValue%str
        end if
    else
        print *, 'An error occurred during interpretation.'
    end if

end program main