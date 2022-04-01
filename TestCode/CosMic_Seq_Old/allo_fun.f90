module allo_fun
implicit none

interface func_test
module procedure :: func_test_int
module procedure :: func_test_char
end interface

contains

function func_test_int(temp) result(res)
implicit none
integer,allocatable  :: res(:)
integer,dimension(:) :: temp
integer a(10)

a =(/1,2,3,4,5,6,7,8,9,10/)
allocate(res(size(temp)))
res = a(temp)

end function

function func_test_char(temp1,temp) result(res)
implicit none
character,allocatable  :: res(:)
character,dimension(:) :: temp
integer,dimension(:)   :: temp1
integer a(10)

! a =(/1,2,3,4,5,6,7,8,9,10/)
! allocate(res(size(temp)))
res = temp(temp1)

end function


subroutine test_sub(input,output)
implicit none
integer,allocatable :: output(:)
integer,dimension(:):: input
integer,dimension(10):: base

base = (/1,2,3,4,5,6,7,8,9,10/)
allocate(output(size(input)))
output = base(input)

end subroutine

end module
program main
use allo_fun
implicit none
type dd
integer,allocatable :: type_id(:)
end type

type(dd)            :: dds
integer,allocatable :: a(:)
integer             :: b(2),c(3),base(10)

integer,allocatable             :: output(:)
character,allocatable           :: output1(:)
character,dimension(4)          :: temp1
 b = (/2,3/)
 c = (/3,4,5/)
!  call test_sub(c,output)
allocate(output(5))
print *,output
output = output(b)
! print *,output
! deallocate(output)
! call test_sub(b,output)
temp1 = (/"a","b","c","d"/)
output1 = func_test(b,temp1)
print *,output1
print *,output
end program
