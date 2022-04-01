

program main
      implicit none
      integer i,size_a,size_b
      integer,dimension(10)::a,b,c,d
      integer, allocatable:: e(:)
      integer,dimension(size(a))::f
      size_a = size(a)
      size_b = size(b)
      do i = 1,size_a
        c(i) = i 
      end do


     do i = 1,size_b
        d(i) = i
     end do
end program
