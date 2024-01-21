!     __________________________________________________________________
!     _______________SUBROUTINE FOR DEBUGGING REAL______________________
!     __________________________________________________________________

subroutine debugger_real(matrix,n,m,matrix_name,line,output)

IMPLICIT NONE

!     -----------------------INTEGER VARIABLES-------------------------- 
integer :: i,j
integer, intent(in):: n,m,output,line

!     ------------------------REAL VARIABLES---------------------------- 
real(kind=8), intent(in) :: matrix(n,m)

!     -----------------------CHARACTER VARIABLES------------------------
character(len=*) :: matrix_name

! NO FORMATING DEPENDING ON COMPILER
if (output==0) then; write(6,*) '-------------'
  write(6,*)  line, matrix_name
  write (6,*) matrix(:,:)
end if

! SCIENTIFIC FORMATING
if (output==1) then
  write(6,*) '-------------'
  write(6,*)  line, matrix_name
  do i=1, n
    write (6,'(10ES15.6)') matrix(i,:)
  end do
end if

! FOR NON INTEL COMPILERS SCIENTIFIC FORMATING
if (output==2) then
  write(6,*) '-------------'
  write(6,*)  line, matrix_name
  do i=1, n
    write (6,'(10ES15.6)') (matrix(i,j),j=1,m)
  end do
end if

return
end subroutine

!     __________________________________________________________________
!     ______________SUBROUTINE FOR DEBUGGING INTEGER____________________
!     __________________________________________________________________

subroutine debugger_integer(matrix,n,m,matrix_name,line,output)

IMPLICIT NONE

!     -----------------------INTEGER VARIABLES-------------------------- 
integer :: i,j
integer, intent(in):: n,m,output,line,matrix(n,m)

!     -----------------------CHARACTER VARIABLES------------------------
character(len=*) :: matrix_name

! NO FORMATING DEPENDING ON COMPILER
if (output==0) then
  write(6,*) '-------------'
  write(6,*)  line, matrix_name
  write (6,*) matrix(:,:)
end if

! SCIENTIFIC FORMATING
if (output==1) then
  write(6,*) '-------------'
  write(6,*)  line, matrix_name
  do i=1, n
    write (6,'(10I4)') matrix(i,:)
  end do
end if

! FOR NON INTEL COMPILERS SCIENTIFIC FORMATING
if (output==2) then
  write(6,*) '-------------'
  write(6,*)  line, matrix_name
  do i=1, n
    write (6,'(10I4)') (matrix(i,j),j=1,m)
  end do
end if

return
end subroutine