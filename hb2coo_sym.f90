! This program convert the symmetric Harwell Boeing format to the
! symetric COO format
! This HB format comes from Ansys Mechanical
! The data region has four sections
! row_pointers
! column_index (is the column number in COO format) 
! values        
! RHS         
!
!   
      program hb2coo_sym
      implicit none

      integer*8, allocatable, dimension(:) :: row_pointer, row_coor
      integer*8, allocatable, dimension(:) :: row_index
      real*8, allocatable, dimension(:) :: values
      real*8, allocatable, dimension(:) :: rhs_values
      integer*8, allocatable, dimension(:) :: nnz_per_row


!TOTCRD: Total number of lines excluding header
!PTRCRD: Number of lines for pointers
!INDCRD: Number of lines for row(or variable) indices. 
! variables means values, row indeces means at each column,
! which row has non-zeros. so it is equivalent as column
! coordinate of each non-zero.
!VALCRD: Number of lines for numerical values
!RHSCRD: Number of lines for right-hand sides

!NROW: Number of rows(or variables) 
!NCOL: Number of columns(or elements)
!NNZERO: Number of row indices(or variables), equal to
! number of entries for assembled matrices
!NELTVL: Number of elemental matrix entries

!PTRFMT: Format for pointers
!INDFMT: Format for row(or variable) indices
!VALFMT: Format for numerical values of coefficient matrix
!RHSFMT: Format for numerical values of right-hand sides

!RHSTYP: Right-side type
!NRHS: Number of right-hand sides
!NRHSIX: Number of row indices     
      character(len=72) :: TITLE
      character(len=8) :: KEY
      integer*8 :: TOTCRD, PTRCRD, INDCRD, VALCRD, RHSCRD
      character(len=3) :: MXTYPE
      integer*8 :: NROW, NCOL, NNZERO, NELTVL
      character(len=16) :: PTRFMT, INDFMT
      character(len=20) :: VALFMT, RHSFMT
      character(len=3) :: RHSTYP
      integer*8 :: NRHS, NRHSIX
      integer*8 :: INDROW
      integer*8 :: n, nnz, i, j, k, p, q, r, s
      integer :: iarg
      character(len=30) :: arg, input_name, output_name
!--------------------------------------------------
       iarg=0
        do iarg=0, iargc()
          call getarg(iarg, arg)
            if(len_trim(arg)==0) exit
            if(iarg.EQ.1) then
              input_name=trim(arg)
              else if(iarg.EQ.2) then
              output_name=trim(arg)
            endif
        end do
      open(unit=10,file=input_name, status='unknown')
      open(unit=11,file=output_name,status='unknown')

      read(10,999) TITLE, KEY
      read(10,1000) TOTCRD, PTRCRD, INDCRD, VALCRD, RHSCRD
      read(10,2000) MXTYPE, NROW, NCOL, NNZERO, NELTVL  
      read(10,3000) PTRFMT, INDFMT, VALFMT, RHSFMT
      read(10,4000) RHSTYP, NRHS, NRHSIX
      print 999, TITLE, KEY
      print 1000, TOTCRD, PTRCRD, INDCRD, VALCRD, RHSCRD
      print 2000, MXTYPE, NROW, NCOL, NNZERO, NELTVL
      print 3000, PTRFMT, INDFMT, VALFMT, RHSFMT
      print 4000, RHSTYP, NRHS, NRHSIX 
!---------------------------------------
!      
!-----------------------------------------
        allocate(row_pointer(PTRCRD))
        do i=1,PTRCRD
          read(10, '(I14)') row_pointer(i)         
        end do
        
        allocate(nnz_per_row(RHSCRD))
        do j=1, PTRCRD-1
          nnz_per_row(j) = row_pointer(j+1)-row_pointer(j)
        end do
        
        allocate(row_coor(VALCRD))
        k = 1
        do i = 1, RHSCRD
          do j = 1, nnz_per_row(i)
             row_coor(k) = i
             k = k+1
          end do   
        end do
       

        allocate(row_index(INDCRD))
        do i=1,INDCRD
           read(10, '(I14)') row_index(i)     
        end do

        allocate(values(VALCRD))
        do j=1, VALCRD
          read(10, '(E25.15)') values(j)
        end do

        allocate(rhs_values(RHSCRD))
        do k=1, RHSCRD
          read(10, '(E25.15)') rhs_values(k)
        end do

        
!---------------------------------------
      open(unit=11,file=output_name, status='unknown')
      write(11, '(I14, I14)') RHSCRD, VALCRD
      do i=1, VALCRD
      write(11, '(I14, I14, E25.15)') row_coor(i),row_index(i),values(i)
      end do

      do i=1, RHSCRD
        write(11, '(E25.15)') rhs_values(i)
      end do
    
 999  format(A72, A8)     
 1000 format(5I14)
 2000 format(A3, 11X, 4I14)
 3000 format(2A16, 2A20)
 4000 format(A3, 11X, 2I14)
 5000 format(E25.15)


 6000 FORMAT ( A72, A8 / 5I14 / A3, 11X, 4I14 / 2A16, 2A20 )
        close(10) 
        close(11)
!---------------------------------------
      stop
      end

