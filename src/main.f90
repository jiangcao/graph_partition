! Copyright (c) 2023 Jiang Cao, ETH Zurich 
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! 1. Redistributions of source code must retain the above copyright notice,
!    this list of conditions and the following disclaimer.
! 2. Redistributions in binary form must reproduce the above copyright notice,
!    this list of conditions and the following disclaimer in the documentation
!    and/or other materials provided with the distribution.
! 3. Neither the name of the copyright holder nor the names of its contributors 
!    may be used to endorse or promote products derived from this software without 
!    specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
! CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
! SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
! INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
! CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
! ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE. 
!
PROGRAM main
use graph_partition 

implicit none

integer, allocatable   :: g(:,:)        !! Graph connectivity table
integer, allocatable   :: S(:,:)        !! Slices information
integer, allocatable   :: E1(:),E2(:)      !! Edge information
integer :: NMAX
integer :: i
real(8) :: threshold

real :: start, finish


print *,'threshold = '
read(*,*) threshold

call cpu_time(start)
    
call ReadGraphFromText('graph_dat',g,threshold=threshold,use0index=.true.)

open(unit=11,file='graph.dat')
call SaveTxtGraph(11,g)
close(11)

call ReadEdgeFromText('edge1_dat',E1,use0index=.false.)
print '("Number of points in left contact ",i18)',size(E1)
call ReadEdgeFromText('edge2_dat',E2,use0index=.false.)
print '("Number of points in right contact ",i18)',size(E2)

call cpu_time(finish)
print '("Reading File Time = ",f0.3," seconds.")',finish-start
start = finish

NMAX = 50
call slice(g,E1,S) ! 1 contact slicing


call cpu_time(finish)
print '("1 Contact Slicing Time = ",f0.3," seconds.")',finish-start
start = finish

i = testSlicing(g,S)

if ( i .ne. 0 ) then
   print *,'problem', i
endif

open(unit=11,file='slice_1contact.dat')

call SaveSlicesTxt(11,S)

close(11)


call cpu_time(finish)
print '("Saving File Time = ",f0.3," seconds.")',finish-start
start = finish

call slice(g,E1,E2,NMAX,S) ! 2 contact slicing

call cpu_time(finish)
print '("2 Contact Slicing Time = ",f0.3," seconds.")',finish-start
start = finish

i = testSlicing(g,S)

if ( i .ne. 0 ) then
   print *,'problem', i
endif

open(unit=11,file='slice_2contacts.dat')

call SaveSlicesTxt(11,S)

close(11)

call cpu_time(finish)
print '("Saving File Time = ",f0.3," seconds.")',finish-start
start = finish

deallocate(g,S,E1,E2)

END PROGRAM MAIN
