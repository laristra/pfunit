!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: BaseTestRunner
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune,  NASA/GSFC 
!!
!! @date
!! 07 Nov 2013
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 07 Nov 2013 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------
module BaseTestRunner_mod
   use TestListener_mod
   implicit none
   private

   public :: BaseTestRunner

   type, abstract, extends(TestListener) :: BaseTestRunner
      private
      logical :: useDebug = .false.
   contains
      procedure(run2), deferred :: runRunner
      procedure :: setDebug
      procedure :: debug
   end type BaseTestRunner

   abstract interface

      ! Bizarre workaround for PGI 13.9.0  this unused method
      ! avoids "Incompatible PASS argument in run"
      subroutine foo(this, aTest, context)
         use Test_mod
         use ParallelContext_mod
         import BaseTestRunner
         
         class (BaseTestRunner), intent(inout) :: this
         class (Test), intent(inout) :: aTest
         class (ParallelContext), intent(in) :: context
      end subroutine foo
      

      function run2(this, aTest, context) result(result)
         use Test_mod
         use ParallelContext_mod
         use TestResult_mod
         import BaseTestRunner

         type (TestResult) :: result
         class (BaseTestRunner), intent(inout) :: this
         class (Test), intent(inout) :: aTest
         class (ParallelContext), intent(in) :: context
      end function run2
      
   end interface

contains

    subroutine setDebug(this)
       class (BaseTestRunner), intent(inout) :: this
       this%useDebug = .true.
    end subroutine setDebug


    logical function debug(this)
       class (BaseTestRunner), intent(inout) :: this
       debug = this%useDebug
    end function debug

end module BaseTestRunner_mod
