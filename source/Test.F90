module Test_mod
   implicit none
   private
   
   public :: Test

   ! Abstract class from which other Test classes inherit
   type, abstract :: Test
      integer :: placeholder
   contains
      procedure(countTestCases), deferred :: countTestCases
      procedure(run2), deferred :: run
   end type Test

   abstract interface

      integer function countTestCases(this)
         import Test
         class (Test), intent(in) :: this
      end function countTestCases

      recursive subroutine run2(this, tstResult, context)
         use TestResult_mod
         use ParallelContext_mod
         import Test
         class (Test), intent(inout) :: this
         class (TestResult), intent(inout) :: tstResult
         class (ParallelContext), intent(in) :: context
       end subroutine run2

   end interface

end module Test_mod
