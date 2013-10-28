module BaseTestRunner_mod
   use TestListener_mod
   implicit none
   private

   public :: BaseTestRunner

   type, abstract, extends(TestListener) :: BaseTestRunner
   contains
      procedure(run2), deferred :: runRunner
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
      

      subroutine run2(this, aTest, context)
         use Test_mod
         use ParallelContext_mod
         import BaseTestRunner
         
         class (BaseTestRunner), intent(inout) :: this
         class (Test), intent(inout) :: aTest
         class (ParallelContext), intent(in) :: context
      end subroutine run2
      
   end interface

end module BaseTestRunner_mod
