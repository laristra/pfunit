module TestListener_mod
   implicit none
   private

   public :: TestListener
   public :: ListenerPointer

   type, abstract :: TestListener
      integer :: placeholder
   contains
     procedure(addFailure2), deferred :: addFailure
     procedure(startTest2), deferred :: startTest
     procedure(endTest2), deferred :: endTest
     procedure :: addError => addError2
   end type TestListener

   type ListenerPointer
     class (TestListener), pointer :: pListener
   end type ListenerPointer

   abstract interface
      subroutine addFailure2(this, testName, exceptions)
         use Exception_mod
         import TestListener
         class (TestListener), intent(inout) :: this
         character(len=*), intent(in) :: testName
         type (Exception), intent(in) :: exceptions(:)
      end subroutine addFailure2

      subroutine startTest2(this, testName)
         import TestListener
         class (TestListener), intent(inout) :: this
         character(len=*), intent(in) :: testName
      end subroutine startTest2
    
      subroutine endTest2(this, testName)
         import TestListener
         class (TestListener), intent(inout) :: this
         character(len=*), intent(in) :: testName
      end subroutine endTest2

   end interface


contains

   ! Most scenarios in Fortran cannot diagnose true errors, so
   ! an empty stub is provided here for convenience.
   subroutine addError2(this, testName, exceptions)
      use Exception_mod, only: Exception
      class (TestListener), intent(inout) :: this
      character(len=*), intent(in) :: testName
      type (Exception), intent(in) :: exceptions(:)
   end subroutine addError2

 end module TestListener_mod
